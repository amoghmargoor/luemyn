/*
 * Copyright 2024 Amogh Margoor
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.cobolrag.rag;

import com.cobolrag.graph.Neo4jDependencyGraphBuilder;
import com.cobolrag.embedding.ChromaEmbeddingService;
import dev.langchain4j.chain.ConversationalRetrievalChain;
import dev.langchain4j.data.document.Metadata;
import dev.langchain4j.data.segment.TextSegment;
import dev.langchain4j.model.chat.ChatLanguageModel;
import dev.langchain4j.model.input.PromptTemplate;
import dev.langchain4j.model.openai.OpenAiChatModel;
import dev.langchain4j.retriever.Retriever;
import org.neo4j.driver.Result;
import org.neo4j.driver.Record;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static dev.langchain4j.internal.Utils.getOrDefault;

public class RagService {
    private static final Logger logger = LoggerFactory.getLogger(RagService.class);

    private static final String PROMPT_TEMPLATE = """
        You are a COBOL expert assistant. Answer the question based on the context below.
        Include relevant code snippets and explain their purpose.
        
        Context from codebase:
        {context}
        
        Dependency graph relations:
        {dependencies}
        
        Question: {question}
        
        Answer in markdown format:""";

    private final Neo4jDependencyGraphBuilder graphBuilder;
    private final ChromaEmbeddingService embeddingService;
    private final ChatLanguageModel chatModel;
    private final ConversationalRetrievalChain retrievalChain;

    public RagService(Neo4jDependencyGraphBuilder graphBuilder,
                      ChromaEmbeddingService embeddingService) {
        this(graphBuilder, embeddingService, getApiKeyFromEnvironment());
    }

    public RagService(Neo4jDependencyGraphBuilder graphBuilder,
                      ChromaEmbeddingService embeddingService,
                      String openAiApiKey) {
        validateApiKey(openAiApiKey);
        this.graphBuilder = graphBuilder;
        this.embeddingService = embeddingService;

        this.chatModel = OpenAiChatModel.builder()
                .apiKey(openAiApiKey)
                .modelName("gpt-4o")
                .temperature(0.1)
                .timeout(Duration.ofSeconds(60))
                .maxRetries(3)
                .build();

        this.retrievalChain = ConversationalRetrievalChain.builder()
                .chatLanguageModel(chatModel)
                .retriever(buildEnhancedRetriever(graphBuilder, embeddingService))
                .promptTemplate(PromptTemplate.from(PROMPT_TEMPLATE))
                .build();
    }

    private static String getApiKeyFromEnvironment() {
        String apiKey = System.getenv("OPENAI_API_KEY");
        if (apiKey == null || apiKey.isBlank()) {
            throw new RagInitializationException(
                    "OPENAI_API_KEY environment variable not set. " +
                            "Please configure your OpenAI API key."
            );
        }
        return apiKey;
    }

    private void validateApiKey(String apiKey) {
        if (apiKey == null || apiKey.length() < 20) { // Basic format check
            throw new RagInitializationException("Invalid OpenAI API key format");
        }
    }

    public static class RagInitializationException extends RuntimeException {
        public RagInitializationException(String message) {
            super(message);
        }
    }

    private Retriever<TextSegment> buildEnhancedRetriever(Neo4jDependencyGraphBuilder graphBuilder,
                                                          ChromaEmbeddingService embeddingService) {
        return query -> {
            // 1. Get relevant code segments
            List<Map<String, Object>> embeddings =
                    embeddingService.queryEmbeddings("cobol-code", query, 5);

            // 2. Enrich with dependency context
            return embeddings.stream()
                    .map(doc -> createEnhancedTextSegment(doc, graphBuilder))
                    .collect(Collectors.toList());
        };
    }

    private TextSegment createEnhancedTextSegment(Map<String, Object> doc,
                                                  Neo4jDependencyGraphBuilder graphBuilder) {
        String code = doc.get("document") != null ? doc.get("document").toString() : "";
        String source = doc.get("source") != null ? doc.get("source").toString() : "unknown";
        String program = doc.get("program") != null ? doc.get("program").toString() : "";

        // Get dependencies for this program
        String dependencies = getProgramDependencies(program, graphBuilder);

        // Combine code and dependencies into context
        String enrichedText = String.format(
                "Code from %s:\n%s\nDependencies:\n%s",
                source, code, dependencies
        );

        return TextSegment.from(enrichedText);
    }

    public String processQuery(String query) {
        try {
            // Handle empty or null queries
            if (query == null || query.trim().isEmpty()) {
                return "Please provide a valid query about the COBOL codebase.";
            }
            
            // Step 4: Generate response with LangChain
            return retrievalChain.execute(query);
        } catch (Exception e) {
            logger.error("RAG query failed", e);
            
            // Handle specific API errors
            if (e.getMessage() != null && e.getMessage().contains("insufficient_quota")) {
                return "Sorry, the AI service is currently unavailable due to quota limits. Please try again later.";
            }
            
            return "Sorry, I couldn't process that request. Error: " + e.getMessage();
        }
    }

    private Retriever<TextSegment> buildRetriever() {
        return query -> embeddingService.queryEmbeddings("cobol-code", query, 5)
                .stream()
                .map(doc -> TextSegment.from(
                        formatDocumentText(doc),
                        getOrDefault((Metadata) doc.get("metadata"), new Metadata())
                ))
                .collect(Collectors.toList());
    }

    private String formatDocumentText(Map<String, Object> doc) {
        return String.format("""
            **Source**: %s
            **Type**: %s
            **Code**:
            ```
            %s
            ```""",
                doc.get("source"),
                doc.get("type"),
                doc.get("document")
        );
    }

    private String getDependencyContext(List<Map<String, Object>> relevantDocs) {
        return relevantDocs.stream()
                .filter(doc -> doc.containsKey("program"))
                .map(doc -> (String) doc.get("program"))
                .distinct()
                .map(program -> getProgramDependencies(program, graphBuilder))
                .collect(Collectors.joining("\n"));
    }

    private String getProgramDependencies(String programName, Neo4jDependencyGraphBuilder graphBuilder) {
        try {
            if (programName == null || programName.isEmpty()) {
                return "No program name provided";
            }
            List<Record> records = graphBuilder.executeQuery(
                    "MATCH (p:Program {name: $name})-[:CALLS]->(called) " +
                            "RETURN collect(called.name) AS deps",
                    Map.of("name", programName)
            );
            if (!records.isEmpty()) {
                Record record = records.get(0);
                Object deps = record.get("deps");
                if (deps != null) {
                    return deps.toString();
                }
            }
            return "No dependencies found";
        } catch (Exception e) {
            logger.warn("Failed to retrieve dependencies for {}", programName, e);
            return "Dependency retrieval error: " + e.getMessage();
        }
    }
}
