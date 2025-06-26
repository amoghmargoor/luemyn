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
package com.cobolrag;

import com.cobolrag.config.CobolRagConfig;
import com.cobolrag.parser.CobolParser;
import com.cobolrag.graph.Neo4jDependencyGraphBuilder;
import com.cobolrag.embedding.ChromaEmbeddingService;
import com.cobolrag.rag.RagService;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

public class CobolRagApplication {
    private static final Logger logger = LoggerFactory.getLogger(CobolRagApplication.class);

    private final CobolParser cobolParser;
    private final Neo4jDependencyGraphBuilder graphBuilder;
    private final ChromaEmbeddingService embeddingService;
    private final RagService ragService;
    private final CobolRagConfig cobolRagConfig;

    public CobolRagApplication(String neo4jUri, String neo4jUser, String neo4jPassword,
                               String chromaDbUrl, String pythonExecutablePath, String configFilePath) {
        this.cobolParser = new CobolParser();
        this.cobolRagConfig = new CobolRagConfig(configFilePath);
        this.graphBuilder = new Neo4jDependencyGraphBuilder(neo4jUri, neo4jUser, neo4jPassword,
                CobolPreprocessor.CobolSourceFormatEnum.valueOf(this.cobolRagConfig.getFormat()));
        this.embeddingService = new ChromaEmbeddingService(chromaDbUrl, System.getenv("OPENAI_API_KEY"));
        this.ragService = new RagService(graphBuilder, embeddingService);
    }

    public void processCobolCodebase(String directoryPath) {
        logger.info("Processing COBOL codebase from directory: {}", directoryPath);

        // Step 1: Parse all COBOL files in the directory
        List<File> cobolFiles = CobolParser.findCobolFiles(new File(directoryPath));
        logger.info("Found {} COBOL files to process", cobolFiles.size());

        // Step 2: Parse each file and build the dependency graph
        for (File file : cobolFiles) {
            try {
                logger.info("Parsing file: {}", file.getName());
                Program program = cobolParser.parseFile(file,
                        CobolPreprocessor.CobolSourceFormatEnum.valueOf(cobolRagConfig.getFormat()));

                // Step 3: Extract dependencies and add to Neo4j
                graphBuilder.addProgramToGraph(program, file.getPath());

                // Step 4: Create embeddings for the file content
                createEmbeddingsForProgram(file, program.getCompilationUnits().get(0).getName());

            } catch (Exception e) {
                logger.error("Error processing file {}: {}", file.getName(), e.getMessage(), e);
            }
        }

        // Step 5: Build relationships between programs in Neo4j
        graphBuilder.buildRelationships();

        logger.info("Completed processing COBOL codebase");
    }

    private void createEmbeddingsForProgram(File file, String programName) throws IOException {
        // Extract code text
        String codeContent = Files.readString(file.toPath(), StandardCharsets.UTF_8);

        // Prepare metadata
        Map<String, Object> metadata = Map.of(
                "program", programName,
                "file_path", file.getAbsolutePath(),
                "file_name", file.getName(),
                "language", "COBOL"
        );

        // Generate unique ID
        String docId = programName + "_" + file.getName();

        // Add to ChromaDB
        embeddingService.addEmbeddings(
                "cobol-code",
                List.of(codeContent),
                List.of(metadata),
                List.of(docId)
        );
    }

    public String queryRag(String query) {
        logger.info("Processing RAG query: {}", query);
        return ragService.processQuery(query);
    }

    public void shutdown() {
        graphBuilder.close();
        embeddingService.close();
    }

    public static void main(String[] args) {
        if (args.length < 6) {
            System.out.println("Usage: CobolRagApplication <neo4jUri> <neo4jUser> <neo4jPassword> <chromaDbUrl> <cobolDirectoryPath> <configFilePath>");
            System.out.println("Note: OPENAI_API_KEY environment variable must be set");
            System.exit(1);
        }

        String neo4jUri = args[0];
        String neo4jUser = args[1];
        String neo4jPassword = args[2];
        String chromaDbUrl = args[3];
        String cobolDir = args[4];
        String configFilePath = args[5];

        // Check for OpenAI API key
        if (System.getenv("OPENAI_API_KEY") == null) {
            System.err.println("ERROR: OPENAI_API_KEY environment variable not set");
            System.exit(1);
        }

        CobolRagApplication app = new CobolRagApplication(neo4jUri, neo4jUser, neo4jPassword, chromaDbUrl, null, configFilePath);

        try {
            app.processCobolCodebase(cobolDir);

            // Example query
            String result = app.queryRag("How does the customer data processing work?");
            System.out.println("Query result: " + result);
        } finally {
            app.shutdown();
        }
    }
}
