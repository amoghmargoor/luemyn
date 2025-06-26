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

import com.cobolrag.embedding.ChromaEmbeddingService;
import com.cobolrag.graph.Neo4jDependencyGraphBuilder;
import com.cobolrag.parser.CobolParser;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@Testcontainers
class RagServiceIntegrationTest {

    @Container
    private static final Neo4jContainer<?> neo4jContainer = 
            new Neo4jContainer<>("neo4j:4.4").withAdminPassword("testpassword");

    @TempDir
    Path tempDir;

    private RagService ragService;
    private ChromaEmbeddingService embeddingService;
    private Neo4jDependencyGraphBuilder graphBuilder;
    private CobolParser parser;

    @BeforeEach
    void setUp() {
        // Initialize services with mock embedding service to avoid API rate limiting
        embeddingService = new MockChromaEmbeddingService();
        
        graphBuilder = new Neo4jDependencyGraphBuilder(
                neo4jContainer.getBoltUrl(),
                "neo4j",
                "testpassword",
                CobolPreprocessor.CobolSourceFormatEnum.FIXED
        );
        
        parser = new CobolParser();
        ragService = new RagService(graphBuilder, embeddingService);
    }

    @Test
    void shouldProcessQueryWithEmbeddedCode() throws IOException {
        // Create test COBOL file
        File testFile = createCobolFile("customer.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. CUSTOMER.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 CUSTOMER-RECORD.\n" +
            "000600     05 CUST-ID PIC 9(5).\n" +
            "000700     05 CUST-NAME PIC X(30).\n" +
            "000800     05 CUST-BALANCE PIC 9(7)V99.\n" +
            "000900 PROCEDURE DIVISION.\n" +
            "001000     DISPLAY 'Customer processing'.\n" +
            "001100     STOP RUN.");

        // Parse and add to graph
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        graphBuilder.addProgramToGraph(program, testFile.getAbsolutePath());

        // Add embeddings using mock service
        String codeContent = Files.readString(testFile.toPath());
        Map<String, Object> metadata = Map.of(
                "program", "CUSTOMER",
                "file_path", testFile.getAbsolutePath(),
                "file_name", testFile.getName(),
                "language", "COBOL"
        );

        embeddingService.addEmbeddings(
                "cobol-code",
                List.of(codeContent),
                List.of(metadata),
                List.of("CUSTOMER_" + testFile.getName())
        );

        // Test query
        String query = "How does the customer data processing work?";
        String result = ragService.processQuery(query);

        assertNotNull(result);
        assertFalse(result.isEmpty());
    }

    @Test
    void shouldHandleQueryWithDependencies() throws IOException {
        // Create multiple COBOL files with dependencies
        File libFile = createCobolFile("library.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. LIBRARY.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-LIB-VAR PIC X(10).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     DISPLAY 'Library function'.\n" +
            "000800     STOP RUN.");

        File appFile = createCobolFile("application.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. APPLICATION.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-APP-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     CALL 'LIBRARY'.\n" +
            "000800     DISPLAY 'Application logic'.\n" +
            "000900     STOP RUN.");

        // Parse and add to graph
        Program libProgram = parser.parseFile(libFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        Program appProgram = parser.parseFile(appFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        
        graphBuilder.addProgramToGraph(libProgram, libFile.getAbsolutePath());
        graphBuilder.addProgramToGraph(appProgram, appFile.getAbsolutePath());
        graphBuilder.buildRelationships();

        // Add embeddings for both files using mock service
        String libContent = Files.readString(libFile.toPath());
        String appContent = Files.readString(appFile.toPath());
        
        Map<String, Object> libMetadata = Map.of(
                "program", "LIBRARY",
                "file_path", libFile.getAbsolutePath(),
                "file_name", libFile.getName(),
                "language", "COBOL"
        );
        
        Map<String, Object> appMetadata = Map.of(
                "program", "APPLICATION",
                "file_path", appFile.getAbsolutePath(),
                "file_name", appFile.getName(),
                "language", "COBOL"
        );

        embeddingService.addEmbeddings(
                "cobol-code",
                List.of(libContent, appContent),
                List.of(libMetadata, appMetadata),
                List.of("LIBRARY_" + libFile.getName(), "APPLICATION_" + appFile.getName())
        );

        // Test query about dependencies
        String query = "What are the dependencies between programs?";
        String result = ragService.processQuery(query);

        assertNotNull(result);
        assertFalse(result.isEmpty());
    }

    @Test
    void shouldHandleEmptyQuery() {
        String query = "";
        String result = ragService.processQuery(query);

        assertNotNull(result);
        // Should handle empty query gracefully - just check it's not null
        assertTrue(result != null);
    }

    @Test
    void shouldHandleNullQuery() {
        String result = ragService.processQuery(null);

        assertNotNull(result);
        // Should handle null query gracefully - just check it's not null
        assertTrue(result != null);
    }

    @Test
    void shouldHandleQueryWithNoRelevantCode() throws IOException {
        // Create a simple COBOL file
        File testFile = createCobolFile("simple.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. SIMPLE.\n" +
            "000300 PROCEDURE DIVISION.\n" +
            "000400     DISPLAY 'Hello'.\n" +
            "000500     STOP RUN.");

        // Add to graph and embeddings
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        graphBuilder.addProgramToGraph(program, testFile.getAbsolutePath());

        String codeContent = Files.readString(testFile.toPath());
        Map<String, Object> metadata = Map.of(
                "program", "SIMPLE",
                "file_path", testFile.getAbsolutePath(),
                "file_name", testFile.getName(),
                "language", "COBOL"
        );

        embeddingService.addEmbeddings(
                "cobol-code",
                List.of(codeContent),
                List.of(metadata),
                List.of("SIMPLE_" + testFile.getName())
        );

        // Query about something not in the code
        String query = "How does the database connection work?";
        String result = ragService.processQuery(query);

        assertNotNull(result);
        // Should return a response even if no relevant code is found
        assertFalse(result.isEmpty());
    }

    @Test
    void shouldHandleComplexQuery() throws IOException {
        // Create a more complex COBOL file
        File testFile = createCobolFile("complex.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. COMPLEX.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 CUSTOMER-RECORD.\n" +
            "000600     05 CUST-ID PIC 9(5).\n" +
            "000700     05 CUST-NAME PIC X(30).\n" +
            "000800     05 CUST-ADDRESS.\n" +
            "000900         10 STREET PIC X(40).\n" +
            "001000         10 CITY PIC X(20).\n" +
            "001100         10 STATE PIC X(2).\n" +
            "001200         10 ZIP PIC 9(5).\n" +
            "001300 01 WS-COUNTER PIC 9(3) VALUE 0.\n" +
            "001400 PROCEDURE DIVISION.\n" +
            "001500 MAIN-LOGIC.\n" +
            "001600     PERFORM PROCESS-CUSTOMER.\n" +
            "001700     STOP RUN.\n" +
            "001800 PROCESS-CUSTOMER.\n" +
            "001900     DISPLAY 'Processing customer data'.\n" +
            "002000     EXIT.");

        // Parse and add to graph
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        graphBuilder.addProgramToGraph(program, testFile.getAbsolutePath());

        // Add embeddings using mock service
        String codeContent = Files.readString(testFile.toPath());
        Map<String, Object> metadata = Map.of(
                "program", "COMPLEX",
                "file_path", testFile.getAbsolutePath(),
                "file_name", testFile.getName(),
                "language", "COBOL"
        );

        embeddingService.addEmbeddings(
                "cobol-code",
                List.of(codeContent),
                List.of(metadata),
                List.of("COMPLEX_" + testFile.getName())
        );

        // Complex query
        String query = "Explain the customer data structure and processing logic";
        String result = ragService.processQuery(query);

        assertNotNull(result);
        assertFalse(result.isEmpty());
    }

    @Test
    void shouldHandleMultipleQueries() throws IOException {
        // Create test file
        File testFile = createCobolFile("multi.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. MULTI.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     DISPLAY 'Multiple queries test'.\n" +
            "000800     STOP RUN.");

        // Setup
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        graphBuilder.addProgramToGraph(program, testFile.getAbsolutePath());

        String codeContent = Files.readString(testFile.toPath());
        Map<String, Object> metadata = Map.of(
                "program", "MULTI",
                "file_path", testFile.getAbsolutePath(),
                "file_name", testFile.getName(),
                "language", "COBOL"
        );

        embeddingService.addEmbeddings(
                "cobol-code",
                List.of(codeContent),
                List.of(metadata),
                List.of("MULTI_" + testFile.getName())
        );

        // Multiple queries
        String[] queries = {
            "What does this program do?",
            "How is the data structured?",
            "What are the main procedures?"
        };

        for (String query : queries) {
            String result = ragService.processQuery(query);
            assertNotNull(result);
            assertFalse(result.isEmpty());
        }
    }

    private File createCobolFile(String fileName, String content) throws IOException {
        File file = tempDir.resolve(fileName).toFile();
        Files.write(file.toPath(), content.getBytes());
        return file;
    }

    // Mock embedding service to avoid OpenAI API rate limiting
    private static class MockChromaEmbeddingService extends ChromaEmbeddingService {
        public MockChromaEmbeddingService() {
            super("http://localhost:8000", "mock-key");
        }

        @Override
        public void addEmbeddings(String collectionName, List<String> documents, 
                                 List<Map<String, Object>> metadatas, List<String> ids) {
            // Mock implementation - do nothing to avoid API calls
        }

        @Override
        public List<Map<String, Object>> queryEmbeddings(String collectionName, String queryText, int nResults) {
            // Return mock results
            return List.of(Map.of(
                "id", "mock-id",
                "document", "Mock COBOL code content",
                "metadata", Map.of("program", "MOCK", "language", "COBOL")
            ));
        }
    }
} 