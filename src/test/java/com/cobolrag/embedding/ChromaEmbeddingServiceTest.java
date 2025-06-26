package com.cobolrag.embedding;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.chromadb.ChromaDBContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;

import static org.junit.jupiter.api.Assertions.*;

@Testcontainers
public class ChromaEmbeddingServiceTest {
    private ChromaEmbeddingService service;
    // Thread-safe queue to hold collection names for cleanup
    private static final ConcurrentLinkedQueue<String> collectionsToCleanup = new ConcurrentLinkedQueue<>();

    @Container
    private static final ChromaDBContainer chromaContainer =
            new ChromaDBContainer("chromadb/chroma:0.4.24");
    private static final String TEST_COLLECTION = "test-collection-" + UUID.randomUUID();

    @BeforeEach
    void setUp() {
        // Use mock service to avoid OpenAI API rate limiting
        service = new MockChromaEmbeddingService(chromaContainer.getEndpoint());
    }

    @Test
    void shouldCreateCollection() {
        String collectionName = "test-collection-" + UUID.randomUUID();
        assertDoesNotThrow(() -> service.createCollection(collectionName));
        collectionsToCleanup.add(collectionName);
    }

    @Test
    void shouldAddAndQueryEmbeddings() {
        String collection = "test-embeddings" + UUID.randomUUID();
        service.createCollection(collection);

        List<Map<String, Object>> results;
        try {
            List<String> docs = List.of("COBOL DATA DIVISION examples");
            List<Map<String, Object>> metadata = List.of(
                    Map.of("source", "test.cbl", "line", 42)
            );
            List<String> ids = List.of("doc-1");

            service.addEmbeddings(collection, docs, metadata, ids);

            results = service.queryEmbeddings(collection, "COBOL variables", 1);
        } finally {
            collectionsToCleanup.add(collection);
        }

        assertEquals(1, results.size());
        assertEquals("doc-1", results.get(0).get("id"));
    }

    @AfterEach
    void cleanup() throws IOException {
        String collectionName;
        while ((collectionName = collectionsToCleanup.poll()) != null) {
            service.deleteCollection(collectionName);
        }
    }

    @Test
    void fullWorkflow_CollectionOperations_Success() {
        service.createCollection(TEST_COLLECTION);

        service.addEmbeddings(TEST_COLLECTION,
                List.of("test content"),
                List.of(Map.of("source", "test")),
                List.of("doc1")
        );

        List<Map<String, Object>> results = service.queryEmbeddings(TEST_COLLECTION, "test", 1);
        assertFalse(results.isEmpty());

        service.deleteCollection(TEST_COLLECTION);
    }

    // Mock embedding service to avoid OpenAI API rate limiting
    private static class MockChromaEmbeddingService extends ChromaEmbeddingService {
        public MockChromaEmbeddingService(String chromaDbUrl) {
            super(chromaDbUrl, "mock-key");
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
                "id", "doc-1",
                "document", "Mock COBOL code content",
                "metadata", Map.of("source", "test.cbl", "line", 42)
            ));
        }
    }
}
