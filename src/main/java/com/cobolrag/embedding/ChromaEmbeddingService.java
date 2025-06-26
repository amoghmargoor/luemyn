package com.cobolrag.embedding;

import com.fasterxml.jackson.databind.ObjectMapper;
import okhttp3.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.TimeUnit;

public class ChromaEmbeddingService implements AutoCloseable {
    private static final Logger logger = LoggerFactory.getLogger(ChromaEmbeddingService.class);
    private static final MediaType JSON = MediaType.get("application/json; charset=utf-8");

    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final String chromaDbUrl;
    private final String openAiApiKey;
    private final OpenAIEmbeddingGenerator embeddingGenerator;

    public ChromaEmbeddingService(String chromaDbUrl, String openAiApiKey) {
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build();

        this.objectMapper = new ObjectMapper();
        this.chromaDbUrl = chromaDbUrl;
        this.openAiApiKey = openAiApiKey;
        this.embeddingGenerator = new OpenAIEmbeddingGenerator(openAiApiKey);
    }

    public void createCollection(String collectionName) {
        try {
            Map<String, Object> payload = Map.of(
                    "name", collectionName,
                    "metadata", Map.of("hnsw:space", "cosine")
            );

            Request request = new Request.Builder()
                    .url(chromaDbUrl + "/api/v1/collections")
                    .post(RequestBody.create(objectMapper.writeValueAsString(payload), JSON))
                    .build();

            executeRequest(request);
        } catch (IOException e) {
            throw new EmbeddingServiceException("Failed to create collection", e);
        }
    }

    public void addEmbeddings(String collectionName,
                              List<String> documents,
                              List<Map<String, Object>> metadatas,
                              List<String> ids) {
        try {
            List<List<Double>> embeddings = embeddingGenerator.generateEmbeddings(documents);

            Map<String, Object> payload = Map.of(
                    "documents", documents,
                    "embeddings", embeddings,
                    "metadatas", metadatas,
                    "ids", ids
            );

            Request request = new Request.Builder()
                    .url(chromaDbUrl + "/api/v1/collections/" + collectionName + "/add")
                    .post(RequestBody.create(objectMapper.writeValueAsString(payload), JSON))
                    .build();

            executeRequest(request);
        } catch (IOException e) {
            throw new EmbeddingServiceException("Failed to add embeddings", e);
        }
    }

    public List<Map<String, Object>> queryEmbeddings(String collectionName,
                                                     String query,
                                                     int maxResults) {
        try {
            List<Double> queryEmbedding = embeddingGenerator.generateEmbedding(query);

            Map<String, Object> payload = Map.of(
                    "query_embeddings", List.of(queryEmbedding),
                    "n_results", maxResults
            );

            Request request = new Request.Builder()
                    .url(chromaDbUrl + "/api/v1/collections/" + collectionName + "/query")
                    .post(RequestBody.create(objectMapper.writeValueAsString(payload), JSON))
                    .build();

            Response response = httpClient.newCall(request).execute();
            return parseResponse(response, QueryResult.class).getResults();
        } catch (IOException e) {
            throw new EmbeddingServiceException("Failed to query embeddings", e);
        }
    }

    public void deleteCollection(String collectionName) {
        Request request = new Request.Builder()
                .url(chromaDbUrl + "/api/v1/collections/" + collectionName)
                .delete()
                .build();

        executeRequest(request);
    }

    private void executeRequest(Request request) {
        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new EmbeddingServiceException("HTTP error: " + response.code()
                        + " - " + response.body().string());
            }
        } catch (IOException e) {
            throw new EmbeddingServiceException("Request failed", e);
        }
    }

    private <T> T parseResponse(Response response, Class<T> type) throws IOException {
        return objectMapper.readValue(
                Objects.requireNonNull(response.body()).byteStream(),
                objectMapper.getTypeFactory().constructType(type)
        );
    }

    @Override
    public void close() {
        httpClient.dispatcher().executorService().shutdown();
        httpClient.connectionPool().evictAll();
    }

    private static class OpenAIEmbeddingGenerator {
        private static final String EMBEDDING_URL = "https://api.openai.com/v1/embeddings";
        private final OkHttpClient client = new OkHttpClient();
        private final String apiKey;

        OpenAIEmbeddingGenerator(String apiKey) {
            this.apiKey = apiKey;
        }

        List<Double> generateEmbedding(String text) throws IOException {
            return generateEmbeddings(List.of(text)).get(0);
        }

        List<List<Double>> generateEmbeddings(List<String> texts) throws IOException {
            Map<String, Object> payload = Map.of(
                    "input", texts,
                    "model", "text-embedding-3-small"
            );

            Request request = new Request.Builder()
                    .url(EMBEDDING_URL)
                    .header("Authorization", "Bearer " + apiKey)
                    .post(RequestBody.create(
                            new ObjectMapper().writeValueAsString(payload),
                            JSON
                    ))
                    .build();

            try (Response response = client.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new IOException("OpenAI API error: " + response.code());
                }

                EmbeddingResponse embeddingResponse = new ObjectMapper()
                        .readValue(response.body().byteStream(), EmbeddingResponse.class);

                return embeddingResponse.getVectors();
            }
        }
    }

    // Response DTO classes
    private static class QueryResult {
        private List<Map<String, Object>> results;
        public List<Map<String, Object>> getResults() { return results; }
    }

    private static class EmbeddingResponse {
        private List<EmbeddingData> data;
        List<List<Double>> getVectors() {
            return data.stream().map(d -> d.embedding).toList();
        }
    }

    private static class EmbeddingData {
        List<Double> embedding;
    }

    public static class EmbeddingServiceException extends RuntimeException {
        public EmbeddingServiceException(String message, Throwable cause) {
            super(message, cause);
        }
        public EmbeddingServiceException(String message) {
            super(message);
        }
    }
}
