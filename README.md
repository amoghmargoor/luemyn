# COBOL RAG (Retrieval-Augmented Generation) Library

A Java library for parsing COBOL codebases, extracting program structure and dependencies, embedding code for semantic search, and providing AI-powered explanations using Retrieval-Augmented Generation (RAG).

---

## Features
- **COBOL Parsing:** Extracts program structure, entities, and dependencies from COBOL source files.
- **Dependency Graph:** Builds a Neo4j graph of program calls and relationships.
- **Embeddings:** Uses ChromaDB to store and search code embeddings for semantic retrieval.
- **RAG Service:** Answers questions about the COBOL codebase using OpenAI models (e.g., GPT-4o).

---

## Prerequisites
- Java 21+
- Maven 3.8+
- Docker (for running Neo4j and ChromaDB via TestContainers)
- OpenAI API key (for RAG service)

---

## Compilation & Installation

1. **Clone the repository:**
   ```bash
   git clone <your-repo-url>
   cd luemyn
   ```

2. **Build the project:**
   ```bash
   mvn clean install
   ```

---

## Running Tests

1. **Set your OpenAI API key:**
   ```bash
   export OPENAI_API_KEY=sk-...your-key...
   ```

2. **Run all tests:**
   ```bash
   mvn test
   ```

---

## Usage Example

You can use the main components in your Java code as follows:

```java
import com.cobolrag.parser.CobolParser;
import com.cobolrag.graph.Neo4jDependencyGraphBuilder;
import com.cobolrag.embedding.ChromaEmbeddingService;
import com.cobolrag.rag.RagService;

// Initialize services (see config for details)
CobolParser parser = new CobolParser();
Neo4jDependencyGraphBuilder graphBuilder = new Neo4jDependencyGraphBuilder("bolt://localhost:7687", "neo4j", "password");
ChromaEmbeddingService embeddingService = new ChromaEmbeddingService("http://localhost:8000");
RagService ragService = new RagService(graphBuilder, embeddingService, System.getenv("OPENAI_API_KEY"));

// Parse a COBOL file
Program program = parser.parseFile(new File("MYPROG.cbl"), CobolPreprocessor.CobolSourceFormatEnum.FIXED);

// Add to graph
graphBuilder.addProgramToGraph(program, "MYPROG.cbl");

// Query the RAG service
String answer = ragService.processQuery("What does MYPROG do?");
System.out.println(answer);
```

---

## Documentation

- **Main Application:** [`CobolRagApplication.java`](src/main/java/com/cobolrag/CobolRagApplication.java)
- **Configuration:** [`CobolRagConfig.java`](src/main/java/com/cobolrag/config/CobolRagConfig.java)
- **COBOL Parser:** [`CobolParser.java`](src/main/java/com/cobolrag/parser/CobolParser.java)
- **Dependency Graph:** [`Neo4jDependencyGraphBuilder.java`](src/main/java/com/cobolrag/graph/Neo4jDependencyGraphBuilder.java)
- **Embeddings:** [`ChromaEmbeddingService.java`](src/main/java/com/cobolrag/embedding/ChromaEmbeddingService.java)
- **RAG Service:** [`RagService.java`](src/main/java/com/cobolrag/rag/RagService.java)

---

## Further Reading
- [OpenAI API Documentation](https://platform.openai.com/docs/api-reference/introduction)
- [Neo4j Documentation](https://neo4j.com/docs/)
- [ChromaDB Documentation](https://docs.trychroma.com/)
- [LangChain4j Documentation](https://github.com/langchain4j/langchain4j)

---

## License

This project is licensed under the Apache License, Version 2.0. See the [LICENSE](LICENSE) file for details. 