# COBOL RAG (Retrieval-Augmented Generation) Library

A Java library for parsing COBOL codebases, extracting program structure and dependencies, embedding code for semantic search, and providing AI-powered explanations using Retrieval-Augmented Generation (RAG).

---

## Features
- **COBOL Parsing:** Extracts program structure, entities, and dependencies from COBOL source files.
- **Dependency Graph:** Builds a Neo4j graph of program calls and relationships.
- **Embeddings:** Uses ChromaDB to store and search code embeddings for semantic retrieval.
- **RAG Service:** Answers questions about the COBOL codebase using OpenAI models (e.g., GPT-4o).
- **COBOL-to-Java Transformation:** Converts COBOL programs to equivalent Java code.

---

## Prerequisites
- Java 21+
- Maven 3.8+
- Docker (for running Neo4j and ChromaDB via TestContainers)
- OpenAI API key (for RAG service)

---

## Compilation & Installation

### 1. Install prolog-cobol-parser dependency

The project requires `prolog-cobol-parser-4.0.0` which is not available in the Maven Central repository. You need to install it locally first:

```bash
# Clone the prolog-cobol-parser repository
git clone https://github.com/uwol/proleap-cobol-parser.git
cd proleap-cobol-parser

# Install version 4.0.0 to your local Maven repository
mvn clean install -Dversion=4.0.0
```

### 2. Build the project

```bash
# Clone this repository
git clone <your-repo-url>
cd luemyn

# Build the project
mvn clean install
```

---

## COBOL-to-Java Transformation

The `proleap-cobol-transform` module provides functionality to convert COBOL programs to equivalent Java code. This transformation preserves the logical structure and flow of the original COBOL program while generating readable Java code.

### Features of the Transformation Module:

- **Program Structure Conversion:** Converts COBOL identification, data, and procedure divisions to Java classes and methods
- **Data Type Mapping:** Maps COBOL data types (PIC clauses) to appropriate Java types
- **Control Flow Translation:** Converts COBOL control structures (IF, PERFORM, CALL) to Java equivalents
- **Variable Declaration:** Transforms COBOL working storage to Java field declarations
- **Procedure Division:** Converts COBOL procedures to Java methods

### Usage Example:

```java
import io.proleap.cobol.transform.CobolTransformRule;
import io.proleap.cobol.transform.runner.CobolTransformRunner;

// Create a transformation runner
CobolTransformRunner runner = new CobolTransformRunner();

// Transform a COBOL file to Java
String javaCode = runner.transform(cobolFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

// The generated Java code will include:
// - A main class with the program name
// - Field declarations for working storage variables
// - Methods for each paragraph/section
// - Proper Java syntax and structure
```

### Example COBOL Input:
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20).
       PROCEDURE DIVISION.
           MOVE "World" TO WS-NAME.
           DISPLAY "Hello " WS-NAME.
           STOP RUN.
```

### Generated Java Output:
```java
public class HELLO {
    private String wsName = "";
    
    public void main() {
        wsName = "World";
        System.out.println("Hello " + wsName);
        System.exit(0);
    }
}
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