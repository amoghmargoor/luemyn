package com.cobolrag.graph;

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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@Testcontainers
class Neo4jDependencyGraphBuilderTest {

    @Container
    private static final Neo4jContainer<?> neo4jContainer = new Neo4jContainer<>("neo4j:4.4")
            .withAdminPassword("testpassword");

    @TempDir
    Path tempDir;

    private Neo4jDependencyGraphBuilder graphBuilder;
    private File testCobolFile;

    @BeforeEach
    void setUp() throws IOException {
        // Create test COBOL file
        testCobolFile = tempDir.resolve("test.cbl").toFile();
        String cobolContent = 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. TESTPROG.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-COUNTER PIC 9(3).\n" +
            "000600 01 WS-NAME PIC X(20).\n" +
            "000700 PROCEDURE DIVISION.\n" +
            "000800     CALL 'SUBPROG' USING WS-COUNTER.\n" +
            "000900     DISPLAY WS-NAME.\n" +
            "001000     STOP RUN.";
        Files.write(testCobolFile.toPath(), cobolContent.getBytes());

        // Initialize graph builder
        graphBuilder = new Neo4jDependencyGraphBuilder(
                neo4jContainer.getBoltUrl(),
                "neo4j",
                "testpassword",
                CobolPreprocessor.CobolSourceFormatEnum.FIXED
        );
    }

    @BeforeEach
    void clearDatabase() {
        // Remove all nodes and relationships
        graphBuilder.executeQuery("MATCH (n) DETACH DELETE n", Map.of());
    }

    @Test
    void shouldInitializeDatabaseSuccessfully() {
        assertDoesNotThrow(() -> {
            // The database should be initialized in the constructor
            assertNotNull(graphBuilder);
        });
    }

    @Test
    void shouldAddProgramToGraph() throws IOException {
        CobolParser parser = new CobolParser();
        Program program = parser.parseFile(testCobolFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        assertDoesNotThrow(() -> {
            graphBuilder.addProgramToGraph(program, testCobolFile.getAbsolutePath());
        });

        // Verify program was added
        var records = graphBuilder.executeQuery(
                "MATCH (p:Program {name: 'TESTPROG'}) RETURN p.name AS name, p.filePath AS filePath",
                Map.of()
        );

        assertFalse(records.isEmpty());
        var record = records.get(0);
        assertEquals("TESTPROG", record.get("name").asString());
        assertEquals(testCobolFile.getAbsolutePath(), record.get("filePath").asString());
    }

    @Test
    void shouldAddEntitiesToGraph() throws IOException {
        CobolParser parser = new CobolParser();
        Program program = parser.parseFile(testCobolFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        graphBuilder.addProgramToGraph(program, testCobolFile.getAbsolutePath());

        // Verify entities were added
        var records = graphBuilder.executeQuery(
                "MATCH (p:Program {name: 'TESTPROG'})-[:CONTAINS]->(e:Entity) RETURN e.name AS name, e.type AS type",
                Map.of()
        );

        assertFalse(records.isEmpty());
        // Should have at least the working storage variables
        int entityCount = records.size();
        for (var record : records) {
            assertNotNull(record.get("name").asString());
            assertNotNull(record.get("type").asString());
        }
        assertTrue(entityCount > 0);
    }

    @Test
    void shouldBuildRelationships() throws IOException {
        // Create a second COBOL file that calls the first one
        File secondCobolFile = tempDir.resolve("main.cbl").toFile();
        String secondCobolContent = 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. MAINPROG.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-VALUE PIC 9(3) VALUE 100.\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     CALL 'TESTPROG' USING WS-VALUE.\n" +
            "000800     STOP RUN.";
        Files.write(secondCobolFile.toPath(), secondCobolContent.getBytes());

        CobolParser parser = new CobolParser();
        
        // Add both programs to graph
        Program testProgram = parser.parseFile(testCobolFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        Program mainProgram = parser.parseFile(secondCobolFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        
        graphBuilder.addProgramToGraph(testProgram, testCobolFile.getAbsolutePath());
        graphBuilder.addProgramToGraph(mainProgram, secondCobolFile.getAbsolutePath());

        // Build relationships
        assertDoesNotThrow(() -> {
            graphBuilder.buildRelationships();
        });

        // Verify relationships were created
        var records = graphBuilder.executeQuery(
                "MATCH (source:Program)-[r:CALLS]->(target:Program) RETURN source.name AS source, target.name AS target",
                Map.of()
        );

        assertFalse(records.isEmpty());
        var record = records.get(1);
        assertEquals("MAINPROG", record.get("source").asString());
        assertEquals("TESTPROG", record.get("target").asString());
    }

    @Test
    void shouldExecuteCustomQueries() {
        // Test custom query execution
        var records = graphBuilder.executeQuery(
                "RETURN 'test' AS value",
                Map.of()
        );

        assertFalse(records.isEmpty());
        var record = records.get(0);
        assertEquals("test", record.get("value").asString());
    }

    @Test
    void shouldHandleQueryWithParameters() {
        // Test query with parameters
        var records = graphBuilder.executeQuery(
                "RETURN $param AS value",
                Map.of("param", "test_value")
        );

        assertFalse(records.isEmpty());
        var record = records.get(0);
        assertEquals("test_value", record.get("value").asString());
    }

    @Test
    void shouldCloseConnectionGracefully() {
        assertDoesNotThrow(() -> {
            graphBuilder.close();
        });
    }

    @Test
    void shouldHandleMultiplePrograms() throws IOException {
        // Create multiple COBOL files
        File[] cobolFiles = new File[3];
        String[] programNames = {"PROG1", "PROG2", "PROG3"};
        
        for (int i = 0; i < 3; i++) {
            cobolFiles[i] = tempDir.resolve("prog" + (i + 1) + ".cbl").toFile();
            String content = 
                "000100 IDENTIFICATION DIVISION.\n" +
                "000200 PROGRAM-ID. " + programNames[i] + ".\n" +
                "000300 DATA DIVISION.\n" +
                "000400 WORKING-STORAGE SECTION.\n" +
                "000500 01 WS-VAR PIC 9(3).\n" +
                "000600 PROCEDURE DIVISION.\n" +
                "000700     DISPLAY 'Hello from " + programNames[i] + "'.\n" +
                "000800     STOP RUN.";
            Files.write(cobolFiles[i].toPath(), content.getBytes());
        }

        CobolParser parser = new CobolParser();
        
        // Add all programs to graph
        for (int i = 0; i < 3; i++) {
            Program program = parser.parseFile(cobolFiles[i], CobolPreprocessor.CobolSourceFormatEnum.FIXED);
            graphBuilder.addProgramToGraph(program, cobolFiles[i].getAbsolutePath());
        }

        // Verify all programs were added
        var records = graphBuilder.executeQuery(
                "MATCH (p:Program) RETURN p.name AS name ORDER BY p.name",
                Map.of()
        );

        List<String> foundProgramNames = new ArrayList<>();
        for (var record : records) {
            foundProgramNames.add(record.get("name").asString());
        }
        
        assertTrue(foundProgramNames.size() >= 3);
    }

    @Test
    void shouldHandleComplexDependencies() throws IOException {
        // Create a more complex dependency structure
        File libFile = tempDir.resolve("lib.cbl").toFile();
        String libContent = 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. LIBRARY.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-LIB-VAR PIC X(10).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     DISPLAY 'Library function'.\n" +
            "000800     STOP RUN.";
        Files.write(libFile.toPath(), libContent.getBytes());

        File utilFile = tempDir.resolve("util.cbl").toFile();
        String utilContent = 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. UTILITY.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-UTIL-VAR PIC 9(5).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     CALL 'LIBRARY'.\n" +
            "000800     DISPLAY 'Utility function'.\n" +
            "000900     STOP RUN.";
        Files.write(utilFile.toPath(), utilContent.getBytes());

        File appFile = tempDir.resolve("app.cbl").toFile();
        String appContent = 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. APPLICATION.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-APP-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     CALL 'UTILITY'.\n" +
            "000800     CALL 'LIBRARY'.\n" +
            "000900     DISPLAY 'Application'.\n" +
            "001000     STOP RUN.";
        Files.write(appFile.toPath(), appContent.getBytes());

        CobolParser parser = new CobolParser();
        
        // Add all programs
        Program libProgram = parser.parseFile(libFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        Program utilProgram = parser.parseFile(utilFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        Program appProgram = parser.parseFile(appFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        
        graphBuilder.addProgramToGraph(libProgram, libFile.getAbsolutePath());
        graphBuilder.addProgramToGraph(utilProgram, utilFile.getAbsolutePath());
        graphBuilder.addProgramToGraph(appProgram, appFile.getAbsolutePath());

        // Build relationships
        graphBuilder.buildRelationships();

        // Verify dependency chain
        var records = graphBuilder.executeQuery(
                "MATCH path = (app:Program {name: 'APPLICATION'})-[:CALLS*]->(lib:Program {name: 'LIBRARY'}) " +
                "RETURN length(path) AS pathLength",
                Map.of()
        );

        assertFalse(records.isEmpty());
        var record = records.get(0);
        assertTrue(record.get("pathLength").asInt() > 0);
    }

    @Test
    void shouldHandleEmptyProgram() throws IOException {
        // Create an empty COBOL file
        File emptyFile = tempDir.resolve("empty.cbl").toFile();
        String emptyContent = 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. EMPTY.\n" +
            "000300 PROCEDURE DIVISION.\n" +
            "000400     STOP RUN.";
        Files.write(emptyFile.toPath(), emptyContent.getBytes());

        CobolParser parser = new CobolParser();
        Program program = parser.parseFile(emptyFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        assertDoesNotThrow(() -> {
            graphBuilder.addProgramToGraph(program, emptyFile.getAbsolutePath());
        });

        // Verify program was added even if empty
        var records = graphBuilder.executeQuery(
                "MATCH (p:Program {name: 'EMPTY'}) RETURN p.name AS name",
                Map.of()
        );

        assertFalse(records.isEmpty());
        var record = records.get(0);
        assertEquals("EMPTY", record.get("name").asString());
    }
} 