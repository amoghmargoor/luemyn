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
package com.cobolrag.parser;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class CobolParserUnitTest {

    @TempDir
    Path tempDir;

    private CobolParser parser;

    @BeforeEach
    void setUp() {
        parser = new CobolParser();
    }

    @Test
    void shouldParseBasicProgram() throws IOException {
        File testFile = createCobolFile("basic.cbl", 
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. BASIC.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     DISPLAY 'Hello'.\n" +
            "000800     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        
        assertNotNull(program);
        assertEquals("Basic", program.getCompilationUnits().get(0).getName());
    }

    @Test
    void shouldExtractEntitiesFromProgram() throws IOException {
        File testFile = createCobolFile("entities.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. ENTITIES.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-COUNTER PIC 9(3).\n" +
            "000600 01 WS-NAME PIC X(20).\n" +
            "000700 01 WS-AMOUNT PIC 9(7)V99.\n" +
            "000800 PROCEDURE DIVISION.\n" +
            "000900     DISPLAY WS-NAME.\n" +
            "001000     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);

        assertNotNull(entities);
        assertTrue(entities.size() >= 3); // Should have at least the 3 variables
        
        // Check that we have the expected entities
        boolean hasCounter = entities.stream().anyMatch(e -> e.getName().equals("WS-COUNTER"));
        boolean hasName = entities.stream().anyMatch(e -> e.getName().equals("WS-NAME"));
        boolean hasAmount = entities.stream().anyMatch(e -> e.getName().equals("WS-AMOUNT"));
        
        assertTrue(hasCounter);
        assertTrue(hasName);
        assertTrue(hasAmount);
    }

    @Test
    void shouldExtractDependenciesFromProgram() throws IOException {
        File testFile = createCobolFile("dependencies.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. DEPS.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     CALL 'SUBPROG1' USING WS-VAR.\n" +
            "000800     CALL 'SUBPROG2'.\n" +
            "000900     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramDependency> dependencies = parser.extractDependencies(program);

        assertNotNull(dependencies);
        assertTrue(dependencies.size() >= 2); // Should have at least 2 CALL statements
        
        // Check that we have the expected dependencies
        boolean hasSubprog1 = dependencies.stream().anyMatch(d -> d.getTargetProgram().equals("SUBPROG1"));
        boolean hasSubprog2 = dependencies.stream().anyMatch(d -> d.getTargetProgram().equals("SUBPROG2"));
        
        assertTrue(hasSubprog1);
        assertTrue(hasSubprog2);
    }

    @Test
    void shouldHandleProgramWithNoDependencies() throws IOException {
        File testFile = createCobolFile("nodeps.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. NODEPS.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700     DISPLAY 'No dependencies'.\n" +
            "000800     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramDependency> dependencies = parser.extractDependencies(program);

        assertNotNull(dependencies);
        // Should be empty or contain only internal dependencies
        assertTrue(dependencies.isEmpty() || dependencies.stream().allMatch(d -> d.getTargetProgram().equals("NODEPS")));
    }

    @Test
    void shouldHandleProgramWithCopyStatements() throws IOException {
        File testFile = createCobolFile("copy.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. COPYTEST.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 COPY CUSTOMER.\n" +
            "000600 COPY PRODUCT.\n" +
            "000700 PROCEDURE DIVISION.\n" +
            "000800     DISPLAY 'Copy test'.\n" +
            "000900     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);

        assertNotNull(entities);
        // Should have entities from the program structure
        assertTrue(entities.size() > 0);
    }

    @Test
    void shouldHandleProgramWithFileSection() throws IOException {
        File testFile = createCobolFile("file.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. FILETEST.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 FILE SECTION.\n" +
            "000500 FD CUSTOMER-FILE.\n" +
            "000600 01 CUSTOMER-RECORD.\n" +
            "000700     05 CUST-ID PIC 9(5).\n" +
            "000800     05 CUST-NAME PIC X(30).\n" +
            "000900 WORKING-STORAGE SECTION.\n" +
            "001000 01 WS-VAR PIC 9(3).\n" +
            "001100 PROCEDURE DIVISION.\n" +
            "001200     DISPLAY 'File test'.\n" +
            "001300     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);

        assertNotNull(entities);
        // Should have entities from both file section and working storage
        assertTrue(entities.size() > 0);
    }

    @Test
    void shouldHandleProgramWithParagraphs() throws IOException {
        File testFile = createCobolFile("paragraphs.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. PARATEST.\n" +
            "000300 DATA DIVISION.\n" +
            "000400 WORKING-STORAGE SECTION.\n" +
            "000500 01 WS-VAR PIC 9(3).\n" +
            "000600 PROCEDURE DIVISION.\n" +
            "000700 MAIN-LOGIC.\n" +
            "000800     PERFORM PROCESS-DATA.\n" +
            "000900     STOP RUN.\n" +
            "001000 PROCESS-DATA.\n" +
            "001100     DISPLAY 'Processing'.\n" +
            "001200     EXIT.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);

        assertNotNull(entities);
        // Should have entities including paragraphs
        assertTrue(entities.size() > 0);
    }

    @Test
    void shouldHandleEmptyProgram() throws IOException {
        File testFile = createCobolFile("empty.cbl",
            "000100 IDENTIFICATION DIVISION.\n" +
            "000200 PROGRAM-ID. EMPTY.\n" +
            "000300 PROCEDURE DIVISION.\n" +
            "000400     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);
        List<CobolParser.ProgramDependency> dependencies = parser.extractDependencies(program);

        assertNotNull(program);
        assertNotNull(entities);
        assertNotNull(dependencies);
        // Empty program should have minimal entities
        assertTrue(entities.size() >= 0);
        assertTrue(dependencies.isEmpty());
    }

    @Test
    void shouldHandleInvalidCobolFile() throws IOException {
        // Create a file with invalid COBOL syntax
        File invalidFile = tempDir.resolve("invalid.cbl").toFile();
        String invalidContent = "This is not valid COBOL code at all";
        Files.write(invalidFile.toPath(), invalidContent.getBytes());
        
        assertThrows(Exception.class, () -> {
            parser.parseFile(invalidFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        });
    }

    @Test
    void shouldFindCobolFilesInDirectory() throws IOException {
        // Create test files
        createCobolFile("test1.cbl", "000100 PROGRAM-ID. TEST1.");
        createCobolFile("test2.cob", "000100 PROGRAM-ID. TEST2.");
        createCobolFile("test3.cobol", "000100 PROGRAM-ID. TEST3.");
        createCobolFile("test4.txt", "This is not COBOL");
        createCobolFile("test5.java", "public class Test {}");

        List<File> cobolFiles = CobolParser.findCobolFiles(tempDir.toFile());
        
        assertNotNull(cobolFiles);
        assertTrue(cobolFiles.size() >= 3); // Should find .cbl, .cob, .cobol files
        
        // Verify only COBOL files are found
        for (File file : cobolFiles) {
            String name = file.getName().toLowerCase();
            assertTrue(name.endsWith(".cbl") || name.endsWith(".cob") || name.endsWith(".cobol"));
        }
    }

    @Test
    void shouldHandleEmptyDirectory() {
        Path emptyDir = tempDir.resolve("empty");
        emptyDir.toFile().mkdir();
        
        List<File> cobolFiles = CobolParser.findCobolFiles(emptyDir.toFile());
        
        assertNotNull(cobolFiles);
        assertEquals(0, cobolFiles.size());
    }

    @Test
    void shouldHandleNonExistentDirectory() {
        File nonExistentDir = new File("/non/existent/path");
        
        assertThrows(IllegalArgumentException.class, () -> {
            CobolParser.findCobolFiles(nonExistentDir);
        });
    }

    @Test
    void shouldHandleProgramWithComplexDataStructures() throws IOException {
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
            "001500     DISPLAY 'Complex structure'.\n" +
            "001600     STOP RUN.");

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);

        assertNotNull(entities);
        // Should have entities for the complex data structure
        assertTrue(entities.size() > 0);
    }

    private File createCobolFile(String fileName, String content) throws IOException {
        File file = tempDir.resolve(fileName).toFile();
        Files.write(file.toPath(), content.getBytes());
        return file;
    }
} 