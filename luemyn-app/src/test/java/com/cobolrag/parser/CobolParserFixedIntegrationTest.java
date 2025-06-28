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
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

class CobolParserFixedFormatTest {
    CobolParser parser = new CobolParser();

    @Test
    void parseBasicCallProgram(@TempDir Path tempDir) throws Exception {
        String cobolCode =
                "000100 IDENTIFICATION DIVISION.\n" +
                        "000200 PROGRAM-ID. CALLER.\n" +
                        "000300 ENVIRONMENT DIVISION.\n" +
                        "000400 DATA DIVISION.\n" +
                        "000500 WORKING-STORAGE SECTION.\n" +
                        "000600 01 WS-COUNTER PIC 9(3).\n" +
                        "000700 PROCEDURE DIVISION.\n" +
                        "000800     CALL 'SUBPROG' USING WS-COUNTER.\n" +
                        "000900     GOBACK.\n";

        File testFile = createCobolFile(tempDir, "CALLER.cbl", cobolCode);
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        List<CobolParser.ProgramDependency> dependencies = parser.extractDependencies(program);
        assertEquals(1, dependencies.size());
        assertEquals("SUBPROG", dependencies.get(0).getTargetProgram());
    }

    @Test
    void parseProgramWithCopybook(@TempDir Path tempDir) throws Exception {
        // Create copybook directory structure
        Path copybookDir = tempDir.resolve("copybooks");
        Files.createDirectories(copybookDir);

        // Create CUSTOMER copybook
        String customerCopybook =
                "000100 01 CUSTOMER-RECORD.\n" +
                        "000200     05 CUST-CODE       PIC 9(5).\n" +
                        "000300     05 CUST-NAME       PIC X(30).\n";
        Path copybook1 = Files.writeString(copybookDir.resolve("CUSTOMER.cpy"), customerCopybook);

        // Create PRODUCT copybook
        String productCopybook =
                "000100 01 PRODUCT-RECORD.\n" +
                        "000200     05 PROD-CODE       PIC 9(5).\n" +
                        "000300     05 PROD-DESC       PIC X(40).\n";
        Path copybook2 = Files.writeString(copybookDir.resolve("PRODUCT.cpy"), productCopybook);
        List<File> copyBooks = new ArrayList<>();
        copyBooks.add(copybook1.toFile());
        copyBooks.add(copybook2.toFile());

        // Main program using COPY statements
        String cobolCode =
                "000100 IDENTIFICATION DIVISION.\n" +
                        "000200 PROGRAM-ID. INVOICE.\n" +
                        "000300 ENVIRONMENT DIVISION.\n" +
                        "000400 INPUT-OUTPUT SECTION.\n" +
                        "000500 DATA DIVISION.\n" +
                        "000600 WORKING-STORAGE SECTION.\n" +
                        "000700 COPY CUSTOMER.\n" +
                        "000800 COPY PRODUCT.\n" +
                        "000900 PROCEDURE DIVISION.\n" +
                        "001000     MOVE 12345 TO CUST-CODE.\n" +
                        "001100     MOVE 67890 TO PROD-CODE.\n" +
                        "001200     DISPLAY 'CUSTOMER: ' CUST-CODE ' PRODUCT: ' PROD-CODE.\n" +
                        "001300     GOBACK.\n";

        File testFile = createCobolFile(tempDir, "INVOICE.cbl", cobolCode);

        // Configure parser with copybook directories
        CobolParserParams params = new CobolParserParamsImpl();

        //params.setCopyBookDirectories(List.of(copybookDir.toFile()));
        params.setCopyBookFiles(copyBooks);

        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED, params);
        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);

        assertAll(
                () -> assertTrue(entities.stream()
                                .anyMatch(e -> e.getName().equals("CUSTOMER-RECORD") &&
                                        e.getType().equals("VARIABLE")),
                        "Should detect CUSTOMER copybook"),
                () -> assertTrue(entities.stream()
                                .anyMatch(e -> e.getName().equals("PRODUCT-RECORD") &&
                                        e.getType().equals("VARIABLE")),
                        "Should detect PRODUCT copybook"),
                () -> assertTrue(entities.stream()
                                .anyMatch(e -> e.getName().equals("CUST-CODE")),
                        "Should detect fields from copybooks")
        );
    }

//    @Test
//    void parseProgramWithCopybook(@TempDir Path tempDir) throws Exception {
//        String cobolCode =
//                "000100 IDENTIFICATION DIVISION.\n" +
//                        "000200 PROGRAM-ID. INVOICE.\n" +
//                        "000300 ENVIRONMENT DIVISION.\n" +
//                        "000400 DATA DIVISION.\n" +
//                        "000500 WORKING-STORAGE SECTION.\n" +
//                        "000600 COPY CUSTOMER.\n" +
//                        "000700 COPY PRODUCT.\n" +
//                        "000800 PROCEDURE DIVISION.\n" +
//                        "000900     CALL 'CALCTAX' USING CUST-CODE PROD-CODE.\n" +
//                        "001000     GOBACK.\n";
//
//        File testFile = createCobolFile(tempDir, "INVOICE.cbl", cobolCode);
//        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
//
//        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);
//        assertTrue(entities.stream().anyMatch(e -> e.getType().equals("COPYBOOK")));
//    }

    @Test
    void parseMinimalValidProgram(@TempDir Path tempDir) throws Exception {
        String cobolCode =
                "000100 IDENTIFICATION DIVISION.\n" +
                        "000200 PROGRAM-ID. MINIMAL.\n" +
                        "000300 ENVIRONMENT DIVISION.\n" +
                        "000400 DATA DIVISION.\n" +
                        "000500 WORKING-STORAGE SECTION.\n" +
                        "000600 01 WS-DUMMY PIC X.\n" +
                        "000700 PROCEDURE DIVISION.\n" +
                        "000800     DISPLAY 'MINIMAL PROGRAM'.\n" +
                        "000900     GOBACK.\n";

        File testFile = createCobolFile(tempDir, "MINIMAL.cbl", cobolCode);
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        assertNotNull(program);
        assertEquals(1, program.getCompilationUnits().size());
    }

    @Test
    void parseComplexDataStructures(@TempDir Path tempDir) throws Exception {
        String cobolCode =
                "000100 IDENTIFICATION DIVISION.\n" +
                        "000200 PROGRAM-ID. DATADEMO.\n" +
                        "000300 ENVIRONMENT DIVISION.\n" +
                        "000400 INPUT-OUTPUT SECTION.\n" +
                        "000500 FILE-CONTROL.\n" +
                        "000600     SELECT IN-FILE ASSIGN TO 'INPUT.DAT'.\n" +
                        "000700 DATA DIVISION.\n" +
                        "000800 FILE SECTION.\n" +
                        "000900 FD  IN-FILE.\n" +
                        "001000 01  INPUT-REC PIC X(80).\n" +
                        "001100 WORKING-STORAGE SECTION.\n" +
                        "001200 01  CUSTOMER-DATA.\n" +
                        "001300     05 CUST-ID       PIC 9(5).\n" +
                        "001400     05 CUST-NAME     PIC X(30).\n" +
                        "001500     05 CUST-BALANCE  PIC S9(7)V99 COMP-3.\n" +
                        "001600 PROCEDURE DIVISION.\n" +
                        "001700     OPEN INPUT IN-FILE.\n" +
                        "001800     CLOSE IN-FILE.\n" +
                        "001900     GOBACK.\n";

        File testFile = createCobolFile(tempDir, "DATADEMO.cbl", cobolCode);
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);
        assertTrue(entities.stream().anyMatch(e -> e.getName().equals("CUST-BALANCE")));
        assertTrue(entities.stream().anyMatch(e -> e.getType().equals("FILE")));
    }

    @Test
    void parseLargeCobolProgram(@TempDir Path tempDir) throws Exception {
        String cobolCode =
                "000100 IDENTIFICATION DIVISION.\n" +
                        "000200 PROGRAM-ID. INVENTRY.\n" +
                        "000300 ENVIRONMENT DIVISION.\n" +
                        "000400 INPUT-OUTPUT SECTION.\n" +
                        "000500 FILE-CONTROL.\n" +
                        "000600     SELECT ITEM-FILE   ASSIGN TO 'ITEMS.DAT'\n" +
                        "000700                        ORGANIZATION IS INDEXED\n" +
                        "000800                        ACCESS MODE IS RANDOM\n" +
                        "000900                        RECORD KEY IS ITEM-CODE.\n" +
                        "001000 DATA DIVISION.\n" +
                        "001100 FILE SECTION.\n" +
                        "001200 FD  ITEM-FILE.\n" +
                        "001300 01  ITEM-RECORD.\n" +
                        "001400     05 ITEM-CODE      PIC 9(5).\n" +
                        "001500     05 ITEM-DESC      PIC X(30).\n" +
                        "001600     05 UNIT-PRICE     PIC 9(5)V99.\n" +
                        "001700     05 QTY-ON-HAND    PIC 9(5).\n" +
                        "001800     05 REORDER-POINT  PIC 9(5).\n" +
                        "001900 WORKING-STORAGE SECTION.\n" +
                        "002000 01  WS-FLAGS.\n" +
                        "002100     05 WS-EOF-FLAG    PIC X VALUE 'N'.\n" +
                        "002200         88 EOF        VALUE 'Y'.\n" +
                        "002300 01  WS-DISPLAY-LINE.\n" +
                        "002400     05 FILLER         PIC X(10) VALUE SPACES.\n" +
                        "002500     05 DIS-CODE       PIC 9(5).\n" +
                        "002600     05 FILLER         PIC X(5) VALUE SPACES.\n" +
                        "002700     05 DIS-DESC       PIC X(30).\n" +
                        "002800     05 FILLER         PIC X(5) VALUE SPACES.\n" +
                        "002900     05 DIS-QTY        PIC ZZZZ9.\n" +
                        "003000 PROCEDURE DIVISION.\n" +
                        "003100 MAIN-LOGIC.\n" +
                        "003200     PERFORM INITIALIZE-PROGRAM\n" +
                        "003300     PERFORM PROCESS-RECORDS UNTIL EOF\n" +
                        "003400     PERFORM TERMINATE-PROGRAM\n" +
                        "003500     GOBACK.\n" +
                        "003600 INITIALIZE-PROGRAM.\n" +
                        "003700     OPEN INPUT ITEM-FILE\n" +
                        "003800     READ ITEM-FILE NEXT RECORD\n" +
                        "003900         AT END SET EOF TO TRUE\n" +
                        "004000     END-READ.\n" +
                        "004100 PROCESS-RECORDS.\n" +
                        "004200     PERFORM DISPLAY-ITEM-INFO\n" +
                        "004300     READ ITEM-FILE NEXT RECORD\n" +
                        "004400         AT END SET EOF TO TRUE\n" +
                        "004500     END-READ.\n" +
                        "004600 DISPLAY-ITEM-INFO.\n" +
                        "004700     MOVE ITEM-CODE TO DIS-CODE\n" +
                        "004800     MOVE ITEM-DESC TO DIS-DESC\n" +
                        "004900     MOVE QTY-ON-HAND TO DIS-QTY\n" +
                        "005000     DISPLAY WS-DISPLAY-LINE.\n" +
                        "005100 TERMINATE-PROGRAM.\n" +
                        "005200     CLOSE ITEM-FILE.\n";

        File testFile = createCobolFile(tempDir, "INVENTRY.cbl", cobolCode);
        Program program = parser.parseFile(testFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);

        List<CobolParser.ProgramEntity> entities = parser.extractEntities(program);
        List<CobolParser.ProgramDependency> deps = parser.extractDependencies(program);

        assertAll(
                () -> assertTrue(entities.size() > 10),
                () -> assertTrue(deps.isEmpty()),
                () -> assertTrue(entities.stream().anyMatch(e -> e.getName() != null && e.getName().equals("ITEM-RECORD") && e.getType().equals("RECORD"))),
                () -> assertTrue(entities.stream().anyMatch(e -> e.getName() != null && e.getName().equals("DIS-CODE"))),
                () -> assertTrue(entities.stream().anyMatch(e -> e.getName() != null && e.getName().equals("ITEM-CODE") && e.getType().equals("FIELD"))),
                () -> assertTrue(entities.stream().anyMatch(e -> e.getName() != null && e.getName().equals("WORKING-STORAGE SECTION") && e.getType().equals("SECTION"))),
                () -> assertTrue(entities.stream().anyMatch(e -> e.getName() != null && e.getName().equals("FILE SECTION") && e.getType().equals("SECTION")))
        );
    }

    private File createCobolFile(Path dir, String filename, String content) throws Exception {
        Path filePath = dir.resolve(filename);
        Files.writeString(filePath, content);
        return filePath.toFile();
    }
}
