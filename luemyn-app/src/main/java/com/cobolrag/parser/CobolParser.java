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

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class CobolParser {
    private static final Logger logger = LoggerFactory.getLogger(CobolParser.class);
    private final CobolParserRunnerImpl parserRunner;

    public CobolParser() {
        this.parserRunner = new CobolParserRunnerImpl();
    }

    public Program parseFile(File file, CobolSourceFormatEnum format) {
        try {
            return parserRunner.analyzeFile(file, format);
        } catch (Exception e) {
            logger.error("Error parsing COBOL file {}: {}", file.getName(), e.getMessage(), e);
            throw new RuntimeException("Failed to parse COBOL file: " + file.getName(), e);
        }
    }

    public Program parseFile(File file, CobolSourceFormatEnum format, CobolParserParams params) {
        try {
            params.setFormat(format);
            return parserRunner.analyzeFile(file, params);
        } catch (Exception e) {
            logger.error("Error parsing COBOL file {}: {}", file.getName(), e.getMessage(), e);
            throw new RuntimeException("Failed to parse COBOL file: " + file.getName(), e);
        }
    }

    public static List<File> findCobolFiles(File directory) {
        if (!directory.isDirectory()) {
            throw new IllegalArgumentException("Path must be a directory: " + directory.getPath());
        }

        List<File> cobolFiles = new ArrayList<>();
        findCobolFilesRecursive(directory, cobolFiles);
        return cobolFiles;
    }

    private static void findCobolFilesRecursive(File directory, List<File> cobolFiles) {
        File[] files = directory.listFiles();
        if (files == null) return;

        for (File file : files) {
            if (file.isDirectory()) {
                findCobolFilesRecursive(file, cobolFiles);
            } else if (isCobolFile(file)) {
                cobolFiles.add(file);
            }
        }
    }

    private static boolean isCobolFile(File file) {
        String name = file.getName().toLowerCase();
        return name.endsWith(".cbl") || name.endsWith(".cob") || name.endsWith(".cobol");
    }

    private String extractCalledProgramName(CallStatement callStatement) {
        try {
            // Try to get the program name from the call statement
            if (callStatement.getProgramValueStmt() != null) {
                ValueStmt programValueStmt = callStatement.getProgramValueStmt();
                
                // Check if it's a literal value
                if (programValueStmt instanceof io.proleap.cobol.asg.metamodel.valuestmt.LiteralValueStmt) {
                    io.proleap.cobol.asg.metamodel.valuestmt.LiteralValueStmt literalStmt = 
                        (io.proleap.cobol.asg.metamodel.valuestmt.LiteralValueStmt) programValueStmt;
                    if (literalStmt.getLiteral() != null) {
                        Object value = literalStmt.getLiteral().getValue();
                        return value != null ? value.toString() : null;
                    }
                }
                
                // Try to get the value as a string representation
                String value = programValueStmt.toString();
                // If it's a quoted string, extract the content
                if (value.startsWith("'") && value.endsWith("'")) {
                    return value.substring(1, value.length() - 1);
                }
                return value;
            }
            
            // If that doesn't work, try to get it from the statement text
            String statementText = callStatement.toString();
            // Look for CALL 'PROGRAM' pattern
            if (statementText.contains("CALL")) {
                // Simple regex to extract program name from CALL statement
                // This is a fallback and might need refinement
                return statementText.replaceAll(".*CALL\\s+['\"]([^'\"]+)['\"].*", "$1");
            }
            
            return null;
        } catch (Exception e) {
            logger.debug("Could not extract program name from call statement: {}", e.getMessage());
            return null;
        }
    }

    private String extractProgramId(ProgramUnit programUnit) {
        // Try to get the program name from the identification division
        if (programUnit.getIdentificationDivision() != null && 
            programUnit.getIdentificationDivision().getProgramIdParagraph() != null) {
            return programUnit.getIdentificationDivision().getProgramIdParagraph().getName();
        }
        
        // Fallback to compilation unit name if identification division is not available
        return null;
    }

    public List<ProgramDependency> extractDependencies(Program program) {
        Set<ProgramDependency> dependencies = new HashSet<>();

        for (CompilationUnit compilationUnit : program.getCompilationUnits()) {
            ProgramUnit programUnit = compilationUnit.getProgramUnit();
            if (programUnit == null) continue;

            String programName = extractProgramId(programUnit);
            if (programName == null) {
                programName = compilationUnit.getName(); // Fallback to filename
            }
            
            final String finalProgramName = programName;

            // Extract CALL statements to find dependencies
            ProcedureDivision procedureDivision = programUnit.getProcedureDivision();
            if (procedureDivision != null) {
                procedureDivision.getSections().forEach(section -> {
                    section.getParagraphs().forEach(paragraph -> {
                        paragraph.getStatements().forEach(statement -> {
                            if (statement.getStatementType() == StatementTypeEnum.CALL) {
                                CallStatement callStatement = (CallStatement) statement;
                                // Try to get the program name from the call statement
                                String calledProgram = extractCalledProgramName(callStatement);
                                if (calledProgram != null) {
                                    dependencies.add(new ProgramDependency(finalProgramName, calledProgram, "CALLS"));
                                }
                            }
                        });
                    });
                });
                procedureDivision.getStatements().forEach(statement -> {
                    if (statement.getStatementType() == StatementTypeEnum.CALL) {
                        CallStatement callStatement = (CallStatement) statement;
                        // Try to get the program name from the call statement
                        String calledProgram = extractCalledProgramName(callStatement);
                        if (calledProgram != null) {
                            dependencies.add(new ProgramDependency(finalProgramName, calledProgram, "CALLS"));
                        }
                    }
                });
            }

            // Extract COPY statements for dependencies
            // This would require analyzing the AST for COPY statements
            // Simplified implementation for demonstration
        }

        return new ArrayList<>(dependencies);
    }

    public List<ProgramEntity> extractEntities(Program program) {
        List<ProgramEntity> entities = new ArrayList<>();

        for (CompilationUnit compilationUnit : program.getCompilationUnits()) {
            ProgramUnit programUnit = compilationUnit.getProgramUnit();
            if (programUnit == null) continue;

            String programName = extractProgramId(programUnit);
            if (programName == null) {
                programName = compilationUnit.getName(); // Fallback to filename
            }
            
            final String finalProgramName = programName;

            // Extract data divisions
            DataDivision dataDivision = programUnit.getDataDivision();
            if (dataDivision != null) {
                // Working storage section
                if (dataDivision.getWorkingStorageSection() != null) {
                    entities.add(new ProgramEntity(
                            finalProgramName,
                            "WORKING-STORAGE SECTION",
                            "SECTION",
                            "DATA_DIVISION",
                            "Variables section"
                    ));
                    dataDivision.getWorkingStorageSection().getDataDescriptionEntries().forEach( entry -> {
                        entities.add(new ProgramEntity(finalProgramName, entry.getName(), "VARIABLE",
                                "WORKING_STORAGE", getDataDescription(entry)));
                    });
                }
                // File section
                if (dataDivision.getFileSection() != null) {
                    entities.add(new ProgramEntity(
                            finalProgramName,
                            "FILE SECTION",
                            "SECTION",
                            "DATA_DIVISION",
                            "File storage definition section"
                    ));
                    dataDivision.getFileSection().getFileDescriptionEntries().forEach(fdEntry -> {
                        // Add FD entry itself
                        entities.add(new ProgramEntity(finalProgramName, fdEntry.getName(), "FILE",
                                "FILE_SECTION", "File Description"));

                        // Add associated data records and fields
                        fdEntry.getDataDescriptionEntries().forEach(dataEntry -> {
                            String type = dataEntry.getLevelNumber() == 1 ? "RECORD" : "FIELD";
                            entities.add(new ProgramEntity(finalProgramName, dataEntry.getName(), type,
                                    "FILE_SECTION", getDataDescription(dataEntry)));
                        });
                    });
                }
            }

            // Extract procedure division sections and paragraphs
            ProcedureDivision procedureDivision = programUnit.getProcedureDivision();
            if (procedureDivision != null) {
                procedureDivision.getSections().forEach( section -> {
                    entities.add(new ProgramEntity(finalProgramName, section.getName(), "SECTION",
                            "PROCEDURE_DIVISION", "Section in " + finalProgramName));

                    section.getParagraphs().forEach( paragraph -> {
                        entities.add(new ProgramEntity(finalProgramName, paragraph.getName(), "PARAGRAPH",
                                "PROCEDURE_DIVISION", "Paragraph in section " + section.getName()));
                    });
                });
            }
        }

        return entities;
    }

    private String getDataDescription(DataDescriptionEntry entry) {
        StringBuilder description = new StringBuilder();

        // Get level number
        description.append("Level: ").append(entry.getLevelNumber()).append(", ");

        // Get basic information from toString
        String entryString = entry.toString();
        if (entryString != null && !entryString.isEmpty()) {
            description.append("Type: ").append(entryString);
        }

        // Remove trailing comma and space if present
        String result = description.toString();
        if (result.endsWith(", ")) {
            result = result.substring(0, result.length() - 2);
        }

        return result.isEmpty() ? "Data field" : result;
    }


    public static class ProgramDependency {
        private final String sourceProgram;
        private final String targetProgram;
        private final String relationshipType;

        public ProgramDependency(String sourceProgram, String targetProgram, String relationshipType) {
            this.sourceProgram = sourceProgram;
            this.targetProgram = targetProgram;
            this.relationshipType = relationshipType;
        }

        public String getSourceProgram() {
            return sourceProgram;
        }

        public String getTargetProgram() {
            return targetProgram;
        }

        public String getRelationshipType() {
            return relationshipType;
        }

        @Override
        public boolean equals(Object o) {
            if (o == null || getClass() != o.getClass()) return false;
            ProgramDependency that = (ProgramDependency) o;
            return Objects.equals(sourceProgram, that.sourceProgram) && Objects.equals(targetProgram, that.targetProgram) && Objects.equals(relationshipType, that.relationshipType);
        }

        @Override
        public int hashCode() {
            return Objects.hash(sourceProgram, targetProgram, relationshipType);
        }
    }

    public static class ProgramEntity {
        private final String program;
        private final String name;
        private final String type;
        private final String section;
        private final String description;

        public ProgramEntity(String program, String name, String type, String section, String description) {
            this.program = program;
            this.name = name;
            this.type = type;
            this.section = section;
            this.description = description;
        }

        public String getProgram() {
            return program;
        }

        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }

        public String getSection() {
            return section;
        }

        public String getDescription() {
            return description;
        }
    }
}
