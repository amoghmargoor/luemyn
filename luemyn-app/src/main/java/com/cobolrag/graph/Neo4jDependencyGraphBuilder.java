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
package com.cobolrag.graph;

import com.cobolrag.config.CobolRagConfig;
import com.cobolrag.parser.CobolParser;
import com.cobolrag.parser.CobolParser.ProgramDependency;
import com.cobolrag.parser.CobolParser.ProgramEntity;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import org.neo4j.driver.*;
import org.neo4j.driver.Record;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Neo4jDependencyGraphBuilder implements AutoCloseable {
    private static final Logger logger = LoggerFactory.getLogger(Neo4jDependencyGraphBuilder.class);

    private final Driver driver;
    private final CobolParser cobolParser;
    private final CobolPreprocessor.CobolSourceFormatEnum cobolFormat;
    private final Map<String, Program> parsedPrograms = new ConcurrentHashMap<>();

    public Neo4jDependencyGraphBuilder(String uri, String user, String password, CobolPreprocessor.CobolSourceFormatEnum format) {
        this.driver = GraphDatabase.driver(uri, AuthTokens.basic(user, password));
        this.cobolParser = new CobolParser();
        this.cobolFormat = format;
        initializeDatabase();
    }

    private void initializeDatabase() {
        try (Session session = driver.session()) {
            // Create constraints and indexes
            session.run("CREATE CONSTRAINT program_name IF NOT EXISTS FOR (p:Program) REQUIRE p.name IS UNIQUE");
            session.run("CREATE CONSTRAINT entity_id IF NOT EXISTS FOR (e:Entity) REQUIRE e.id IS UNIQUE");
            session.run("CREATE INDEX program_file_idx IF NOT EXISTS FOR (p:Program) ON (p.filePath)");
        } catch (Exception e) {
            logger.error("Error initializing Neo4j database: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to initialize Neo4j database", e);
        }
    }

    public void addProgramToGraph(Program program, String filePath) {
        // Extract the actual PROGRAM-ID from the COBOL code
        String programName = null;
        if (!program.getCompilationUnits().isEmpty()) {
            ProgramUnit programUnit = program.getCompilationUnits().get(0).getProgramUnit();
            if (programUnit != null && programUnit.getIdentificationDivision() != null && 
                programUnit.getIdentificationDivision().getProgramIdParagraph() != null) {
                programName = programUnit.getIdentificationDivision().getProgramIdParagraph().getName();
            }
        }
        
        // Fallback to filename if PROGRAM-ID is not available
        if (programName == null) {
            programName = program.getCompilationUnits().get(0).getName();
        }
        
        final String finalProgramName = programName;

        // Store the parsed program in memory
        parsedPrograms.put(finalProgramName, program);

        try (Session session = driver.session()) {
            // Create program node
            session.executeWrite(tx -> {
                tx.run("MERGE (p:Program {name: $name}) " +
                                "SET p.filePath = $filePath, p.lastUpdated = datetime()",
                        Map.of("name", finalProgramName, "filePath", filePath));
                return null;
            });

            // Extract and add entities
            List<ProgramEntity> entities = cobolParser.extractEntities(program);
            for (ProgramEntity entity : entities) {
                addEntityToGraph(session, entity);
            }

            logger.info("Added program {} to graph with {} entities", finalProgramName, entities.size());
        } catch (Exception e) {
            logger.error("Error adding program to graph: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to add program to graph", e);
        }
    }

    private void addEntityToGraph(Session session, ProgramEntity entity) {
        session.executeWrite(tx -> {
            String entityId = entity.getProgram() + ":" + entity.getName();

            tx.run("MERGE (e:Entity {id: $id}) " +
                            "SET e.name = $name, e.type = $type, e.section = $section, " +
                            "e.description = $description, e.lastUpdated = datetime()",
                    Map.of(
                            "id", entityId,
                            "name", entity.getName(),
                            "type", entity.getType(),
                            "section", entity.getSection(),
                            "description", entity.getDescription()
                    ));

            // Create relationship from program to entity
            tx.run("MATCH (p:Program {name: $program}), (e:Entity {id: $entityId}) " +
                            "MERGE (p)-[:CONTAINS]->(e)",
                    Map.of("program", entity.getProgram(), "entityId", entityId));

            return null;
        });
    }

    public void buildRelationships() {
        try (Session session = driver.session()) {
            // Use the in-memory parsed programs instead of re-parsing from disk
            for (Map.Entry<String, Program> entry : parsedPrograms.entrySet()) {
                String programName = entry.getKey();
                Program program = entry.getValue();

                // Extract dependencies from the in-memory program
                List<ProgramDependency> dependencies = cobolParser.extractDependencies(program);

                // Add dependencies to graph
                for (ProgramDependency dependency : dependencies) {
                    addDependencyToGraph(session, dependency);
                }

                logger.info("Built {} relationships for program {}", dependencies.size(), programName);
            }
        } catch (Exception e) {
            logger.error("Error building relationships: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to build relationships", e);
        }
    }

    private void addDependencyToGraph(Session session, ProgramDependency dependency) {
        session.executeWrite(tx -> {
            tx.run("MATCH (source:Program {name: $sourceName}) " +
                            "MERGE (target:Program {name: $targetName}) " +
                            "MERGE (source)-[r:" + dependency.getRelationshipType() + "]->(target) " +
                            "SET r.lastUpdated = datetime()",
                    Map.of(
                            "sourceName", dependency.getSourceProgram(),
                            "targetName", dependency.getTargetProgram()
                    ));
            return null;
        });
    }

    public List<Record> executeQuery(String query, Map<String, Object> parameters) {
        try (Session session = driver.session()) {
            Result result = session.run(query, parameters);
            List<Record> records = result.list();
            return records;
        }
    }

    public Driver getDriver() {
        return driver;
    }

    @Override
    public void close() {
        driver.close();
    }
}
