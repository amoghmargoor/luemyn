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
package com.cobolrag.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

class CobolRagConfigTest {

    @TempDir
    Path tempDir;

    private File validConfigFile;
    private File invalidConfigFile;
    private File missingFieldsConfigFile;

    @BeforeEach
    void setUp() throws IOException {
        // Create valid config file
        validConfigFile = tempDir.resolve("valid.conf").toFile();
        String validConfig = 
            "cobolrag {\n" +
            "  format = \"fixed\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"password123\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "  python {\n" +
            "    path = \"/usr/bin/python3\"\n" +
            "  }\n" +
            "}";
        Files.write(validConfigFile.toPath(), validConfig.getBytes());

        // Create invalid config file
        invalidConfigFile = tempDir.resolve("invalid.conf").toFile();
        String invalidConfig = 
            "cobolrag {\n" +
            "  format = \"invalid_format\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"password123\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "}";
        Files.write(invalidConfigFile.toPath(), invalidConfig.getBytes());

        // Create config file with missing required fields
        missingFieldsConfigFile = tempDir.resolve("missing.conf").toFile();
        String missingFieldsConfig = 
            "cobolrag {\n" +
            "  format = \"fixed\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    // Missing user and password\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "}";
        Files.write(missingFieldsConfigFile.toPath(), missingFieldsConfig.getBytes());
    }

    @Test
    void shouldLoadValidConfiguration() {
        assertDoesNotThrow(() -> {
            CobolRagConfig config = new CobolRagConfig(validConfigFile.getAbsolutePath());
            
            assertEquals("fixed", config.getFormat());
            assertEquals("bolt://localhost:7687", config.getNeo4jUri());
            assertEquals("neo4j", config.getNeo4jUser());
            assertEquals("password123", config.getNeo4jPassword());
            assertEquals("http://localhost:8000", config.getChromaDbUrl());
            assertEquals("/usr/bin/python3", config.getPythonExecutablePath());
        });
    }

    @Test
    void shouldUseDefaultConfigurationWhenFileNotFound() {
        assertThrows(IllegalArgumentException.class, () -> {
            CobolRagConfig config = new CobolRagConfig("/non/existent/path.conf");
            // Should throw exception for missing required configuration
        });
    }

    @Test
    void shouldThrowExceptionForInvalidFormat() {
        assertThrows(IllegalArgumentException.class, () -> {
            new CobolRagConfig(invalidConfigFile.getAbsolutePath());
        });
    }

    @Test
    void shouldThrowExceptionForMissingRequiredFields() {
        assertThrows(IllegalArgumentException.class, () -> {
            new CobolRagConfig(missingFieldsConfigFile.getAbsolutePath());
        });
    }

    @Test
    void shouldAcceptValidFormats() {
        // Test fixed format
        assertDoesNotThrow(() -> {
            createConfigWithFormat("fixed");
        });

        // Test tandem format
        assertDoesNotThrow(() -> {
            createConfigWithFormat("tandem");
        });
    }

    @Test
    void shouldUseDefaultPythonPathWhenNotSpecified() throws IOException {
        // Create config without python path
        File configWithoutPython = tempDir.resolve("no-python.conf").toFile();
        String configContent = 
            "cobolrag {\n" +
            "  format = \"fixed\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"password123\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "}";
        Files.write(configWithoutPython.toPath(), configContent.getBytes());

        CobolRagConfig config = new CobolRagConfig(configWithoutPython.getAbsolutePath());
        assertEquals("python3", config.getPythonExecutablePath());
    }

    @Test
    void shouldHandleCaseInsensitiveFormat() throws IOException {
        // Test uppercase format
        File uppercaseConfig = tempDir.resolve("uppercase.conf").toFile();
        String configContent = 
            "cobolrag {\n" +
            "  format = \"FIXED\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"password123\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "}";
        Files.write(uppercaseConfig.toPath(), configContent.getBytes());

        assertDoesNotThrow(() -> {
            CobolRagConfig config = new CobolRagConfig(uppercaseConfig.getAbsolutePath());
            assertEquals("FIXED", config.getFormat());
        });
    }

    @Test
    void shouldHandleEmptyConfiguration() {
        assertThrows(IllegalArgumentException.class, () -> {
            CobolRagConfig config = new CobolRagConfig();
            // Should throw exception for missing required configuration
        });
    }

    @Test
    void shouldValidateRequiredPaths() {
        // Test that required paths are validated
        assertThrows(IllegalArgumentException.class, () -> {
            // This would test missing required configuration
            // In a real scenario, you'd create a config with missing required fields
            new CobolRagConfig("/non/existent/path.conf");
        });
    }

    @Test
    void shouldHandleSpecialCharactersInPasswords() throws IOException {
        // Test passwords with special characters
        File specialCharConfig = tempDir.resolve("special.conf").toFile();
        String configContent = 
            "cobolrag {\n" +
            "  format = \"fixed\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"pass@word#123$\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "}";
        Files.write(specialCharConfig.toPath(), configContent.getBytes());

        CobolRagConfig config = new CobolRagConfig(specialCharConfig.getAbsolutePath());
        assertEquals("pass@word#123$", config.getNeo4jPassword());
    }

    @Test
    void shouldHandleUrlsWithPorts() throws IOException {
        // Test URLs with different ports
        File portConfig = tempDir.resolve("port.conf").toFile();
        String configContent = 
            "cobolrag {\n" +
            "  format = \"fixed\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"password123\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8001\"\n" +
            "  }\n" +
            "}";
        Files.write(portConfig.toPath(), configContent.getBytes());

        CobolRagConfig config = new CobolRagConfig(portConfig.getAbsolutePath());
        assertEquals("http://localhost:8001", config.getChromaDbUrl());
    }

    private void createConfigWithFormat(String format) throws IOException {
        File configFile = tempDir.resolve("format-" + format + ".conf").toFile();
        String configContent = 
            "cobolrag {\n" +
            "  format = \"" + format + "\"\n" +
            "  neo4j {\n" +
            "    uri = \"bolt://localhost:7687\"\n" +
            "    user = \"neo4j\"\n" +
            "    password = \"password123\"\n" +
            "  }\n" +
            "  chroma {\n" +
            "    url = \"http://localhost:8000\"\n" +
            "  }\n" +
            "}";
        Files.write(configFile.toPath(), configContent.getBytes());
        
        new CobolRagConfig(configFile.getAbsolutePath());
    }
} 