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

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class CobolRagConfig {
    private static final Logger logger = LoggerFactory.getLogger(CobolRagConfig.class);

    private final Config config;

    public CobolRagConfig(String configPath) {
        File configFile = new File(configPath);

        if (!configFile.exists()) {
            logger.warn("Configuration file not found: {}. Using default configuration.", configPath);
            this.config = ConfigFactory.load();
        } else {
            logger.info("Loading configuration from: {}", configPath);
            this.config = ConfigFactory.parseFile(configFile).resolve();
        }

        // Validate configuration
        validate();
    }

    public CobolRagConfig() {
        this.config = ConfigFactory.load();
        validate();
    }

    private void validate() {
        // Validate format
        if (config.hasPath("cobolrag.format")) {
            String formatStr = config.getString("cobolrag.format");
            if (!formatStr.equalsIgnoreCase("fixed") && !formatStr.equalsIgnoreCase("tandem")) {
                throw new IllegalArgumentException("Invalid configuration for cobolrag.format " + formatStr);
            }
        }

        // Ensure required paths exist
        requiredPath("cobolrag.neo4j.uri");
        requiredPath("cobolrag.neo4j.user");
        requiredPath("cobolrag.neo4j.password");
        requiredPath("cobolrag.chroma.url");
        
        // Python path is optional for embedding service
        if (!config.hasPath("cobolrag.python.path")) {
            logger.warn("cobolrag.python.path not configured, using system default");
        }
    }

    private void requiredPath(String path) {
        if (!config.hasPath(path)) {
            throw new IllegalArgumentException("Missing required configuration: " + path);
        }
    }

    public String getNeo4jUri() {
        return config.getString("cobolrag.neo4j.uri");
    }

    public String getNeo4jUser() {
        return config.getString("cobolrag.neo4j.user");
    }

    public String getNeo4jPassword() {
        return config.getString("cobolrag.neo4j.password");
    }

    public String getChromaDbUrl() {
        return config.getString("cobolrag.chroma.url");
    }

    public String getPythonExecutablePath() {
        return config.hasPath("cobolrag.python.path") ? 
            config.getString("cobolrag.python.path") : "python3";
    }

    public String getFormat() {
        return config.getString("cobolrag.format");
    }

    // Add more getters for other configuration properties as needed
}
