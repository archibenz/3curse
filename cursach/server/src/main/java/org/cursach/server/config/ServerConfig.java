package org.cursach.server.config;

import com.google.gson.Gson;
import com.google.gson.JsonIOException;
import com.google.gson.JsonSyntaxException;
import com.google.gson.annotations.SerializedName;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;

public record ServerConfig(
        int port,
        @SerializedName(value = "database") String database,
        @SerializedName(value = "log_file") String logFile,
        @SerializedName(value = "static_dir") String staticDir
) {
    public static ServerConfig load(Path configPath) {
        Gson gson = new Gson();
        try (Reader reader = Files.newBufferedReader(configPath)) {
            ServerConfig config = gson.fromJson(reader, ServerConfig.class);
            if (config == null) {
                throw new IllegalStateException("Config is empty");
            }
            if (config.database == null || config.logFile == null || config.staticDir == null) {
                throw new IllegalStateException("Config is missing required fields: database, log_file, static_dir");
            }
            return config;
        } catch (IOException | JsonIOException | JsonSyntaxException e) {
            throw new RuntimeException("Failed to read config " + configPath.toAbsolutePath(), e);
        }
    }
}
