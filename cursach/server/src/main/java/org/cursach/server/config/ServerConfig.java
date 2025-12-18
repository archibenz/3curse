package org.cursach.server.config;

import com.google.gson.Gson;
import com.google.gson.JsonIOException;
import com.google.gson.JsonSyntaxException;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;

public record ServerConfig(int port, String database, String logFile, String staticDir) {
    public static ServerConfig load(Path configPath) {
        Gson gson = new Gson();
        try (Reader reader = Files.newBufferedReader(configPath)) {
            ServerConfig config = gson.fromJson(reader, ServerConfig.class);
            if (config == null) {
                throw new IllegalStateException("Config is empty");
            }
            return config;
        } catch (IOException | JsonIOException | JsonSyntaxException e) {
            throw new RuntimeException("Failed to read config " + configPath.toAbsolutePath(), e);
        }
    }
}
