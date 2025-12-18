package org.cursach.server.logging;

import org.cursach.server.db.Database;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class RequestLogger {
    private final Path logPath;
    private final Database database;
    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    public RequestLogger(String logPath, Database database) {
        this.logPath = Path.of(logPath);
        this.database = database;
        if (this.logPath.getParent() != null) {
            try {
                Files.createDirectories(this.logPath.getParent());
            } catch (IOException e) {
                throw new RuntimeException("Failed to create log directory", e);
            }
        }
    }

    public void log(String method, String path, int status, String payload) {
        String timestamp = LocalDateTime.now().format(formatter);
        String line = String.format("[%s] %s %s -> %d %s%n", timestamp, method, path, status, payload == null ? "" : payload);
        try {
            Files.writeString(logPath, line, Files.exists(logPath) ? java.nio.file.StandardOpenOption.APPEND : java.nio.file.StandardOpenOption.CREATE);
        } catch (IOException e) {
            System.err.println("Failed to write log file: " + e.getMessage());
        }
        database.logRequest(method, path, status, payload);
    }
}
