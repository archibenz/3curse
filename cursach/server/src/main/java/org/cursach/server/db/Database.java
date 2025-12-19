package org.cursach.server.db;

import org.cursach.server.model.Event;

import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Database {
    private final Path dbPath;

    public Database(String dbPath) {
        this.dbPath = Path.of(dbPath);
        if (this.dbPath.getParent() != null) {
            try {
                Files.createDirectories(this.dbPath.getParent());
            } catch (Exception e) {
                throw new RuntimeException("Failed to create database directory", e);
            }
        }
        ensureSchema();
    }

    public void ensureSchema() {
        try (Connection connection = getConnection(); Statement statement = connection.createStatement()) {
            statement.execute(
                "CREATE TABLE IF NOT EXISTS events (" +
                    "id INTEGER PRIMARY KEY AUTOINCREMENT," +
                    "title TEXT NOT NULL," +
                    "description TEXT DEFAULT ''," +
                    "status TEXT DEFAULT 'planned'," +
                    "created_at TEXT DEFAULT (datetime('now'))" +
                ")"
            );
            statement.execute(
                "CREATE TABLE IF NOT EXISTS request_logs (" +
                    "id INTEGER PRIMARY KEY AUTOINCREMENT," +
                    "method TEXT NOT NULL," +
                    "path TEXT NOT NULL," +
                    "status INTEGER NOT NULL," +
                    "payload TEXT," +
                    "created_at TEXT DEFAULT (datetime('now'))" +
                ")"
            );
        } catch (SQLException e) {
            throw new RuntimeException("Failed to initialize schema", e);
        }
    }

    private Connection getConnection() throws SQLException {
        return DriverManager.getConnection("jdbc:sqlite:" + dbPath.toAbsolutePath());
    }

    public List<Event> listEvents() {
        try (Connection connection = getConnection();
             PreparedStatement statement = connection.prepareStatement(
                 "SELECT id, title, description, status, created_at FROM events ORDER BY id DESC")) {
            ResultSet rs = statement.executeQuery();
            List<Event> events = new ArrayList<>();
            while (rs.next()) {
                events.add(mapEvent(rs));
            }
            return events;
        } catch (SQLException e) {
            throw new RuntimeException("Failed to list events", e);
        }
    }

    public Optional<Event> getEvent(int id) {
        try (Connection connection = getConnection();
             PreparedStatement statement = connection.prepareStatement(
                 "SELECT id, title, description, status, created_at FROM events WHERE id = ?")) {
            statement.setInt(1, id);
            ResultSet rs = statement.executeQuery();
            if (rs.next()) {
                return Optional.of(mapEvent(rs));
            }
            return Optional.empty();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to fetch event", e);
        }
    }

    public Event createEvent(String title, String description, String status) {
        try (Connection connection = getConnection();
             PreparedStatement statement = connection.prepareStatement(
                 "INSERT INTO events (title, description, status) VALUES (?, ?, ?)",
                 Statement.RETURN_GENERATED_KEYS)) {
            statement.setString(1, title);
            statement.setString(2, description);
            statement.setString(3, status);
            statement.executeUpdate();
            try (ResultSet rs = statement.getGeneratedKeys()) {
                if (rs.next()) {
                    int id = rs.getInt(1);
                    return getEvent(id).orElseThrow();
                }
            }
            throw new RuntimeException("Failed to retrieve created event");
        } catch (SQLException e) {
            throw new RuntimeException("Failed to create event", e);
        }
    }

    public Optional<Event> updateEvent(int id, String title, String description, String status) {
        Event existing = getEvent(id).orElse(null);
        if (existing == null) {
            return Optional.empty();
        }
        String newTitle = title != null ? title : existing.title();
        String newDescription = description != null ? description : existing.description();
        String newStatus = status != null ? status : existing.status();

        try (Connection connection = getConnection();
             PreparedStatement statement = connection.prepareStatement(
                 "UPDATE events SET title = ?, description = ?, status = ? WHERE id = ?")) {
            statement.setString(1, newTitle);
            statement.setString(2, newDescription);
            statement.setString(3, newStatus);
            statement.setInt(4, id);
            statement.executeUpdate();
            return getEvent(id);
        } catch (SQLException e) {
            throw new RuntimeException("Failed to update event", e);
        }
    }

    public boolean deleteEvent(int id) {
        try (Connection connection = getConnection();
             PreparedStatement statement = connection.prepareStatement("DELETE FROM events WHERE id = ?")) {
            statement.setInt(1, id);
            return statement.executeUpdate() > 0;
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete event", e);
        }
    }

    public void logRequest(String method, String path, int status, String payload) {
        try (Connection connection = getConnection();
             PreparedStatement statement = connection.prepareStatement(
                 "INSERT INTO request_logs (method, path, status, payload) VALUES (?, ?, ?, ?)")
        ) {
            statement.setString(1, method);
            statement.setString(2, path);
            statement.setInt(3, status);
            statement.setString(4, payload);
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to log request", e);
        }
    }

    private Event mapEvent(ResultSet rs) throws SQLException {
        return new Event(
            rs.getInt("id"),
            rs.getString("title"),
            rs.getString("description"),
            rs.getString("status"),
            rs.getString("created_at")
        );
    }
}
