package org.cursach.server.http;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonSyntaxException;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import org.cursach.server.db.Database;
import org.cursach.server.logging.RequestLogger;
import org.cursach.server.model.Event;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;

public class EventHandler implements HttpHandler {
    private final Database database;
    private final RequestLogger requestLogger;
    private final Gson gson = new Gson();

    public EventHandler(Database database, RequestLogger requestLogger) {
        this.database = database;
        this.requestLogger = requestLogger;
    }

    @Override
    public void handle(HttpExchange exchange) throws IOException {
        String method = exchange.getRequestMethod();
        String path = exchange.getRequestURI().getPath();
        String body = readBody(exchange);
        int status = 500;
        try {
            if (path.equals("/api/events") || path.equals("/api/events/")) {
                status = handleCollection(exchange, method, body);
            } else if (path.matches("/api/events/\\d+/?")) {
                int id = Integer.parseInt(path.replaceAll("[^0-9]", ""));
                status = handleSingle(exchange, method, id, body);
            } else {
                sendJson(exchange, 404, new Message("Not found"));
                status = 404;
            }
        } catch (Exception ex) {
            sendJson(exchange, 500, new Message("Server error: " + ex.getMessage()));
            status = 500;
        } finally {
            requestLogger.log(method, path, status, body);
        }
    }

    private int handleCollection(HttpExchange exchange, String method, String body) throws IOException {
        switch (method) {
            case "GET" -> {
                List<Event> events = database.listEvents();
                sendJson(exchange, 200, events);
                return 200;
            }
            case "POST" -> {
                JsonObject payload = parseJson(body, exchange);
                if (payload == null) return 400;
                if (!payload.has("title") || payload.get("title").getAsString().isBlank()) {
                    sendJson(exchange, 400, new Message("Title is required"));
                    return 400;
                }
                String title = payload.get("title").getAsString();
                String description = payload.has("description") ? payload.get("description").getAsString() : "";
                String status = payload.has("status") ? payload.get("status").getAsString() : "planned";
                Event event = database.createEvent(title, description, status);
                sendJson(exchange, 201, event);
                return 201;
            }
            default -> {
                sendJson(exchange, 405, new Message("Method not allowed"));
                return 405;
            }
        }
    }

    private int handleSingle(HttpExchange exchange, String method, int id, String body) throws IOException {
        switch (method) {
            case "GET" -> {
                Optional<Event> event = database.getEvent(id);
                if (event.isPresent()) {
                    sendJson(exchange, 200, event.get());
                    return 200;
                }
                sendJson(exchange, 404, new Message("Event not found"));
                return 404;
            }
            case "PUT" -> {
                JsonObject payload = parseJson(body, exchange);
                if (payload == null) return 400;
                String title = payload.has("title") ? payload.get("title").getAsString() : null;
                String description = payload.has("description") ? payload.get("description").getAsString() : null;
                String status = payload.has("status") ? payload.get("status").getAsString() : null;
                Optional<Event> updated = database.updateEvent(id, title, description, status);
                if (updated.isPresent()) {
                    sendJson(exchange, 200, updated.get());
                    return 200;
                }
                sendJson(exchange, 404, new Message("Event not found"));
                return 404;
            }
            case "DELETE" -> {
                boolean removed = database.deleteEvent(id);
                if (removed) {
                    sendJson(exchange, 204, null);
                    return 204;
                }
                sendJson(exchange, 404, new Message("Event not found"));
                return 404;
            }
            default -> {
                sendJson(exchange, 405, new Message("Method not allowed"));
                return 405;
            }
        }
    }

    private JsonObject parseJson(String body, HttpExchange exchange) throws IOException {
        try {
            return gson.fromJson(body, JsonObject.class);
        } catch (JsonSyntaxException ex) {
            sendJson(exchange, 400, new Message("Invalid JSON"));
            return null;
        }
    }

    private String readBody(HttpExchange exchange) throws IOException {
        try (InputStreamReader reader = new InputStreamReader(exchange.getRequestBody(), StandardCharsets.UTF_8)) {
            StringBuilder sb = new StringBuilder();
            char[] buffer = new char[1024];
            int read;
            while ((read = reader.read(buffer)) != -1) {
                sb.append(buffer, 0, read);
            }
            return sb.toString();
        }
    }

    private void sendJson(HttpExchange exchange, int statusCode, Object payload) throws IOException {
        byte[] response = payload == null ? new byte[0] : gson.toJson(payload).getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json; charset=utf-8");
        exchange.sendResponseHeaders(statusCode, response.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(response);
        }
    }

    private record Message(String message) {}
}
