package org.cursach.server.http;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class StaticFileHandler implements HttpHandler {
    private final Path root;
    private final Map<String, String> mimeTypes = new HashMap<>();

    public StaticFileHandler(Path root) {
        this.root = root.toAbsolutePath();
        mimeTypes.put(".html", "text/html; charset=utf-8");
        mimeTypes.put(".css", "text/css; charset=utf-8");
        mimeTypes.put(".js", "application/javascript; charset=utf-8");
        mimeTypes.put(".json", "application/json; charset=utf-8");
    }

    @Override
    public void handle(HttpExchange exchange) throws IOException {
        String rawPath = exchange.getRequestURI().getPath();
        if (rawPath.equals("/")) {
            rawPath = "/index.html";
        }

        Path requested = root.resolve(rawPath.substring(1)).normalize();
        if (!requested.startsWith(root) || !Files.exists(requested) || Files.isDirectory(requested)) {
            requested = root.resolve("index.html");
        }

        byte[] content = Files.readAllBytes(requested);
        Headers headers = exchange.getResponseHeaders();
        headers.add("Content-Type", mimeTypes.getOrDefault(getExtension(requested), "text/plain; charset=utf-8"));
        exchange.sendResponseHeaders(200, content.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(content);
        }
    }

    private String getExtension(Path path) {
        String name = path.getFileName().toString();
        int idx = name.lastIndexOf('.');
        return idx == -1 ? "" : name.substring(idx);
    }
}
