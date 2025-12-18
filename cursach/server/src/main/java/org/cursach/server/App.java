package org.cursach.server;

import com.sun.net.httpserver.HttpServer;
import org.cursach.server.config.ServerConfig;
import org.cursach.server.db.Database;
import org.cursach.server.http.EventHandler;
import org.cursach.server.http.StaticFileHandler;
import org.cursach.server.logging.RequestLogger;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.concurrent.Executors;

public class App {
    public static void main(String[] args) throws IOException {
        boolean healthcheck = Arrays.asList(args).contains("--healthcheck");

        ServerConfig config = ServerConfig.load(Path.of("config/server_config.json"));
        Database database = new Database(config.database());
        RequestLogger requestLogger = new RequestLogger(config.logFile(), database);

        if (healthcheck) {
            database.ensureSchema();
            System.out.println("Healthcheck passed. Tables are ready.");
            return;
        }

        HttpServer server = HttpServer.create(new InetSocketAddress(config.port()), 0);
        server.createContext("/api/events", new EventHandler(database, requestLogger));
        server.createContext("/", new StaticFileHandler(Path.of(config.staticDir())));
        server.setExecutor(Executors.newFixedThreadPool(16));
        server.start();

        System.out.printf("Server started on port %d. Open http://localhost:%d%n", config.port(), config.port());
    }
}
