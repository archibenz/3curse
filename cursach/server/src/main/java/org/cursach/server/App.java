package org.cursach.server;

import com.sun.net.httpserver.HttpServer;
import org.cursach.server.config.ServerConfig;
import org.cursach.server.db.Database;
import org.cursach.server.http.EventHandler;
import org.cursach.server.http.StaticFileHandler;
import org.cursach.server.logging.RequestLogger;

import java.io.IOException;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.OptionalInt;
import java.util.concurrent.Executors;

public class App {
    public static void main(String[] args) throws IOException {
        boolean healthcheck = Arrays.asList(args).contains("--healthcheck");

        ServerConfig config = ServerConfig.load(Path.of("config/server_config.json"));
        PortResolution portResolution = resolvePort(config, args);
        int port = portResolution.port();
        Database database = new Database(config.database());
        RequestLogger requestLogger = new RequestLogger(config.logFile(), database);

        if (healthcheck) {
            database.ensureSchema();
            System.out.println("Healthcheck passed. Tables are ready.");
            return;
        }

        HttpServer server;
        try {
            server = HttpServer.create(new InetSocketAddress(port), 0);
        } catch (BindException e) {
            if (portResolution.overridden()) {
                System.err.printf("Port %d is already in use. Change the port in config/server_config.json or pass --port=<value> to override.\n", port);
                System.exit(1);
                return;
            }

            System.err.printf("Port %d is busy. Falling back to an available port automatically.%n", port);
            server = HttpServer.create(new InetSocketAddress(0), 0);
        }
        server.createContext("/api/events", new EventHandler(database, requestLogger));
        server.createContext("/", new StaticFileHandler(Path.of(config.staticDir())));
        server.setExecutor(Executors.newFixedThreadPool(16));
        server.start();

        int actualPort = server.getAddress().getPort();
        System.out.printf("Server started on port %d. Open http://localhost:%d%n", actualPort, actualPort);
    }

    private static PortResolution resolvePort(ServerConfig config, String[] args) {
        OptionalInt override = Arrays.stream(args)
                .filter(arg -> arg.startsWith("--port="))
                .findFirst()
                .map(arg -> {
                    String value = arg.substring("--port=".length());
                    try {
                        return OptionalInt.of(Integer.parseInt(value));
                    } catch (NumberFormatException e) {
                        System.err.printf("Invalid port '%s'. Falling back to configured port %d.%n", value, config.port());
                        return OptionalInt.empty();
                    }
                })
                .orElseGet(OptionalInt::empty);

        return new PortResolution(override.orElse(config.port()), override.isPresent());
    }

    private record PortResolution(int port, boolean overridden) {}
}
