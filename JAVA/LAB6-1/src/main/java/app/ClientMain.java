package app;

import client.DeviceClient;
import util.PropertyLoader;

import java.nio.file.Path;
import java.util.Properties;

public class ClientMain {

    public static void main(String[] args) {
        String configPath = args.length > 0 ? args[0] : "config/application.properties";
        Properties properties = PropertyLoader.load(Path.of(configPath));
        String host = properties.getProperty("server.host", "127.0.0.1");
        int port = Integer.parseInt(properties.getProperty("server.port", "5000"));

        try {
            DeviceClient client = new DeviceClient(host, port);
            String response = client.requestDevices();
            if (response == null) {
                System.out.println("Сервер не ответил.");
            } else {
                System.out.println(response);
            }
        } catch (Exception e) {
            System.out.println("Ошибка клиента: " + e.getMessage());
        }
    }
}
