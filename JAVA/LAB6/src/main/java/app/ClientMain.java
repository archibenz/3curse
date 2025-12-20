package app;

import client.ClientController;
import client.DeviceClient;
import util.PropertyLoader;
import view.ClientView;

import java.nio.file.Path;
import java.util.Properties;

public class ClientMain {
    public static void main(String[] args) {
        Properties props = PropertyLoader.load(Path.of("config", "application.properties"));
        String host = props.getProperty("server.host", "127.0.0.1");
        int port = Integer.parseInt(props.getProperty("server.port", "9000"));

        DeviceClient client = new DeviceClient(host, port);
        ClientController controller = new ClientController(client, new ClientView());
        controller.run();
    }
}
