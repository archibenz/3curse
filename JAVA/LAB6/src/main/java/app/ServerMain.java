package app;

import repository.CsvDeviceRepository;
import repository.DeviceRepository;
import server.DeviceServer;
import util.PropertyLoader;

import java.nio.file.Path;
import java.util.Properties;

public class ServerMain {
    public static void main(String[] args) throws Exception {
        Properties props = PropertyLoader.load(Path.of("config", "server.properties"));
        int port = Integer.parseInt(props.getProperty("server.port", "9000"));

        Path csvPath = Path.of("data", "devices.csv");
        DeviceRepository repository = new CsvDeviceRepository(csvPath);

        DeviceServer server = new DeviceServer(port, repository);
        server.start();
    }
}
