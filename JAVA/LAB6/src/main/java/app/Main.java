package app;

import controller.LockController;
import repository.CsvDeviceRepository;
import repository.DeviceRepository;
import view.ConsoleView;

import java.nio.file.Path;

public class Main {
    public static void main(String[] args) {
        Path csvPath = Path.of("data", "devices.csv");
        DeviceRepository repository = new CsvDeviceRepository(csvPath);
        ConsoleView view = new ConsoleView();
        LockController controller = new LockController(repository, view);
        controller.run();
    }
}
