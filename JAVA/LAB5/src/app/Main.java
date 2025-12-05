package app;

import controller.LockController;
import repository.CsvDeviceRepository;
import repository.DeviceRepository;
import view.ConsoleView;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Main {
    public static void main(String[] args) {
        Path csvPath = resolveCsvPath();
        DeviceRepository repository = new CsvDeviceRepository(csvPath);
        ConsoleView view = new ConsoleView();
        LockController controller = new LockController(repository, view);
        controller.run();
    }

    private static Path resolveCsvPath() {
        Path local = Paths.get("data", "devices.csv");
        if (Files.exists(local)) {
            return local;
        }

        Path repoRooted = Paths.get("JAVA", "LAB5", "data", "devices.csv");
        if (Files.exists(repoRooted)) {
            return repoRooted;
        }

        return local; // fall back to original relative path; CsvDeviceRepository will create it
    }
}
