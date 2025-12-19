package controller;

import devices.WearableDevice;
import fileio.FileReader;
import fileio.CsvLogger;
import view.ConsoleView;
import fileio.CsvWatcher;

import java.io.IOException;
import java.nio.file.*;
import java.util.List;

public class DeviceController {

    private final ConsoleView view;

    public DeviceController(ConsoleView view) {
        this.view = view;
    }

    public void run(String[] args) {
        if (args.length == 0 || "--help".equals(args[0])) {
            view.showHelp();
            return;
        }

        switch (args[0]) {
            case "--once"    -> handleOnce(args);
            case "--replace" -> handleReplace(args);
            case "--watch"   -> handleWatch(args);
            case "--csv"     -> handleCsv(args);
            default          -> view.showError("Неизвестный ключ: " + args[0]);
        }
    }

    private List<WearableDevice> readDevices(String fileName) throws IOException {
        FileReader reader = new FileReader(fileName);
        List<WearableDevice> devices = reader.readFile();
        reader.printStatistics(); // вывод статистики можно тоже передать во view
        return devices;
    }

    private void handleOnce(String[] args) {
        if (args.length < 2) {
            view.showError("Укажите файл: --once <file>");
            return;
        }
        try {
            List<WearableDevice> devices = readDevices(args[1]);
            view.showDevices(devices);
        } catch (IOException e) {
            view.showError("Не удалось прочитать файл: " + e.getMessage());
        }
    }

    private void handleReplace(String[] args) {
        if (args.length < 3) {
            view.showError("Использование: --replace <from> <to>");
            return;
        }
        try {
            Files.copy(Path.of(args[1]), Path.of(args[2]), StandardCopyOption.REPLACE_EXISTING);
            view.showFileReplaced(args[1], args[2]);
        } catch (IOException e) {
            view.showError("Ошибка при замене файла: " + e.getMessage());
        }
    }

    private void handleWatch(String[] args) {
        if (args.length < 4 || !"--interval".equals(args[2])) {
            view.showError("Использование: --watch <file> --interval <сек>");
            return;
        }
        String file = args[1];
        long intervalMs = Long.parseLong(args[3]) * 1000L;
        while (true) {
            try {
                List<WearableDevice> devices = readDevices(file);
                view.showDevices(devices);
                Thread.sleep(intervalMs);
            } catch (IOException e) {
                view.showError("Ошибка чтения: " + e.getMessage());
                break;
            } catch (InterruptedException e) {
                break;
            }
        }
    }

    private void handleCsv(String[] args) {
        if (args.length < 6) {
            view.showError(
                    "Использование: --csv <file> --out svFile> --interval <сек>");
            return;
        }
        String file = args[1];
        String out = args[3];
        long intervalMs = Long.parseLong(args[5]) * 1000L;

        try (CsvLogger logger = new CsvLogger(out)) {
            CsvWatcher watcher = new CsvWatcher(file, logger);
            watcher.loop(intervalMs);
        } catch (IOException e) {
            view.showError("Ошибка CSV‑логгера: " + e.getMessage());
        }
    }
}
