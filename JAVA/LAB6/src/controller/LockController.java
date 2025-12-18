package controller;

import model.*;
import repository.DeviceRepository;
import view.ConsoleView;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class LockController {

    private final DeviceRepository repository;
    private final ConsoleView view;
    private List<LockDevice> devices;

    public LockController(DeviceRepository repository, ConsoleView view) {
        this.repository = repository;
        this.view = view;
        this.textImporter = new TextDeviceImporter();
        this.textDataPath = Path.of("data", "devices.txt");
        this.devices = new ArrayList<>(repository.loadDevices());
    }

    public void run() {
        view.showBanner();
        boolean running = true;
        while (running) {
            int cmd = view.showMainMenu(textDataPath.toString());
            switch (cmd) {
                case 1 -> view.showDevices(devices, "Устройства в памяти");
                case 2 -> addDevice();
                case 3 -> controlDevice();
                case 4 -> reloadFromFile();
                case 5 -> importFromText();
                case 6 -> changeTextFile();
                case 0 -> running = false;
                default -> view.showMessage("Неизвестная команда.");
            }
        }
        repository.saveDevices(devices);
        view.showMessage("Программа завершена, данные сохранены.");
    }

    private void addDevice() {
        ConsoleView.DeviceForm form = view.askNewDevice();
        LockDevice device = switch (form.type) {
            case 1 -> new RemoteControllableLock(form.manufacturer, form.model, form.material, form.key, form.batteryLevel);
            case 2 -> new BarrierGate(form.manufacturer, form.model, form.material, form.key, form.autoCloseSeconds);
            case 3 -> {
                SafeLock s = new SafeLock(form.manufacturer, form.model, form.material, form.key);
                s.setCombination(form.combination);
                yield s;
            }
            default -> null;
        };
        if (device != null) {
            devices.add(device);
            repository.saveDevices(devices);
            view.showMessage("Добавлено: " + device.statusInfo());
        }
    }

    private void controlDevice() {
        if (devices.isEmpty()) {
            view.showMessage("Список пуст.");
            return;
        }
        view.showDevices(devices, "Текущие устройства");
        int id = view.askDeviceId();
        Optional<LockDevice> targetOpt = devices.stream().filter(d -> d.getId() == id).findFirst();
        if (targetOpt.isEmpty()) {
            view.showMessage("Не найдено.");
            return;
        }
        LockDevice d = targetOpt.get();
        boolean back = false;
        while (!back) {
            int action = view.askDeviceAction(d);
            switch (action) {
                case 1 -> d.lock();
                case 2 -> d.unlock();
                case 3 -> view.showMessage(d.statusInfo());
                case 4 -> d.addKey(view.askKeyValue());
                case 5 -> d.randomizeProperties();
                case 6 -> specificAction(d);
                case 0 -> back = true;
                default -> view.showMessage("Мимо.");
            }
            repository.saveDevices(devices);
        }
    }

    private void specificAction(LockDevice d) {
        if (d instanceof RemoteControllableLock r) {
            int a = view.askRemoteAction();
            switch (a) {
                case 1 -> r.remoteLock();
                case 2 -> r.remoteUnlock();
                case 3 -> view.showMessage(r.ping());
                case 4 -> r.replaceBattery();
                default -> {}
            }
        } else if (d instanceof BarrierGate g) {
            int a = view.askGateAction();
            switch (a) {
                case 1 -> g.open();
                case 2 -> g.close();
                case 3 -> g.toggle();
                case 4 -> g.passVehicle();
                default -> {}
            }
        } else if (d instanceof SafeLock s) {
            int a = view.askSafeAction();
            switch (a) {
                case 1 -> {
                    boolean ok = s.enterCombination(view.askCombination(4));
                    view.showMessage(ok ? "Комбинация верна" : "Комбинация неверна/блокировка");
                }
                case 2 -> s.adminResetLockout();
                default -> {}
            }
        }
    }

    private void reloadFromFile() {
        devices = new ArrayList<>(repository.loadDevices());
        view.showDevices(devices, "Данные после чтения CSV");
    }

    private void importFromText() {
        TextImportResult result = textImporter.importFile(textDataPath);
        if (!result.devices().isEmpty()) {
            devices.addAll(result.devices());
            repository.saveDevices(devices);
        }
        view.showImportSummary("Чтение текстового файла", result);
    }

    private void changeTextFile() {
        String newName = view.askFileName(textDataPath.toString()).trim();
        if (newName.isEmpty()) {
            view.showMessage("Путь не изменён.");
            return;
        }
        textDataPath = Path.of(newName);
        view.showMessage("Теперь будет использоваться файл: " + textDataPath);
    }
}
