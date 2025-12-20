package view;

import model.*;
import repository.TextImportResult;
import util.Inputs;

import java.util.List;

public class ConsoleView {

    public void showBanner() {
        System.out.println("\n=== Лабораторные 4-5. Работа с файлами ===");
    }

    public int showMainMenu(String textDataFile) {
        System.out.println("1) Показать устройства (из памяти)");
        System.out.println("2) Добавить устройство и сохранить в CSV");
        System.out.println("3) Управлять устройством");
        System.out.println("4) Перечитать CSV и показать данные");
        System.out.println("5) Прочитать устройства из текстового файла (ЛР4)");
        System.out.println("6) Указать другой текстовый файл (сейчас: " + textDataFile + ")");
        System.out.println("0) Выход");
        return Inputs.readIntInRange("Выбор: ", 0, 6);
    }

    public void showDevices(List<LockDevice> devices, String title) {
        System.out.println("\n" + title);
        if (devices.isEmpty()) {
            System.out.println("Список пуст.");
            return;
        }
        for (LockDevice d : devices) {
            System.out.println("--------------------------------");
            System.out.println(d.statusInfo());
        }
    }

    public DeviceForm askNewDevice() {
        System.out.println("\nТип устройства:");
        System.out.println("1) RemoteControllableLock");
        System.out.println("2) BarrierGate");
        System.out.println("3) SafeLock");
        int type = Inputs.readIntInRange("Выбор: ", 1, 3);
        String manufacturer = Inputs.readString("Производитель: ");
        String model = Inputs.readString("Модель: ");
        String material = Inputs.readString("Материал: ");
        String key = Inputs.readString("Мастер-ключ/код: ");
        DeviceForm form = new DeviceForm(type, manufacturer, model, material, key);
        switch (type) {
            case 1 -> form.batteryLevel = Inputs.readIntInRange("Батарея (0..100): ", 0, 100);
            case 2 -> form.autoCloseSeconds = Inputs.readIntInRange("Автозакрытие, сек (0..120): ", 0, 120);
            case 3 -> form.combination = Inputs.readDigitsList(4);
            default -> {}
        }
        return form;
    }

    public int askDeviceId() {
        return Inputs.readInt("ID устройства: ");
    }

    public int askRemoteAction() {
        System.out.println("1) Remote Lock");
        System.out.println("2) Remote Unlock");
        System.out.println("3) Ping");
        System.out.println("4) Заменить батарею");
        System.out.println("0) Назад");
        return Inputs.readIntInRange("Выбор: ", 0, 4);
    }

    public int askGateAction() {
        System.out.println("1) Открыть");
        System.out.println("2) Закрыть");
        System.out.println("3) Переключить");
        System.out.println("4) Пропустить авто");
        System.out.println("0) Назад");
        return Inputs.readIntInRange("Выбор: ", 0, 4);
    }

    public int askSafeAction() {
        System.out.println("1) Ввести комбинацию");
        System.out.println("2) Сбросить блокировку админом");
        System.out.println("0) Назад");
        return Inputs.readIntInRange("Выбор: ", 0, 2);
    }

    public int askDeviceAction(LockDevice d) {
        System.out.println("\nРабота с: " + d.shortInfo());
        System.out.println("1) Запереть");
        System.out.println("2) Разблокировать");
        System.out.println("3) Проверить статус");
        System.out.println("4) Добавить ключ");
        System.out.println("5) Случайно изменить свойства");
        if (d instanceof RemoteControllableLock) {
            System.out.println("6) Удалённое управление");
        } else if (d instanceof BarrierGate) {
            System.out.println("6) Операции со шлагбаумом");
        } else if (d instanceof SafeLock) {
            System.out.println("6) Работа с комбинацией");
        }
        System.out.println("0) Назад");
        return Inputs.readIntInRange("Выбор: ", 0, 6);
    }

    public String askKeyValue() {
        return Inputs.readString("Ключ/код: ");
    }

    public List<Integer> askCombination(int size) {
        return Inputs.readDigitsList(size);
    }

    public String askFileName(String current) {
        System.out.println("Текущий файл: " + current);
        return Inputs.readString("Новый путь (можно относительный): ");
    }

    public void showImportSummary(String title, TextImportResult result) {
        System.out.println("\n" + title);
        System.out.printf("Источник: %s, строк прочитано: %d%n", result.source(), result.linesScanned());
        System.out.printf("Найдено объектов: %d, свойств прочитано: %d, свойств не найдено: %d%n",
                result.objectsFound(), result.propertiesRead(), result.propertiesMissing());
        if (!result.devices().isEmpty()) {
            System.out.println("--- Считанные объекты ---");
            showDevices(result.devices(), "");
        }
        result.messages().forEach(System.out::println);
    }

    public void showMessage(String text) {
        System.out.println(text);
    }

    public static class DeviceForm {
        public final int type;
        public final String manufacturer;
        public final String model;
        public final String material;
        public final String key;
        public int batteryLevel;
        public int autoCloseSeconds;
        public List<Integer> combination;

        public DeviceForm(int type, String manufacturer, String model, String material, String key) {
            this.type = type;
            this.manufacturer = manufacturer;
            this.model = model;
            this.material = material;
            this.key = key;
        }
    }
}
