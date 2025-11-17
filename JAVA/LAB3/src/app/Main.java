package app;

import model.*;
import util.Inputs;

import java.util.*;

public class Main {

    private static final List<LockDevice> devices = new ArrayList<>();

    public static void main(String[] args) {
        while (true) {
            System.out.println("\n=== ЛР3: Абстрактный класс, наследование, полиморфизм ===");
            System.out.println("1) Добавить устройство");
            System.out.println("2) Удалить устройство");
            System.out.println("3) Изменить свойства устройства");
            System.out.println("4) Функциональная работа с устройством");
            System.out.println("5) Показать все устройства");
            System.out.println("6) Случайно изменить свойства устройства");
            System.out.println("7) PANIC MODE: мгновенно всё запереть/опустить");
            System.out.println("0) Выход");
            int cmd = Inputs.readIntInRange("Выбор: ", 0, 7);
            switch (cmd) {
                case 1 -> addDevice();
                case 2 -> removeDevice();
                case 3 -> editDevice();
                case 4 -> useDevice();
                case 5 -> printDevices();
                case 6 -> randomizeDevice();
                case 7 -> panicMode();
                case 0 -> {
                    System.out.println("Программа завершена");
                    return;
                }
                default -> System.out.println("Команда мимо.");
            }
        }
    }

    private static void addDevice() {
        System.out.println("\nТип устройства:");
        System.out.println("1) Замок с удалённым управлением");
        System.out.println("2) Шлагбаум");
        System.out.println("3) Замок для сейфа");
        int t = Inputs.readIntInRange("Выбор: ", 1, 3);

        String manufacturer = Inputs.readString("Производитель: ");
        String model = Inputs.readString("Модель: ");
        String material = Inputs.readString("Материал: ");
        String key = Inputs.readString("Начальный ключ (строка): ");

        switch (t) {
            case 1 -> {
                int battery = Inputs.readIntInRange("Заряд батареи (0..100): ", 0, 100);
                RemoteControllableLock r = new RemoteControllableLock(manufacturer, model, material, key, battery);
                devices.add(r);
                System.out.println("Добавлено: " + r.shortInfo());
            }
            case 2 -> {
                int autoClose = Inputs.readIntInRange("Автозакрытие, сек (0 чтобы выключить): ", 0, 120);
                BarrierGate g = new BarrierGate(manufacturer, model, material, key, autoClose);
                devices.add(g);
                System.out.println("Добавлено: " + g.shortInfo());
            }
            case 3 -> {
                SafeLock s = new SafeLock(manufacturer, model, material, key);
                System.out.println("Зададим комбинацию сейфа из 4 цифр (0..9).");
                List<Integer> comb = Inputs.readDigitsList(4);
                s.setCombination(comb);
                devices.add(s);
                System.out.println("Добавлено: " + s.shortInfo());
            }
            default -> System.out.println("Нет такого типа.");
        }
    }

    private static void removeDevice() {
        LockDevice d = pickDevice();
        if (d == null) return;
        devices.remove(d);
        System.out.println("Удалён: " + d.shortInfo());
    }

    private static void editDevice() {
        LockDevice d = pickDevice();
        if (d == null) return;

        System.out.println("\nЧто меняем у " + d.shortInfo() + " ?");
        System.out.println("1) Производитель");
        System.out.println("2) Модель");
        System.out.println("3) Материал");
        System.out.println("4) Заменить мастер-ключ");
        System.out.println("5) Типоспецифичные настройки");
        System.out.println("0) Назад");
        int c = Inputs.readInt("Выбор: ");
        switch (c) {
            case 1 -> d.setManufacturer(Inputs.readString("Новый производитель: "));
            case 2 -> d.setModel(Inputs.readString("Новая модель: "));
            case 3 -> d.setMaterial(Inputs.readString("Новый материал: "));
            case 4 -> d.replaceKey(Inputs.readString("Новый ключ: "));
            case 5 -> typeSpecificEdit(d);
            case 0 -> { return; }
            default -> System.out.println("Мимо.");
        }
        System.out.println("Теперь: " + d.statusInfo());
    }

    private static void typeSpecificEdit(LockDevice d) {
        if (d instanceof RemoteControllableLock r) {
            System.out.println("1) Установить уровень батареи");
            System.out.println("2) Подключить удалённо");
            System.out.println("3) Отключить удалённо");
            int x = Inputs.readInt("Выбор: ");
            switch (x) {
                case 1 -> r.setBatteryLevel(Inputs.readIntInRange("0..100: ", 0, 100));
                case 2 -> r.connectRemote();
                case 3 -> r.disconnectRemote();
                default -> System.out.println("Мимо.");
            }
        } else if (d instanceof BarrierGate g) {
            System.out.println("1) Установить автозакрытие (сек)");
            int x = Inputs.readInt("Выбор: ");
            if (x == 1) g.setAutoCloseSeconds(Inputs.readIntInRange("0..120: ", 0, 120));
            else System.out.println("Мимо.");
        } else if (d instanceof SafeLock s) {
            System.out.println("1) Сменить комбинацию");
            System.out.println("2) Сбросить блокировку после неверных попыток");
            int x = Inputs.readInt("Выбор: ");
            switch (x) {
                case 1 -> s.setCombination(Inputs.readDigitsList(4));
                case 2 -> s.adminResetLockout();
                default -> System.out.println("Мимо.");
            }
        }
    }

    private static void useDevice() {
        LockDevice d = pickDevice();
        if (d == null) return;

        while (true) {
            System.out.println("\nРабота с: " + d.shortInfo());
            System.out.println("1) Запереть");
            System.out.println("2) Разблокировать");
            System.out.println("3) Проверить, заблокирован ли");
            System.out.println("4) Проверить состояние и вывести информацию");
            System.out.println("5) Ключи: добавить/удалить/проверить наличие");
            if (d instanceof RemoteControllableLock) {
                System.out.println("6) Удалённое запирание/отпирание + ping");
            } else if (d instanceof BarrierGate) {
                System.out.println("6) Открыть/Закрыть/Пропустить авто");
            } else if (d instanceof SafeLock) {
                System.out.println("6) Ввести комбинацию");
            }
            System.out.println("0) Назад");
            int c = Inputs.readInt("Выбор: ");

            switch (c) {
                case 1 -> { d.lock(); System.out.println("Статус: " + (d.isLocked() ? "заперт" : "открыт")); }
                case 2 -> { d.unlock(); System.out.println("Статус: " + (d.isLocked() ? "заперт" : "открыт")); }
                case 3 -> System.out.println(d.isLocked() ? "Заперт" : "Открыт");
                case 4 -> System.out.println(d.statusInfo());
                case 5 -> keyOps(d);
                case 6 -> specificUse(d);
                case 0 -> { return; }
                default -> System.out.println("Мимо.");
            }
        }
    }

    private static void keyOps(LockDevice d) {
        System.out.println("1) Добавить ключ");
        System.out.println("2) Удалить ключ");
        System.out.println("3) Проверить наличие ключа");
        int k = Inputs.readInt("Выбор: ");
        String key = Inputs.readString("Ключ: ");
        switch (k) {
            case 1 -> d.addKey(key);
            case 2 -> d.removeKey(key);
            case 3 -> System.out.println(d.hasKey(key) ? "Есть" : "Нет");
            default -> System.out.println("Мимо.");
        }
    }

    private static void specificUse(LockDevice d) {
        if (d instanceof RemoteControllableLock r) {
            System.out.println("1) Remote Lock");
            System.out.println("2) Remote Unlock");
            System.out.println("3) Ping");
            System.out.println("4) Заменить батарею");
            int x = Inputs.readInt("Выбор: ");
            switch (x) {
                case 1 -> r.remoteLock();
                case 2 -> r.remoteUnlock();
                case 3 -> System.out.println(r.ping());
                case 4 -> r.replaceBattery();
                default -> System.out.println("Мимо.");
            }
        } else if (d instanceof BarrierGate g) {
            System.out.println("1) Открыть");
            System.out.println("2) Закрыть");
            System.out.println("3) Переключить");
            System.out.println("4) Пропустить автомобиль");
            int x = Inputs.readInt("Выбор: ");
            switch (x) {
                case 1 -> g.open();
                case 2 -> g.close();
                case 3 -> g.toggle();
                case 4 -> g.passVehicle();
                default -> System.out.println("Мимо.");
            }
        } else if (d instanceof SafeLock s) {
            System.out.println("Введите 4 цифры комбинации");
            List<Integer> comb = Inputs.readDigitsList(4);
            boolean ok = s.enterCombination(comb);
            System.out.println(ok ? "Комбинация верна." : "Комбинация неверна.");
        }
        System.out.println("Состояние: " + d.statusInfo());
    }

    private static void printDevices() {
        if (devices.isEmpty()) {
            System.out.println("Нечего показывать. Пусто как в студенческом холодильнике.");
            return;
        }
        devices.forEach(d -> {
            System.out.println("--------------------------------");
            System.out.println(d.statusInfo());
        });
    }

    private static void randomizeDevice() {
        LockDevice d = pickDevice();
        if (d == null) return;
        d.randomizeProperties();
        System.out.println("Случайные изменения применены.\n" + d.statusInfo());
    }

    private static void panicMode() {
        System.out.println("\nP A N I C   M O D E");
        for (LockDevice d : devices) {
            d.lock();
            if (d instanceof BarrierGate g) g.close();
        }
        System.out.println("Все устройства заперты/опущены. Успокаиваемся, дышим.");
    }

    private static LockDevice pickDevice() {
        if (devices.isEmpty()) {
            System.out.println("Список пуст.");
            return null;
        }
        devices.forEach(d -> System.out.println(d.getId() + ") " + d.shortInfo()));
        int id = Inputs.readInt("ID устройства: ");
        return devices.stream().filter(x -> x.getId() == id).findFirst().orElseGet(() -> {
            System.out.println("Не найдено.");
            return null;
        });
    }
}