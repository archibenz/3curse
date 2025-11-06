package app;

import model.SmartLock;
import util.Config;

import java.util.Arrays;
import java.util.Scanner;

public class Main {
    private static int nextId = 1;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        SmartLock[] locks = new SmartLock[0];
        boolean running = true;

        while (running) {
            printMenu();
            System.out.print("Выберите пункт: ");
            String choice = sc.nextLine().trim();

            switch (choice) {
                case "1":
                    locks = appendAll(locks, createInitialObjects());
                    System.out.println("Создано " + locks.length + " объектов и объединено в массив.");
                    break;

                case "2":
                    showAll(locks);
                    break;

                case "3":
                    if (locks.length == 0) { System.out.println("Массив пуст."); break; }
                    int idx = readInt(sc, "Индекс объекта (1.." + locks.length + "): ") - 1;
                    if (!isValidIndex(idx, locks.length)) { System.out.println("Нет такого индекса."); break; }
                    editLock(sc, locks[idx]);
                    System.out.println("Изменения сохранены.");
                    break;

                case "4":
                    SmartLock newLock = inputLock(sc);
                    locks = add(locks, newLock);
                    System.out.println("Объект добавлен. Размер массива: " + locks.length);
                    break;

                case "5":
                    if (locks.length == 0) { System.out.println("Массив пуст."); break; }
                    int del = readInt(sc, "Индекс для удаления (1.." + locks.length + "): ") - 1;
                    if (!isValidIndex(del, locks.length)) { System.out.println("Нет такого индекса."); break; }
                    locks = remove(locks, del);
                    System.out.println("Удалено. Размер массива: " + locks.length);
                    break;

                case "6":
                    if (locks.length == 0) { System.out.println("Массив пуст."); break; }
                    double avg = computeAverageServiceLife(locks);
                    System.out.printf("Средний срок эксплуатации: %.2f лет%n", avg);
                    break;

                case "0":
                    running = false;
                    System.out.println("Выход.");
                    break;

                default:
                    System.out.println("Нет такого пункта.");
            }
        }
        sc.close();
    }

    private static void printMenu() {
        System.out.println("\nМеню:");
        System.out.println("1. Создать несколько объектов и объединить их в массив");
        System.out.println("2. Показать массив (вывод toString в цикле)");
        System.out.println("3. Изменить объект по индексу");
        System.out.println("4. Добавить объект в массив");
        System.out.println("5. Удалить объект из массива");
        System.out.println("6. Вычислить средний срок эксплуатации");
        System.out.println("0. Выход");
    }

    private static SmartLock[] createInitialObjects() {
        SmartLock l1 = new SmartLock();
        l1.setId(nextId++);
        l1.setBrand("Xiaomi");
        l1.setModel("Aqara S2");
        l1.setProductionYear(2021);
        l1.setProtocol("Zigbee");
        l1.setBatteryHealth(Config.DEFAULT_BATTERY_HEALTH);
        l1.setLocked(true);
        l1.setServiceLifeYears(l1.yearsInUse(Config.CURRENT_YEAR));

        SmartLock l2 = new SmartLock(
                nextId++, "Yale", "Linus", 2022,
                "Wi-Fi", 100, false, Config.CURRENT_YEAR - 2022
        );

        SmartLock l3 = new SmartLock(
                nextId++, "August", "Smart Lock Pro", 2019,
                "Bluetooth", 80, true, Config.CURRENT_YEAR - 2019
        );

        return new SmartLock[] { l1, l2, l3 };
    }

    private static void showAll(SmartLock[] arr) {
        if (arr.length == 0) { System.out.println("Массив пуст."); return; }
        for (int i = 0; i < arr.length; i++) {
            System.out.println((i + 1) + ") " + arr[i]);
        }
    }

    private static SmartLock[] add(SmartLock[] arr, SmartLock item) {
        SmartLock[] res = Arrays.copyOf(arr, arr.length + 1);
        res[arr.length] = item;
        return res;
    }

    private static SmartLock[] appendAll(SmartLock[] arr, SmartLock[] items) {
        SmartLock[] res = Arrays.copyOf(arr, arr.length + items.length);
        System.arraycopy(items, 0, res, arr.length, items.length);
        return res;
    }

    private static SmartLock[] remove(SmartLock[] arr, int idx) {
        SmartLock[] res = new SmartLock[arr.length - 1];
        int r = 0;
        for (int i = 0; i < arr.length; i++) {
            if (i == idx) continue;
            res[r++] = arr[i];
        }
        return res;
    }

    private static void editLock(Scanner sc, SmartLock lock) {
        boolean done = false;
        while (!done) {
            System.out.println("\nРедактирование: " + lock);
            System.out.println("1. Бренд"); 
            System.out.println("2. Модель");
            System.out.println("3. Год выпуска");
            System.out.println("4. Протокол");
            System.out.println("5. Состояние батареи");
            System.out.println("6. Состояние: заперт/открыт");
            System.out.println("7. Срок эксплуатации (лет)");
            System.out.println("8. Заблокировать");
            System.out.println("9. Разблокировать");
            System.out.println("0. Назад");
            System.out.print("Выберите: ");
            String ch = sc.nextLine().trim();

            switch (ch) {
                case "1": System.out.print("Новое значение: "); lock.setBrand(sc.nextLine().trim()); break;
                case "2": System.out.print("Новое значение: "); lock.setModel(sc.nextLine().trim()); break;
                case "3": lock.setProductionYear(readInt(sc, "Новый год выпуска: ")); lock.setServiceLifeYears(lock.yearsInUse(Config.CURRENT_YEAR)); break;
                case "4": System.out.print("Новый протокол: "); lock.setProtocol(sc.nextLine().trim()); break;
                case "5": lock.setBatteryHealth(clamp(readInt(sc, "Новое состояние батареи (0..100): "), 0, 100)); break;
                case "6": lock.setLocked(readBoolean(sc, "Заперт? (y/n): ")); break;
                case "7": lock.setServiceLifeYears(lock.yearsInUse(Config.CURRENT_YEAR)); System.out.println("Срок эксплуатации пересчитан автоматически."); break;
                case "8": lock.lock(); break;
                case "9": lock.unlock(); break;
                case "0": done = true; break;
                default: System.out.println("Нет такого пункта.");
            }
        }
    }

    private static double computeAverageServiceLife(SmartLock[] arr) {
        double sum = 0;
        for (SmartLock l : arr) sum += l.getServiceLifeYears();
        return sum / arr.length;
    }

    private static SmartLock inputLock(Scanner sc) {
        int id = nextId++;
        System.out.println("Создание нового замка (id=" + id + ")");
        String brand = readNonEmptyString(sc, "Бренд: ");
        String model = readNonEmptyString(sc, "Модель: ");
        int year = readYear(sc, "Год выпуска: ");
        String protocol = readProtocol(sc, "Протокол (Wi-Fi/Bluetooth/Zigbee): ");
        int battery = readIntInRange(sc, "Состояние батареи (0..100): ", 0, 100);
        boolean locked = readBoolean(sc, "Сейчас заперт? (y/n): ");
        double life = (double)(Config.CURRENT_YEAR - year);
        return new SmartLock(id, brand, model, year, protocol, battery, locked, life);
    }

    private static int readInt(Scanner sc, String prompt) {
        while (true) {
            System.out.print(prompt);
            String s = sc.nextLine().trim();
            try { return Integer.parseInt(s); }
            catch (NumberFormatException e) { System.out.println("Введите целое число."); }
        }
    }

    private static double readDouble(Scanner sc, String prompt) {
        while (true) {
            System.out.print(prompt);
            String s = sc.nextLine().replace(',', '.').trim();
            try { return Double.parseDouble(s); }
            catch (NumberFormatException e) { System.out.println("Введите число."); }
        }
    }

    private static boolean readBoolean(Scanner sc, String prompt) {
        while (true) {
            System.out.print(prompt);
            String s = sc.nextLine().trim().toLowerCase();
            if (s.equals("y") || s.equals("д") || s.equals("да") || s.equals("true")) return true;
            if (s.equals("n") || s.equals("н") || s.equals("нет") || s.equals("false")) return false;
            System.out.println("Ответьте 'y'/'n' или 'да'/'нет'.");
        }
    }

    private static boolean isValidIndex(int idx, int len) {
        return idx >= 0 && idx < len;
    }

    private static int clamp(int val, int min, int max) {
        return Math.max(min, Math.min(max, val));
    }

    private static String readNonEmptyString(Scanner sc, String prompt) {
        while (true) {
            System.out.print(prompt);
            String s = sc.nextLine().trim();
            if (!s.isEmpty()) return s;
            System.out.println("Значение не может быть пустым.");
        }
    }

    private static int readIntInRange(Scanner sc, String prompt, int min, int max) {
        while (true) {
            int v = readInt(sc, prompt);
            if (v >= min && v <= max) return v;
            System.out.println("Введите число в диапазоне " + min + ".." + max + ".");
        }
    }

    private static int readYear(Scanner sc, String prompt) {
        int current = Config.CURRENT_YEAR;
        while (true) {
            int y = readInt(sc, prompt);
            if (y >= 1900 && y <= current) return y;
            System.out.println("Год должен быть в диапазоне 1900.." + current + ".");
        }
    }

    private static String readProtocol(Scanner sc, String prompt) {
        while (true) {
            System.out.print(prompt);
            String p = sc.nextLine().trim();
            String pl = p.toLowerCase();
            if (pl.equals("wi-fi") || pl.equals("wifi") || pl.equals("bluetooth") || pl.equals("zigbee")) {
                if (pl.equals("wifi")) return "Wi-Fi";
                if (pl.equals("bluetooth")) return "Bluetooth";
                if (pl.equals("zigbee")) return "Zigbee";
                return "Wi-Fi";
            }
            System.out.println("Введите одно из значений: Wi-Fi, Bluetooth, Zigbee.");
        }
    }

    private static double readNonNegativeDouble(Scanner sc, String prompt) {
        while (true) {
            double d = readDouble(sc, prompt);
            if (d >= 0) return d;
            System.out.println("Значение должно быть неотрицательным.");
        }
    }
}