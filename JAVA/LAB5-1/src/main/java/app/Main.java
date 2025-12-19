package app;

import controller.DeviceController;
import view.ConsoleView;
import java.util.*;
import devices.*;
import config.Config;
import fileio.FileReader;

public class Main {
    private static int idCounter = 1;
    private static String currentFileName = "devices.txt";
    private static FileReader fileReader = new FileReader(currentFileName);

    public static void main(String[] args) {
        if (args.length > 0) {
            ConsoleView view = new ConsoleView();
            DeviceController controller = new DeviceController(view);
            controller.run(args);
        } else {
            runInteractiveMenu();
        }
    }

    private static void runInteractiveMenu() {
        Scanner sc = new Scanner(System.in);
        ArrayList<WearableDevice> devices = new ArrayList<>();
        int choice;

        do {
            System.out.println("\n=== МЕНЮ ===");
            System.out.println("1. Добавить устройство");
            System.out.println("2. Удалить устройство");
            System.out.println("3. Изменить профиль пользователя");
            System.out.println("4. Выполнить функционал устройств");
            System.out.println("5. Показать все устройства");
            System.out.println("6. Читать устройства из файла");
            System.out.println("7. Изменить имя файла");
            System.out.println("8. Выход");

            choice = readIntInRange(sc, "Выбор: ", 1, 8);

            switch (choice) {
                case 1: {
                    String id = String.format("%03d", idCounter++);
                    String user = readNonEmptyString(sc, "Введите имя пользователя: ");
                    System.out.println("Тип устройства: 1 - Часы, 2 - Браслет, 3 - Тонометр");

                    int type = readIntInRange(sc, "Выберите тип (1–3): ", 1, 3);
                    int battery = readIntInRange(sc,
                            "Введите уровень заряда батареи (" + Config.MIN_BATTERY + "–" + Config.MAX_BATTERY + "): ",
                            Config.MIN_BATTERY, Config.MAX_BATTERY);

                    WearableDevice d = null;
                    if (type == 1) {
                        d = new Watch(id, user, battery);
                    } else if (type == 2) {
                        d = new FitnessBand(id, user, battery);
                    } else if (type == 3) {
                        d = new Tonometer(id, user, battery);
                    }

                    if (d != null) {
                        devices.add(d);
                        System.out.println("Устройство добавлено:\n" + d);
                    }
                    break;
                }

                case 2: {
                    String delId = readNonEmptyString(sc, "Введите ID для удаления: ");
                    boolean removed = devices.removeIf(dev -> dev.getId().equals(delId));
                    System.out.println(removed ? "Устройство удалено." : "Устройство с таким ID не найдено.");
                    break;
                }

                case 3: {
                    String editId = readNonEmptyString(sc, "Введите ID устройства: ");
                    boolean found = false;
                    for (WearableDevice dev : devices) {
                        if (dev.getId().equals(editId)) {
                            found = true;
                            String newUser = readNonEmptyString(sc, "Введите новое имя пользователя: ");
                            dev.setUserProfile(newUser);
                            System.out.println("Имя пользователя изменено.");
                        }
                    }
                    if (!found) System.out.println("Устройство с таким ID не найдено.");
                    break;
                }

                case 4: {
                    if (devices.isEmpty()) {
                        System.out.println("Список устройств пуст.");
                        break;
                    }

                    System.out.println("\nВыберите устройство:");
                    for (int i = 0; i < devices.size(); i++)
                        System.out.println((i + 1) + ". " + devices.get(i));

                    int index = readIntInRange(sc, "Выбор устройства: ", 1, devices.size()) - 1;
                    WearableDevice dev = devices.get(index);

                    if (dev instanceof Watch) {
                        Watch w = (Watch) dev;
                        System.out.println("1. Включить/выключить GPS");
                        System.out.println("2. Переключить формат времени");
                        System.out.println("3. Добавить будильник");
                        System.out.println("4. Включить/выключить уведомления");
                        System.out.println("5. Показать текущее время");
                        System.out.println("6. Добавить шаги");
                        System.out.println("7. Изменить часовой пояс");
                        System.out.println("8. Добавить уведомление");
                        System.out.println("9. Секундомер (start/stop/show)");

                        int action = readIntInRange(sc, "Выберите действие: ", 1, 9);
                        switch (action) {
                            case 1:
                                w.toggleGPS();
                                System.out.println("GPS " + (w.isGpsEnabled() ? "вкл" : "выкл"));
                                break;
                            case 2:
                                w.switchTimeFormat();
                                System.out.println("Формат времени переключен");
                                break;
                            case 3:
                                w.addAlarm();
                                System.out.println("Будильник добавлен");
                                break;
                            case 4:
                                w.toggleNotifications();
                                System.out.println("Уведомления " + (w.isNotificationsEnabled() ? "вкл" : "выкл"));
                                break;
                            case 5:
                                w.showTime();
                                break;
                            case 6: {
                                int s = readIntInRange(sc, "Введите количество шагов: ", 0, 100000);
                                w.addSteps(s);
                                System.out.println("Шаги добавлены");
                                break;
                            }
                            case 7: {
                                int tz = readInt(sc, "Введите смещение часового пояса (часов): ");
                                w.changeTimezone(tz);
                                System.out.println("Часовой пояс изменен");
                                break;
                            }
                            case 8: {
                                String note = readNonEmptyString(sc, "Введите текст уведомления: ");
                                w.addNotification(note);
                                System.out.println("Уведомление добавлено");
                                break;
                            }
                            case 9: {
                                System.out.println("1. Запуск, 2. Остановка, 3. Показ");
                                int st = readIntInRange(sc, "Выберите действие: ", 1, 3);
                                if (st == 1) {
                                    w.startStopwatch();
                                    System.out.println("Секундомер запущен");
                                } else if (st == 2) {
                                    w.stopStopwatch();
                                    System.out.println("Секундомер остановлен");
                                } else {
                                    w.showStopwatch();
                                }
                                break;
                            }
                        }

                    } else if (dev instanceof FitnessBand) {
                        FitnessBand f = (FitnessBand) dev;
                        System.out.println("1. Добавить шаги");
                        System.out.println("2. Включить/выключить трекер сна");
                        System.out.println("3. Включить/выключить пульсометр");
                        System.out.println("4. Показать статистику");
                        System.out.println("5. Провести тренировку");
                        System.out.println("6. Анализ сна");

                        int action = readIntInRange(sc, "Выберите действие: ", 1, 6);
                        switch (action) {
                            case 1: {
                                int s = readIntInRange(sc, "Введите количество шагов: ", 0, 100000);
                                f.addSteps(s);
                                System.out.println("Шаги добавлены");
                                break;
                            }
                            case 2: {
                                f.toggleSleepTracking();
                                System.out.println("Трекер сна " + (f.toString().contains("вкл") ? "вкл" : "выкл"));
                                break;
                            }
                            case 3: {
                                f.toggleHeartMonitor();
                                System.out.println("Пульсометр " + (f.toString().contains("вкл") ? "вкл" : "выкл"));
                                break;
                            }
                            case 4:
                                f.showStats();
                                break;
                            case 5: {
                                int min = readIntInRange(sc, "Длительность тренировки (мин): ", 1, 300);
                                int inten = readIntInRange(sc, "Интенсивность (1-10): ", 1, 10);
                                f.doWorkout(min, inten);
                                break;
                            }
                            case 6:
                                f.analyzeSleep();
                                break;
                        }

                    } else if (dev instanceof Tonometer) {
                        Tonometer t = (Tonometer) dev;
                        System.out.println("1. Измерить давление");
                        System.out.println("2. Включить/выключить память");
                        System.out.println("3. Показать последнее измерение");
                        System.out.println("4. Показать среднее давление");

                        int action = readIntInRange(sc, "Выберите действие: ", 1, 4);
                        switch (action) {
                            case 1: {
                                t.measurePressure();
                                System.out.println("Измерение выполнено");
                                t.showLastMeasurement();
                                break;
                            }
                            case 2: {
                                t.toggleMemory();
                                System.out.println("Память " + (t.toString().contains("вкл") ? "вкл" : "выкл"));
                                break;
                            }
                            case 3:
                                t.showLastMeasurement();
                                break;
                            case 4:
                                t.showAveragePressure();
                                break;
                        }
                    }
                    break;
                }

                case 5: {
                    if (devices.isEmpty()) {
                        System.out.println("Список устройств пуст.");
                    } else {
                        System.out.println("\n=== Список устройств ===");
                        for (WearableDevice d : devices) {
                            System.out.println(d);
                        }
                    }
                    break;
                }

                case 6: {
                    System.out.println("Текущий файл: " + currentFileName);
                    try {
                        List<WearableDevice> loadedDevices = fileReader.readFile();
                        if (loadedDevices.isEmpty()) {
                            System.out.println("В файле не найдено устройств.");
                        } else {
                            devices.addAll(loadedDevices);
                            System.out.println("Успешно загружено устройств: " + loadedDevices.size());
                            for (WearableDevice d : loadedDevices) {
                                System.out.println("  - " + d);
                            }
                        }
                        fileReader.printStatistics();
                    } catch (java.io.FileNotFoundException e) {
                        System.out.println("Ошибка: Файл '" + currentFileName + "' не найден.");
                    } catch (java.io.IOException e) {
                        System.out.println("Ошибка при чтении файла: " + e.getMessage());
                    }
                    break;
                }

                case 7: {
                    System.out.println("Текущий файл: " + currentFileName);
                    String newFileName = readNonEmptyString(sc, "Введите новое имя файла: ");
                    currentFileName = newFileName;
                    fileReader.setFileName(newFileName);
                    System.out.println("Имя файла изменено на: " + currentFileName);
                    break;
                }

                case 8:
                    System.out.println("Выход из программы...");
                    break;
            }

        } while (choice != 8);

        sc.close();
    }

    private static int readInt(Scanner sc, String prompt) {
        int value;
        while (true) {
            System.out.print(prompt);
            if (sc.hasNextInt()) {
                value = sc.nextInt();
                sc.nextLine();
                return value;
            } else {
                System.out.println("Ошибка: введите число!");
                sc.nextLine();
            }
        }
    }

    private static int readIntInRange(Scanner sc, String prompt, int min, int max) {
        int value;
        do {
            value = readInt(sc, prompt);
            if (value < min || value > max) {
                System.out.println("Ошибка: значение должно быть в диапазоне " + min + "–" + max + "!");
            }
        } while (value < min || value > max);
        return value;
    }

    private static String readNonEmptyString(Scanner sc, String prompt) {
        String input;
        do {
            System.out.print(prompt);
            input = sc.nextLine().trim();
            if (input.isEmpty()) System.out.println("Ошибка: значение не может быть пустым!");
        } while (input.isEmpty());
        return input;
    }
}
