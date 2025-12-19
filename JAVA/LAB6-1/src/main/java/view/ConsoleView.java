package view;

import devices.WearableDevice;
import java.util.List;

public class ConsoleView {

    public void showDevices(List<WearableDevice> devices) {
        if (devices == null || devices.isEmpty()) {
            System.out.println("Устройства не найдены");
            return;
        }
        for (WearableDevice d : devices) {
            System.out.println(d);
        }
    }

    public void showFileReplaced(String from, String to) {
        System.out.println("Файл \"" + from + "\" заменён файлом \"" + to + "\"");
    }

    public void showHelp() {
        System.out.println("""
Использование: java -jar lab3.jar <режим> [опции]

Режимы:
  --once <file>                 Разовый вывод состояния устройств из файла
  --replace <from> <to>         Замена одного файла другим
  --watch <file> --interval N   Цикличный вывод, период N секунд
  --csv <file> --out log.csv --interval N
                                Логирование изменяющихся свойств в CSV с периодом N секунд
  --help                        Справка
""");
    }

    public void showError(String message) {
        System.err.println("Ошибка: " + message);
    }
}
