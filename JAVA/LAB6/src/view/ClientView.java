package view;

import util.Inputs;

import java.util.List;

public class ClientView {

    public void showBanner() {
        System.out.println("\n=== Лабораторная 6. Клиент ===");
    }

    public int showMenu() {
        System.out.println("1) Получить список устройств с сервера");
        System.out.println("2) Пинг сервера");
        System.out.println("3) Заблокировать устройство по ID");
        System.out.println("4) Разблокировать устройство по ID");
        System.out.println("0) Выход");
        return Inputs.readIntInRange("Выбор: ", 0, 4);
    }

    public void showStatuses(List<String> statuses) {
        if (statuses.isEmpty()) {
            System.out.println("Сервер вернул пустой список.");
            return;
        }
        System.out.println("\nДоступные устройства:");
        statuses.forEach(s -> {
            System.out.println("--------------------------------");
            System.out.println(s);
        });
    }

    public void showMessage(String text) {
        System.out.println(text);
    }

    public int askDeviceId() {
        return Inputs.readInt("Введите ID устройства: ");
    }
}
