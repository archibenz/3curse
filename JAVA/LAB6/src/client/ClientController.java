package client;

import view.ClientView;

public class ClientController {

    private final DeviceClient client;
    private final ClientView view;

    public ClientController(DeviceClient client, ClientView view) {
        this.client = client;
        this.view = view;
    }

    public void run() {
        view.showBanner();
        boolean running = true;
        while (running) {
            int cmd = view.showMenu();
            switch (cmd) {
                case 1 -> showStatuses();
                case 2 -> doPing();
                case 3 -> changeLockState(true);
                case 4 -> changeLockState(false);
                case 0 -> running = false;
                default -> view.showMessage("Неизвестная команда");
            }
        }
        view.showMessage("Клиент завершён.");
    }

    private void showStatuses() {
        try {
            var statuses = client.fetchDeviceStatuses();
            view.showStatuses(statuses);
        } catch (Exception e) {
            view.showMessage("Ошибка при получении данных: " + e.getMessage());
        }
    }

    private void doPing() {
        try {
            String response = client.ping();
            view.showMessage("Ответ сервера: " + response);
        } catch (Exception e) {
            view.showMessage("Сервер недоступен: " + e.getMessage());
        }
    }

    private void changeLockState(boolean lock) {
        try {
            int id = view.askDeviceId();
            String resp = client.setLocked(id, lock);
            view.showMessage("Ответ: " + resp);
        } catch (Exception e) {
            view.showMessage("Не удалось выполнить операцию: " + e.getMessage());
        }
    }
}
