package app;

import server.DeviceServer;

public class ServerMain {

    public static void main(String[] args) {
        int port = 5000;
        String fileName = "devices.txt";
        if (args.length > 0) {
            try {
                port = Integer.parseInt(args[0]);
            } catch (NumberFormatException ignored) {
                System.out.println("Неверный порт, используется 5000");
            }
        }
        if (args.length > 1) {
            fileName = args[1];
        }

        try {
            new DeviceServer(port, fileName).start();
        } catch (Exception e) {
            System.out.println("Ошибка сервера: " + e.getMessage());
        }
    }
}
