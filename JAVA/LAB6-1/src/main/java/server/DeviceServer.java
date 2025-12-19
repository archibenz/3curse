package server;

import devices.WearableDevice;
import fileio.FileReader;
import net.PacketIO;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DeviceServer {

    private final int port;
    private final String fileName;
    private final ExecutorService executor = Executors.newCachedThreadPool();

    public DeviceServer(int port, String fileName) {
        this.port = port;
        this.fileName = fileName;
    }

    public void start() throws IOException {
        try (ServerSocket serverSocket = new ServerSocket(port)) {
            System.out.println("Сервер запущен на порту " + port + ". Файл: " + fileName);
            while (true) {
                Socket socket = serverSocket.accept();
                executor.submit(() -> handleClient(socket));
            }
        } finally {
            executor.shutdown();
        }
    }

    private void handleClient(Socket socket) {
        try (socket;
             DataInputStream in = new DataInputStream(socket.getInputStream());
             DataOutputStream out = new DataOutputStream(socket.getOutputStream())) {

            String request = PacketIO.receiveMessage(in);
            if (request == null) {
                return;
            }
            if (request.startsWith("LIST")) {
                String payload = loadDevicesPayload();
                PacketIO.sendMessage(out, payload);
            } else {
                PacketIO.sendMessage(out, "Неизвестная команда: " + request);
            }
        } catch (IOException e) {
            System.out.println("Ошибка при обработке клиента: " + e.getMessage());
        }
    }

    private String loadDevicesPayload() {
        FileReader reader = new FileReader(fileName);
        StringBuilder sb = new StringBuilder();
        sb.append("Ответ сервера. Источник: ").append(fileName).append('\n');
        try {
            List<WearableDevice> devices = reader.readFile();
            if (devices.isEmpty()) {
                sb.append("Данные не найдены.\n");
            } else {
                sb.append("Найдено устройств: ").append(devices.size()).append('\n');
                for (WearableDevice device : devices) {
                    sb.append("- ").append(device).append('\n');
                }
            }
            sb.append("Статистика чтения: объекты=").append(reader.getObjectsFound())
                    .append(", свойства прочитаны=").append(reader.getPropertiesRead())
                    .append(", свойства не найдены=").append(reader.getPropertiesNotFound())
                    .append('\n');
        } catch (IOException e) {
            sb.append("Ошибка чтения файла: ").append(e.getMessage()).append('\n');
        }
        return sb.toString();
    }
}
