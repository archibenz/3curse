package server;

import model.LockDevice;
import net.PacketIO;
import repository.DeviceRepository;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

public class DeviceServer {

    private final int port;
    private final DeviceRepository repository;
    private final ExecutorService executor = Executors.newCachedThreadPool();

    public DeviceServer(int port, DeviceRepository repository) {
        this.port = port;
        this.repository = repository;
    }

    public void start() throws IOException {
        try (ServerSocket serverSocket = new ServerSocket(port)) {
            System.out.println("[SERVER] Запуск на порту " + port);
            while (true) {
                Socket socket = serverSocket.accept();
                executor.submit(() -> handleClient(socket));
            }
        }
    }

    private void handleClient(Socket socket) {
        String remote = socket.getRemoteSocketAddress().toString();
        System.out.println("[SERVER] Клиент подключен: " + remote);
        try (socket;
             DataInputStream in = new DataInputStream(socket.getInputStream());
             DataOutputStream out = new DataOutputStream(socket.getOutputStream())) {

            String request;
            while ((request = PacketIO.receiveMessage(in)) != null) {
                String cmd = request.trim().toUpperCase();
                switch (cmd) {
                    case "LIST", "LIST_STATUS" -> sendDeviceStatuses(out);
                    case "PING" -> PacketIO.sendMessage(out, "PONG");
                    default -> PacketIO.sendMessage(out, "ERROR:UNKNOWN_COMMAND");
                }
            }
        } catch (IOException e) {
            System.out.println("[SERVER] Ошибка при работе с клиентом " + remote + ": " + e.getMessage());
        } finally {
            System.out.println("[SERVER] Клиент отключён: " + remote);
        }
    }

    private void sendDeviceStatuses(DataOutputStream out) throws IOException {
        List<LockDevice> devices = repository.loadDevices();
        String payload = devices.stream()
                .map(LockDevice::statusInfo)
                .collect(Collectors.joining("\n"));
        PacketIO.sendMessage(out, payload);
    }
}
