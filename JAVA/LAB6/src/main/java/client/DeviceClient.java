package client;

import net.PacketIO;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.Arrays;
import java.util.List;

public class DeviceClient {

    private final String host;
    private final int port;

    public DeviceClient(String host, int port) {
        this.host = host;
        this.port = port;
    }

    public List<String> fetchDeviceStatuses() throws IOException {
        String payload = sendAndReceive("LIST");
        if (payload == null || payload.isEmpty()) return List.of();
        return Arrays.stream(payload.split("\n"))
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .toList();
    }

    public String ping() throws IOException {
        String response = sendAndReceive("PING");
        return response == null ? "Нет ответа" : response;
    }

    public String setLocked(int deviceId, boolean lock) throws IOException {
        String command = (lock ? "LOCK " : "UNLOCK ") + deviceId;
        String response = sendAndReceive(command);
        return response == null ? "Нет ответа" : response;
    }

    private String sendAndReceive(String command) throws IOException {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(host, port), 3000);
            try (DataOutputStream out = new DataOutputStream(socket.getOutputStream());
                 DataInputStream in = new DataInputStream(socket.getInputStream())) {
                PacketIO.sendMessage(out, command);
                return PacketIO.receiveMessage(in);
            }
        }
    }
}
