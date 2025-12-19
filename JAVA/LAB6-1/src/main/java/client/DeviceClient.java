package client;

import net.PacketIO;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

public class DeviceClient {

    private final String host;
    private final int port;

    public DeviceClient(String host, int port) {
        this.host = host;
        this.port = port;
    }

    public String requestDevices() throws IOException {
        try (Socket socket = new Socket(host, port);
             DataOutputStream out = new DataOutputStream(socket.getOutputStream());
             DataInputStream in = new DataInputStream(socket.getInputStream())) {
            PacketIO.sendMessage(out, "LIST");
            return PacketIO.receiveMessage(in);
        }
    }
}
