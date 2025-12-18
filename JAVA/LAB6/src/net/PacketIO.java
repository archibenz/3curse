package net;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * Простая утилита для передачи сообщений с префиксом длины и разбиением на пакеты,
 * чтобы укладываться в стандартный MTU и не дробить полезные данные.
 */
public final class PacketIO {

    private static final int CHUNK_SIZE = 1300; // чуть меньше MTU Ethernet (1500 байт)

    private PacketIO() {}

    public static void sendMessage(DataOutputStream out, String payload) throws IOException {
        byte[] data = payload.getBytes(StandardCharsets.UTF_8);
        out.writeInt(data.length);
        int offset = 0;
        while (offset < data.length) {
            int size = Math.min(CHUNK_SIZE, data.length - offset);
            out.write(data, offset, size);
            offset += size;
        }
        out.flush();
    }

    public static String receiveMessage(DataInputStream in) throws IOException {
        int length;
        try {
            length = in.readInt();
        } catch (EOFException e) {
            return null;
        }
        if (length < 0) return null;
        byte[] data = in.readNBytes(length);
        if (data.length < length) throw new IOException("Недостаточно данных в пакете");
        return new String(data, StandardCharsets.UTF_8);
    }
}
