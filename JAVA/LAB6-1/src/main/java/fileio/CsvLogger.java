package fileio;

import java.io.*;
import java.time.LocalDateTime;

public class CsvLogger implements AutoCloseable {

    private final PrintWriter out;

    public CsvLogger(String fileName) throws IOException {
        boolean append = new java.io.File(fileName).exists();
        out = new PrintWriter(new FileWriter(fileName, true));
        if (!append) {
            out.println("timestamp;deviceId;property;oldValue;newValue");
        }
    }

    public void logChange(String deviceId, String property,
                          String oldValue, String newValue) {
        String ts = LocalDateTime.now().toString();
        out.printf("%s;%s;%s;%s;%s%n", ts, deviceId, property, oldValue, newValue);
        out.flush();
    }

    @Override
    public void close() {
        out.close();
    }
}
