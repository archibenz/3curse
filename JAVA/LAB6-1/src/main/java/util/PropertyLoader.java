package util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

public final class PropertyLoader {

    private PropertyLoader() {
    }

    public static Properties load(Path path) {
        Properties properties = new Properties();
        if (path == null) {
            return properties;
        }
        if (Files.notExists(path)) {
            return properties;
        }
        try (var reader = Files.newBufferedReader(path)) {
            properties.load(reader);
        } catch (IOException e) {
            System.out.println("Не удалось загрузить настройки " + path + ": " + e.getMessage());
        }
        return properties;
    }
}
