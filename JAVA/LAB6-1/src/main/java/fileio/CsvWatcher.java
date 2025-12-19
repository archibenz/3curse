package fileio;

import devices.WearableDevice;

import java.io.IOException;
import java.util.*;

public class CsvWatcher {

    private final String fileName;
    private final fileio.CsvLogger logger;
    private Map<String, Map<String, String>> lastSnapshot = new HashMap<>();

    public CsvWatcher(String fileName, fileio.CsvLogger logger) {
        this.fileName = fileName;
        this.logger = logger;
    }

    public void loop(long intervalMs) {
        while (true) {
            try {
                FileReader reader = new FileReader(fileName);
                List<WearableDevice> devices = reader.readFile();
                Map<String, Map<String, String>> current = toSnapshot(devices);
                compareAndLog(lastSnapshot, current);
                lastSnapshot = current;
                Thread.sleep(intervalMs);
            } catch (IOException | InterruptedException e) {
                System.err.println("CSV‑watcher остановлен: " + e.getMessage());
                break;
            }
        }
    }

    private Map<String, Map<String, String>> toSnapshot(List<WearableDevice> devices) {
        Map<String, Map<String, String>> res = new HashMap<>();
        for (WearableDevice d : devices) {
            res.put(d.getId(), d.toPropertyMap());
        }
        return res;
    }

    private void compareAndLog(Map<String, Map<String, String>> oldSnap,
                               Map<String, Map<String, String>> newSnap) {
        for (var entry : newSnap.entrySet()) {
            String id = entry.getKey();
            Map<String, String> newProps = entry.getValue();
            Map<String, String> oldProps = oldSnap.getOrDefault(id, Collections.emptyMap());

            for (var p : newProps.entrySet()) {
                String key = p.getKey();
                String newVal = p.getValue();
                String oldVal = oldProps.get(key);
                if (oldVal != null && !oldVal.equals(newVal)) {
                    logger.logChange(id, key, oldVal, newVal);
                }
            }
        }
    }
}
