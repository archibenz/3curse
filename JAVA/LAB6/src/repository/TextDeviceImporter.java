package repository;

import model.BarrierGate;
import model.LockDevice;
import model.RemoteControllableLock;
import model.SafeLock;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Простейший парсер текстовых файлов (ЛР4): ищет пары ключ: значение, разделённые запятыми или переводами строк
 * и пытается воссоздать объекты устройств.
 */
public class TextDeviceImporter {

    private static final Pattern KV = Pattern.compile("([\\p{L}\\p{N}_ ]+)\\s*:\\s*([^,]+)");

    public TextImportResult importFile(Path path) {
        List<LockDevice> devices = new ArrayList<>();
        List<String> messages = new ArrayList<>();
        int propertiesRead = 0;
        int propertiesMissing = 0;
        int linesScanned = 0;

        if (path == null) {
            messages.add("Не задан файл для чтения.");
            return new TextImportResult("<none>", devices, propertiesRead, propertiesMissing, linesScanned, messages);
        }

        if (!Files.exists(path)) {
            messages.add("Файл не найден: " + path);
            return new TextImportResult(path.toString(), devices, propertiesRead, propertiesMissing, linesScanned, messages);
        }

        Map<String, String> current = new LinkedHashMap<>();
        try {
            for (String line : Files.readAllLines(path, StandardCharsets.UTF_8)) {
                linesScanned++;
                if (line.trim().isEmpty()) {
                    Counters c = finalizeDevice(current, devices);
                    propertiesRead += c.found;
                    propertiesMissing += c.missing;
                    current.clear();
                    continue;
                }

                Matcher m = KV.matcher(line);
                boolean matched = false;
                while (m.find()) {
                    matched = true;
                    String key = canonicalKey(m.group(1));
                    String value = sanitizeValue(m.group(2));
                    if (!key.isEmpty() && !value.isEmpty()) {
                        current.put(key, value);
                    }
                }

                // Если встретился новый type в одной строке, считаем, что начался следующий объект
                if (matched && current.containsKey("type") && current.size() > 1 && line.toLowerCase().contains("type")) {
                    Counters c = finalizeDevice(current, devices);
                    propertiesRead += c.found;
                    propertiesMissing += c.missing;
                    current.clear();
                }
            }
            Counters c = finalizeDevice(current, devices);
            propertiesRead += c.found;
            propertiesMissing += c.missing;
        } catch (IOException e) {
            messages.add("Ошибка чтения: " + e.getMessage());
        }

        if (devices.isEmpty() && messages.isEmpty()) {
            messages.add("Не удалось распознать объекты в файле.");
        }

        return new TextImportResult(path.toString(), devices, propertiesRead, propertiesMissing, linesScanned, messages);
    }

    private Counters finalizeDevice(Map<String, String> fields, List<LockDevice> devices) {
        if (fields.isEmpty()) return new Counters(0, 0);

        String type = fields.getOrDefault("type", "remotecontrollablelock");
        List<String> expected = expectedFields(type);
        Counters counters = countFields(fields, expected);

        LockDevice device = buildDevice(type, fields);
        if (device != null) {
            devices.add(device);
        }
        return counters;
    }

    private LockDevice buildDevice(String typeKey, Map<String, String> fields) {
        String manufacturer = fields.getOrDefault("manufacturer", "Unknown");
        String model = fields.getOrDefault("model", "Model");
        String material = fields.getOrDefault("material", "Steel");
        String masterKey = fields.getOrDefault("key", "0000");
        boolean locked = parseBoolean(fields.get("locked"), true);
        double wear = parseDouble(fields.get("wear"), -1);
        int id = parseInt(fields.get("id"), 0);

        return switch (typeKey.toLowerCase()) {
            case "barriergate" -> new BarrierGate(
                    id, manufacturer, model, material, masterKey, locked,
                    parseInt(fields.get("autoclose"), 0),
                    parseBoolean(fields.get("down"), locked),
                    parseInt(fields.get("vehicles"), 0),
                    parseInt(fields.get("queue"), 0),
                    wear
            );
            case "safelock" -> new SafeLock(
                    id, manufacturer, model, material, masterKey, locked,
                    parseCombination(fields.get("combination")),
                    parseInt(fields.get("wrongattempts"), 0),
                    parseBoolean(fields.get("lockout"), false),
                    parseDouble(fields.get("innertemp"), 22.0),
                    wear
            );
            default -> new RemoteControllableLock(
                    id, manufacturer, model, material, masterKey, locked,
                    parseInt(fields.get("battery"), 50),
                    parseBoolean(fields.get("online"), false),
                    parseInt(fields.get("signal"), 40),
                    wear
            );
        };
    }

    private Counters countFields(Map<String, String> fields, List<String> expected) {
        int found = 0;
        int missing = 0;
        for (String key : expected) {
            if (fields.containsKey(key)) found++; else missing++;
        }
        return new Counters(found, missing);
    }

    private List<String> expectedFields(String typeKey) {
        List<String> base = new ArrayList<>(List.of("manufacturer", "model", "material", "key", "locked", "wear"));
        switch (typeKey.toLowerCase()) {
            case "barriergate" -> base.addAll(List.of("autoclose", "down", "vehicles", "queue"));
            case "safelock" -> base.addAll(List.of("combination", "wrongattempts", "lockout", "innertemp"));
            default -> base.addAll(List.of("battery", "signal", "online"));
        }
        return base;
    }

    private String canonicalKey(String raw) {
        String k = raw == null ? "" : raw.toLowerCase().replaceAll("[^\\p{L}\\p{N}]", "");
        return switch (k) {
            case "vendor", "brand", "maker" -> "manufacturer";
            case "name" -> "model";
            case "material", "metal" -> "material";
            case "key", "code", "password" -> "key";
            case "state", "closed" -> "locked";
            case "autoclose", "autocloseseconds", "autoclose_sec" -> "autoclose";
            case "battery", "batterylevel" -> "battery";
            case "signal", "signalstrength" -> "signal";
            case "online", "connected" -> "online";
            case "down", "lowered" -> "down";
            case "vehicles", "cars", "vehiclespassed" -> "vehicles";
            case "queue", "queueestimate" -> "queue";
            case "combo", "combination", "pin" -> "combination";
            case "wrongattempts", "attempts" -> "wrongattempts";
            case "lockout", "blocked" -> "lockout";
            case "innertemp", "temperature", "temp" -> "innertemp";
            case "wear", "wearpercent" -> "wear";
            default -> k;
        };
    }

    private String sanitizeValue(String raw) {
        if (raw == null) return "";
        return raw.replaceAll("[;,]+$", "").trim();
    }

    private boolean parseBoolean(String s, boolean def) {
        if (s == null) return def;
        String v = s.trim().toLowerCase();
        if (v.equals("true") || v.equals("yes") || v.equals("да") || v.equals("y")) return true;
        if (v.equals("false") || v.equals("no") || v.equals("нет") || v.equals("n")) return false;
        return def;
    }

    private int parseInt(String s, int def) {
        try { return Integer.parseInt(s.trim()); } catch (Exception e) { return def; }
    }

    private double parseDouble(String s, double def) {
        try { return Double.parseDouble(s.trim().replace(',', '.')); } catch (Exception e) { return def; }
    }

    private List<Integer> parseCombination(String raw) {
        if (raw == null || raw.isEmpty()) return Arrays.asList(0, 0, 0, 0);
        String cleaned = raw.replaceAll("[;,_]", "-");
        String[] parts = cleaned.split("[- ]+");
        List<Integer> res = new ArrayList<>();
        for (String p : parts) {
            try {
                res.add(Integer.parseInt(p));
            } catch (NumberFormatException ignored) { }
        }
        while (res.size() < 4) res.add(0);
        if (res.size() > 4) res = res.subList(0, 4);
        return res;
    }

    private record Counters(int found, int missing) { }
}
