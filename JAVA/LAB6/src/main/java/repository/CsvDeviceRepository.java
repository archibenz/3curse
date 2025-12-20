package repository;

import model.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class CsvDeviceRepository implements DeviceRepository {

    private static final String HEADER = "type;id;manufacturer;model;material;key;locked;param1;param2;param3;param4;wear";

    private final Path path;

    public CsvDeviceRepository(Path path) {
        this.path = path;
    }

    @Override
    public List<LockDevice> loadDevices() {
        ensureFileExists();
        List<LockDevice> devices = new ArrayList<>();
        int maxId = 0;
        try {
            List<String> lines = Files.readAllLines(path, StandardCharsets.UTF_8);
            for (String line : lines) {
                if (line.trim().isEmpty() || line.startsWith("type")) continue;
                LockDevice d = parseLine(line);
                if (d != null) {
                    devices.add(d);
                    maxId = Math.max(maxId, d.getId());
                }
            }
        } catch (IOException e) {
            System.out.println("Не удалось прочитать файл: " + e.getMessage());
        }
        LockDevice.ensureNextIdAbove(maxId);
        return devices;
    }

    @Override
    public void saveDevices(List<LockDevice> devices) {
        ensureFileExists();
        List<String> lines = new ArrayList<>();
        lines.add(HEADER);
        for (LockDevice d : devices) {
            lines.add(toCsvLine(d));
        }
        try {
            Files.write(path, lines, StandardCharsets.UTF_8);
        } catch (IOException e) {
            System.out.println("Не удалось записать CSV: " + e.getMessage());
        }
    }

    private void ensureFileExists() {
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            if (Files.notExists(path)) {
                Files.writeString(path, HEADER + System.lineSeparator(), StandardCharsets.UTF_8);
            }
        } catch (IOException e) {
            System.out.println("Ошибка при создании файла: " + e.getMessage());
        }
    }

    private LockDevice parseLine(String line) {
        String[] parts = line.split(";");
        if (parts.length < 12) return null;
        String type = parts[0];
        int id = parseInt(parts[1], 0);
        String manufacturer = parts[2];
        String model = parts[3];
        String material = parts[4];
        String key = parts[5];
        boolean locked = Boolean.parseBoolean(parts[6]);
        String p1 = parts[7];
        String p2 = parts[8];
        String p3 = parts[9];
        String p4 = parts[10];
        double wear = parseDouble(parts[11], -1);

        return switch (type) {
            case "RemoteControllableLock" -> new RemoteControllableLock(
                    id, manufacturer, model, material, key, locked,
                    parseInt(p1, 50), Boolean.parseBoolean(p2), parseInt(p3, 30), wear
            );
            case "BarrierGate" -> new BarrierGate(
                    id, manufacturer, model, material, key, locked,
                    parseInt(p1, 0), Boolean.parseBoolean(p2), parseInt(p3, 0), parseInt(p4, 0), wear
            );
            case "SafeLock" -> new SafeLock(
                    id, manufacturer, model, material, key, locked,
                    parseCombination(p1), parseInt(p2, 0), Boolean.parseBoolean(p3), parseDouble(p4, 22.0), wear
            );
            default -> null;
        };
    }

    private String toCsvLine(LockDevice d) {
        String base = String.join(";",
                d.getType(),
                String.valueOf(d.getId()),
                d.getManufacturer(),
                d.getModel(),
                d.getMaterial(),
                d.getKeys().isEmpty() ? "" : d.getKeys().get(0),
                String.valueOf(d.isLocked())
        );
        if (d instanceof RemoteControllableLock r) {
            return base + String.format(";%d;%s;%d;;%.2f",
                    r.getBatteryLevel(), r.isOnline(), r.getSignalStrength(), d.getWearPercent());
        }
        if (d instanceof BarrierGate g) {
            return base + String.format(";%d;%s;%d;%d;%.2f",
                    g.getAutoCloseSeconds(), g.isDown(), g.getVehiclesPassed(), g.getQueueEstimate(), d.getWearPercent());
        }
        if (d instanceof SafeLock s) {
            String comb = s.getCombination().stream().map(String::valueOf).collect(Collectors.joining("-"));
            return base + String.format(";%s;%d;%s;%.1f;%.2f",
                    comb, s.getWrongAttempts(), s.isLockout(), s.getInnerTempC(), d.getWearPercent());
        }
        return base + ";;;;;";
    }

    private int parseInt(String s, int def) {
        try { return Integer.parseInt(s); } catch (NumberFormatException e) { return def; }
    }

    private double parseDouble(String s, double def) {
        try {
            return Double.parseDouble(s.replace(',', '.'));
        } catch (NumberFormatException e) {
            return def;
        }
    }

    private List<Integer> parseCombination(String comb) {
        try {
            String[] parts = comb.split("-");
            List<Integer> res = new ArrayList<>();
            for (String p : parts) res.add(Integer.parseInt(p));
            if (res.size() == 4) return res;
        } catch (Exception ignored) { }
        return Arrays.asList(0,0,0,0);
    }
}
