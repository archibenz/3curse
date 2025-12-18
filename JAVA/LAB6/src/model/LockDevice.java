package model;

import java.util.*;

public abstract class LockDevice {
    private static int NEXT_ID = 1;

    protected final int id;
    protected String manufacturer;
    protected String model;
    protected String material;
    protected final List<String> keys = new ArrayList<>();
    protected boolean locked;
    protected double wearPercent; // 0..100

    protected final Random rnd = new Random();

    protected LockDevice(String manufacturer, String model, String material, String masterKey) {
        this(0, manufacturer, model, material, masterKey, true, -1);
    }

    protected LockDevice(int id, String manufacturer, String model, String material, String masterKey, boolean locked, double wearPercent) {
        if (id <= 0) {
            this.id = NEXT_ID++;
        } else {
            this.id = id;
            NEXT_ID = Math.max(NEXT_ID, id + 1);
        }
        this.manufacturer = manufacturer;
        this.model = model;
        this.material = material;
        if (masterKey != null && !masterKey.isEmpty()) {
            keys.add(masterKey);
        }
        this.locked = locked;
        this.wearPercent = wearPercent >= 0 ? wearPercent : rnd.nextDouble() * 10;
    }

    public static void ensureNextIdAbove(int maxExistingId) {
        NEXT_ID = Math.max(NEXT_ID, maxExistingId + 1);
    }

    // 7.1 Базовые возможности
    public void lock() { locked = true; }

    public void unlock() { locked = false; }

    public boolean isLocked() { return locked; }

    public void replaceKey(String newKey) {
        if (newKey == null || newKey.isEmpty()) return;
        if (keys.isEmpty()) keys.add(newKey);
        else keys.set(0, newKey); // заменяем мастер-ключ (первый)
    }

    public String statusInfo() {
        return String.format(
            "[%d] %s | Тип: %s | Материал: %s | Заперт: %s | Ключей: %d | Износ: %.1f%%",
            id, manufacturer + " " + model, getType(), material, locked ? "да" : "нет", keys.size(), wearPercent
        );
    }

    // 4. Дополнительные методы абстрактного класса
    public void addKey(String k) {
        if (k == null || k.isEmpty()) return;
        keys.add(k);
    }

    public void removeKey(String k) { keys.remove(k); }

    public boolean hasKey(String k) { return keys.contains(k); }

    public void randomizeProperties() {
        wearPercent = Math.min(100.0, Math.max(0.0, wearPercent + rnd.nextGaussian() * 5 + 2));
        onRandomize();
    }

    protected void onRandomize() { /* по умолчанию ничего */ }

    public String shortInfo() { return String.format("%s #%d (%s %s)", getType(), id, manufacturer, model); }

    public String getType() { return this.getClass().getSimpleName(); }

    // Геттеры/сеттеры (не считаются как функционал по условию)
    public int getId() { return id; }
    public String getManufacturer() { return manufacturer; }
    public void setManufacturer(String manufacturer) { this.manufacturer = manufacturer; }
    public String getModel() { return model; }
    public void setModel(String model) { this.model = model; }
    public String getMaterial() { return material; }
    public void setMaterial(String material) { this.material = material; }
    public boolean getLocked() { return locked; }
    public void setLocked(boolean locked) { this.locked = locked; }
    public double getWearPercent() { return wearPercent; }
    public void setWearPercent(double wearPercent) { this.wearPercent = wearPercent; }
    public List<String> getKeys() { return Collections.unmodifiableList(keys); }
}
