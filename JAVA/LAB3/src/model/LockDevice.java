package model;

import java.util.*;

public abstract class LockDevice {
    private static int NEXT_ID = 1;

    protected final int id;
    protected String manufacturer;
    protected String model;
    protected String material;
    protected final List<String> keys = new ArrayList<>();
    protected boolean locked = true;

    // Свойство, которое по запросу меняется на случайное значение (требование 4.3)
    protected double wearPercent; // 0..100, условный износ
    protected final Random rnd = new Random();

    protected LockDevice(String manufacturer, String model, String material, String masterKey) {
        this.id = NEXT_ID++;
        this.manufacturer = manufacturer;
        this.model = model;
        this.material = material;
        if (masterKey != null && !masterKey.isEmpty()) {
            keys.add(masterKey);
        }
        this.wearPercent = rnd.nextDouble() * 10; // стартовый износ до 10%
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

    public void randomizeProperties() { // 4.3 изменение на случайную величину в пределах
        wearPercent = Math.min(100.0, Math.max(0.0, wearPercent + rnd.nextGaussian() * 5 + 2));
        onRandomize(); // типоспецифичное
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
}