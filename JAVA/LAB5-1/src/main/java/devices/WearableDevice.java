package devices;

import java.util.Map;
import java.util.Random;
import config.Config;

public abstract class WearableDevice {
    protected String id;
    protected String userProfile;
    protected int batteryLevel;

    public WearableDevice(String id, String userProfile, int batteryLevel) {
        this.id = id;
        this.userProfile = userProfile;
        this.batteryLevel = batteryLevel;
    }

    public abstract void putOn();
    public abstract void takeOff();
    public abstract void chargeBattery();
    public abstract int getHeartRate();

    public void setBatteryLevel(int level) {
        if (level < Config.MIN_BATTERY) this.batteryLevel = Config.MIN_BATTERY;
        else if (level > Config.MAX_BATTERY) this.batteryLevel = Config.MAX_BATTERY;
        else this.batteryLevel = level;
    }

    public int getBatteryLevel() {
        return batteryLevel;
    }

    public String getUserProfile() {
        return userProfile;
    }

    public void setUserProfile(String userProfile) {
        this.userProfile = userProfile;
    }

    public String getId() {
        return id;
    }

    protected int randomInRange(int min, int max) {
        return new Random().nextInt(max - min + 1) + min;
    }

    public Map<String, String> toPropertyMap() {
        Map<String, String> m = new java.util.LinkedHashMap<>();
        m.put("id", id);
        m.put("userProfile", userProfile);
        m.put("batteryLevel", String.valueOf(batteryLevel));
        return m;
    }

    @Override
    public String toString() {
        return "ID: " + id + ", Пользователь: " + userProfile +
                ", Заряд: " + batteryLevel + "%";
    }
}
