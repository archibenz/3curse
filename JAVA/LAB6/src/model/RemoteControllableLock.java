package model;

public class RemoteControllableLock extends LockDevice {

    private boolean online;          // подключено к удалённому управлению
    private int batteryLevel;        // 0..100
    private int signalStrength;      // 0..100

    public RemoteControllableLock(String manufacturer, String model, String material, String masterKey, int batteryLevel) {
        super(manufacturer, model, material, masterKey);
        this.batteryLevel = Math.max(0, Math.min(100, batteryLevel));
        this.signalStrength = 30 + rnd.nextInt(71); // 30..100
        this.online = false;
    }

    public RemoteControllableLock(int id, String manufacturer, String model, String material, String masterKey, boolean locked,
                                  int batteryLevel, boolean online, int signalStrength, double wearPercent) {
        super(id, manufacturer, model, material, masterKey, locked, wearPercent);
        this.batteryLevel = Math.max(0, Math.min(100, batteryLevel));
        this.online = online;
        this.signalStrength = Math.max(0, Math.min(100, signalStrength));
    }

    public void connectRemote() {
        if (batteryLevel <= 5) {
            online = false;
        } else {
            online = true;
            signalStrength = 30 + rnd.nextInt(71);
        }
    }

    public void disconnectRemote() { online = false; }

    public String ping() {
        if (!online) return "Офлайн.";
        if (batteryLevel <= 5) return "Батарея почти разряжена.";
        return "Сигнал " + signalStrength + "%, задержка " + (10 + rnd.nextInt(90)) + " мс";
    }

    public void remoteLock() {
        if (online && batteryLevel > 5) lock();
    }

    public void remoteUnlock() {
        if (online && batteryLevel > 5) unlock();
    }

    public void replaceBattery() { batteryLevel = 100; }

    public void setBatteryLevel(int level) { batteryLevel = Math.max(0, Math.min(100, level)); }

    public int getBatteryLevel() { return batteryLevel; }

    public boolean isOnline() { return online; }

    public int getSignalStrength() { return signalStrength; }

    public void setOnline(boolean online) { this.online = online; }

    public void setSignalStrength(int signalStrength) { this.signalStrength = Math.max(0, Math.min(100, signalStrength)); }

    @Override
    protected void onRandomize() {
        batteryLevel = Math.max(0, batteryLevel - (1 + rnd.nextInt(6)));
        signalStrength = Math.max(0, Math.min(100, signalStrength + rnd.nextInt(21) - 10));
    }

    @Override
    public String statusInfo() {
        return super.statusInfo() +
                String.format(" | Remote: %s | Battery: %d%% | Signal: %d%%",
                        online ? "on" : "off", batteryLevel, signalStrength);
    }
}
