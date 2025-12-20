package model;

public class BarrierGate extends LockDevice {

    private boolean down = true; // опущен = закрыт
    private int vehiclesPassed = 0;
    private int autoCloseSeconds; // 0 = выкл.
    private int queueEstimate;    // случайная «очередь» у шлагбаума, 0..10

    public BarrierGate(String manufacturer, String model, String material, String masterKey, int autoCloseSeconds) {
        super(manufacturer, model, material, masterKey);
        this.autoCloseSeconds = Math.max(0, Math.min(120, autoCloseSeconds));
        this.queueEstimate = rnd.nextInt(5);
        this.down = true;
        this.locked = true; // для консистентности: заперт == опущен
    }

    public BarrierGate(int id, String manufacturer, String model, String material, String masterKey, boolean locked,
                       int autoCloseSeconds, boolean down, int vehiclesPassed, int queueEstimate, double wearPercent) {
        super(id, manufacturer, model, material, masterKey, locked, wearPercent);
        this.autoCloseSeconds = Math.max(0, Math.min(120, autoCloseSeconds));
        this.down = down;
        this.vehiclesPassed = Math.max(0, vehiclesPassed);
        this.queueEstimate = Math.max(0, Math.min(10, queueEstimate));
    }

    public void open() {
        down = false;
        locked = false;
    }

    public void close() {
        down = true;
        locked = true;
    }

    public void toggle() { if (down) open(); else close(); }

    public void passVehicle() {
        if (!down) {
            vehiclesPassed++;
            queueEstimate = Math.max(0, queueEstimate - 1);
            if (autoCloseSeconds > 0) {
                close();
            }
        }
    }

    public void setAutoCloseSeconds(int sec) { this.autoCloseSeconds = Math.max(0, Math.min(120, sec)); }

    public int getAutoCloseSeconds() { return autoCloseSeconds; }

    public boolean isDown() { return down; }

    public int getVehiclesPassed() { return vehiclesPassed; }

    public int getQueueEstimate() { return queueEstimate; }

    public void setDown(boolean down) { this.down = down; this.locked = down; }

    public void setVehiclesPassed(int vehiclesPassed) { this.vehiclesPassed = Math.max(0, vehiclesPassed); }

    public void setQueueEstimate(int queueEstimate) { this.queueEstimate = Math.max(0, Math.min(10, queueEstimate)); }

    @Override
    protected void onRandomize() {
        queueEstimate = Math.max(0, Math.min(10, queueEstimate + rnd.nextInt(5) - 2));
    }

    @Override
    public String statusInfo() {
        return super.statusInfo() +
                String.format(" | Положение: %s | Пропущено авто: %d | Автозакрытие: %ds | Очередь~%d",
                        down ? "опущен" : "поднят", vehiclesPassed, autoCloseSeconds, queueEstimate);
    }
}
