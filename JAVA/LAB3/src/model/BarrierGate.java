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
                // Эмуляция автозакрытия без таймеров: просто закрываем сразу, чтобы не плодить потоки
                close();
            }
        }
    }

    public void setAutoCloseSeconds(int sec) { this.autoCloseSeconds = Math.max(0, Math.min(120, sec)); }

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