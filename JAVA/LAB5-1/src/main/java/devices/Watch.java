package devices;

import config.Config;
import java.util.*;

public class Watch extends WearableDevice {
    private boolean gpsEnabled;
    private String timeFormat;
    private boolean notificationsEnabled;
    private int alarmCount;
    private int currentHour;
    private int steps;
    private int calories;
    private List<String> notificationsList;
    private long stopwatchStart;
    private boolean stopwatchRunning;

    public Watch(String id, String userProfile, int batteryLevel) {
        super(id, userProfile, batteryLevel);
        gpsEnabled = false;
        timeFormat = "24h";
        notificationsEnabled = true;
        alarmCount = 0;
        currentHour = randomInRange(0,23);
        steps = 0;
        calories = 0;
        notificationsList = new LinkedList<>();
        stopwatchRunning = false;
    }

    @Override
    public void putOn() {}
    @Override
    public void takeOff() {}
    @Override
    public void chargeBattery() { setBatteryLevel(Config.MAX_BATTERY); }
    @Override
    public int getHeartRate() { return randomInRange(Config.MIN_HEART_RATE, Config.MAX_HEART_RATE); }

    public void toggleGPS() { gpsEnabled = !gpsEnabled; }
    public void toggleNotifications() { notificationsEnabled = !notificationsEnabled; }
    public void addAlarm() { alarmCount++; }
    public void switchTimeFormat() { timeFormat = timeFormat.equals("24h") ? "12h" : "24h"; }
    public void addSteps(int s) { steps += s; calories += s/20; }
    public void changeTimezone(int hours) { currentHour = (currentHour + hours + 24) % 24; }
    public void addNotification(String note) {
        if(notificationsList.size() >= 5) notificationsList.remove(0);
        notificationsList.add(note);
    }
    public void startStopwatch() { stopwatchStart = System.currentTimeMillis(); stopwatchRunning = true; }
    public void stopStopwatch() { stopwatchRunning = false; }
    public void showStopwatch() {
        if(stopwatchRunning) System.out.println("Секундомер: " + (System.currentTimeMillis()-stopwatchStart)/1000.0 + " сек");
        else System.out.println("Секундомер не запущен");
    }

    public void showTime() {
        if (timeFormat.equals("24h")) {
            System.out.println("Текущее время: " + currentHour + ":00");
        } else {
            int hour = currentHour % 12 == 0 ? 12 : currentHour % 12;
            String ampm = currentHour < 12 ? "AM" : "PM";
            System.out.println("Текущее время: " + hour + ":00 " + ampm);
        }
    }


    public boolean isGpsEnabled() { return gpsEnabled; }
    public boolean isNotificationsEnabled() { return notificationsEnabled; }

    @Override
    public String toString() {
        return "[Часы] " + super.toString() +
                ", GPS: " + (gpsEnabled?"вкл":"выкл") +
                ", Формат: " + timeFormat +
                ", Уведомления: " + (notificationsEnabled?"вкл":"выкл") +
                ", Будильников: " + alarmCount +
                ", Шаги: " + steps + ", Калорий: " + calories;
    }
}
