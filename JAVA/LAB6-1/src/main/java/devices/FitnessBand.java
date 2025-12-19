package devices;

import config.Config;
import java.util.*;

public class FitnessBand extends WearableDevice {
    private int stepsCount;
    private boolean sleepTracking;
    private int caloriesBurned;
    private int distanceKm;
    private boolean heartMonitorActive;
    private LinkedList<Integer> historySteps;
    private LinkedList<Integer> historyCalories;

    public FitnessBand(String id, String userProfile, int batteryLevel) {
        super(id, userProfile, batteryLevel);
        stepsCount = 0;
        sleepTracking = false;
        caloriesBurned = 0;
        distanceKm = 0;
        heartMonitorActive = false;
        historySteps = new LinkedList<>();
        historyCalories = new LinkedList<>();
    }

    @Override
    public void putOn() {}
    @Override
    public void takeOff() {}
    @Override
    public void chargeBattery() { setBatteryLevel(Config.MAX_BATTERY); }
    @Override
    public int getHeartRate() { return randomInRange(Config.MIN_HEART_RATE, Config.MAX_HEART_RATE); }

    public void addSteps(int s) {
        stepsCount += s;
        distanceKm += s/1300;
        caloriesBurned += s/20;
        historySteps.add(s);
        historyCalories.add(s/20);
        if(historySteps.size()>5) historySteps.removeFirst();
        if(historyCalories.size()>5) historyCalories.removeFirst();
    }

    public void toggleSleepTracking() { sleepTracking = !sleepTracking; }
    public void toggleHeartMonitor() { heartMonitorActive = !heartMonitorActive; }

    public void doWorkout(int minutes, int intensity) {
        int steps = minutes * intensity * randomInRange(80,120);
        addSteps(steps);
        System.out.println("Тренировка завершена. Добавлено шагов: " + steps);
    }

    public void analyzeSleep() {
        int hours = randomInRange(4,9);
        System.out.println("Ночной сон: " + hours + " часов");
    }

    public void showStats() {
        System.out.println("Шагов: " + stepsCount + ", Дистанция: " + distanceKm + " км, Калорий: " + caloriesBurned);
    }

    @Override
    public String toString() {
        return "[Фитнес-браслет] " + super.toString() +
                ", Шагов: " + stepsCount + ", Сон: " + (sleepTracking?"вкл":"выкл") +
                ", Пульсометр: " + (heartMonitorActive?"вкл":"выкл") +
                ", Калорий: " + caloriesBurned + ", Дистанция: " + distanceKm + " км";
    }
}
