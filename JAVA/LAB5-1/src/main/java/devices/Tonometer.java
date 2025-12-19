package devices;

import config.Config;
import java.util.*;

public class Tonometer extends WearableDevice {
    private int systolicPressure;
    private int diastolicPressure;
    private int lastPulse;
    private boolean memoryEnabled;
    private int measurementsTaken;
    private LinkedList<int[]> history; // [systolic, diastolic, pulse]

    public Tonometer(String id, String userProfile, int batteryLevel) {
        super(id,userProfile,batteryLevel);
        systolicPressure = 120;
        diastolicPressure = 80;
        lastPulse = 70;
        memoryEnabled = true;
        measurementsTaken = 0;
        history = new LinkedList<>();
    }

    @Override
    public void putOn() {}
    @Override
    public void takeOff() {}
    @Override
    public void chargeBattery() { setBatteryLevel(Config.MAX_BATTERY); }
    @Override
    public int getHeartRate() { return randomInRange(50,100); }

    public void measurePressure() {
        systolicPressure = randomInRange(100,160);
        diastolicPressure = randomInRange(60,100);
        lastPulse = getHeartRate();
        measurementsTaken++;
        history.add(new int[]{systolicPressure, diastolicPressure, lastPulse});
        if(history.size()>5) history.removeFirst();
    }

    public void toggleMemory() { memoryEnabled = !memoryEnabled; }

    public void showLastMeasurement() {
        System.out.println("Последнее измерение: " + systolicPressure + "/" + diastolicPressure +
                ", Пульс: " + lastPulse + " уд/мин");
    }

    public void showAveragePressure() {
        if(history.isEmpty()) {
            System.out.println("Нет измерений");
            return;
        }
        int sumS=0, sumD=0, sumP=0;
        for(int[] m: history){
            sumS += m[0]; sumD += m[1]; sumP += m[2];
        }
        int n = history.size();
        System.out.println("Среднее давление: " + sumS/n + "/" + sumD/n + ", Пульс: " + sumP/n);
    }

    @Override
    public String toString() {
        return "[Тонометр] " + super.toString() +
                ", Давление: " + systolicPressure + "/" + diastolicPressure +
                ", Память: " + (memoryEnabled?"вкл":"выкл") +
                ", Измерений: " + measurementsTaken;
    }
}
