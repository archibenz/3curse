package fileio;

import devices.*;
import config.Config;
import java.io.*;
import java.util.*;

/**
 * Класс для чтения файла с данными устройств
 * Ищет ключевые фразы в файле и воссоздает объекты
 */
public class FileReader {
    private String fileName;
    private int objectsFound;
    private int propertiesRead;
    private int propertiesNotFound;
    
    public FileReader(String fileName) {
        this.fileName = fileName;
        this.objectsFound = 0;
        this.propertiesRead = 0;
        this.propertiesNotFound = 0;
    }
    
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
    
    public String getFileName() {
        return fileName;
    }
    
    /**
     * Читает файл и возвращает список созданных устройств
     */
    public List<WearableDevice> readFile() throws IOException {
        List<WearableDevice> devices = new ArrayList<>();
        objectsFound = 0;
        propertiesRead = 0;
        propertiesNotFound = 0;
        
        try (BufferedReader reader = new BufferedReader(new java.io.FileReader(fileName))) {
            String line;
            Map<String, String> currentObjectData = new HashMap<>();
            String currentDeviceType = null;
            
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                
                // Проверка на начало нового объекта (поиск типа устройства)
                String deviceType = detectDeviceType(line);
                if (deviceType != null) {
                    // Сохраняем предыдущий объект, если есть
                    if (currentDeviceType != null && !currentObjectData.isEmpty()) {
                        WearableDevice device = createDevice(currentDeviceType, currentObjectData);
                        if (device != null) {
                            devices.add(device);
                            objectsFound++;
                        }
                    }
                    // Начинаем новый объект
                    currentDeviceType = deviceType;
                    currentObjectData = new HashMap<>();
                    
                    // Парсим строку с типом устройства и данными через запятую
                    parseCommaSeparatedLine(line, deviceType, currentObjectData);
                    continue;
                }
                
                // Если тип устройства уже определен, продолжаем парсинг данных
                if (currentDeviceType != null) {
                    parseCommaSeparatedLine(line, currentDeviceType, currentObjectData);
                }
            }
            
            // Сохраняем последний объект
            if (currentDeviceType != null && !currentObjectData.isEmpty()) {
                WearableDevice device = createDevice(currentDeviceType, currentObjectData);
                if (device != null) {
                    devices.add(device);
                    objectsFound++;
                }
            }
        }
        
        return devices;
    }

    private String detectDeviceType(String line) {
        if (line.contains(DeviceDataStructure.WATCH_KEYWORD) || 
            line.toLowerCase().contains("watch") || 
            line.toLowerCase().contains("часы")) {
            return "watch";
        }
        if (line.contains(DeviceDataStructure.FITNESS_BAND_KEYWORD) || 
            line.toLowerCase().contains("fitnessband") || 
            line.toLowerCase().contains("браслет")) {
            return "fitnessband";
        }
        if (line.contains(DeviceDataStructure.TONOMETER_KEYWORD) || 
            line.toLowerCase().contains("tonometer") || 
            line.toLowerCase().contains("тонометр")) {
            return "tonometer";
        }
        return null;
    }

    private void parseCommaSeparatedLine(String line, String deviceType, Map<String, String> data) {
        if (deviceType == null) return;

        String[] parts = line.split(",");
        if (parts.length < 1) return;

        List<String> fieldsOrder = DeviceDataStructure.getFieldsOrderForType(deviceType);

        int startIndex = 0;
        String firstPart = parts[0].trim().toLowerCase();
        if (firstPart.contains("часы") || firstPart.contains("watch") || 
            firstPart.contains("фитнес") || firstPart.contains("браслет") || 
            firstPart.contains("fitnessband") || firstPart.contains("тонометр") || 
            firstPart.contains("tonometer")) {
            startIndex = 1;
        }

        int fieldIndex = 0;
        for (int i = startIndex; i < parts.length && fieldIndex < fieldsOrder.size(); i++) {
            String value = parts[i].trim();
            if (value.isEmpty()) continue;
            
            String fieldKey = fieldsOrder.get(fieldIndex);

            if (data.containsKey(fieldKey)) {
                fieldIndex++;
                i--;
                continue;
            }
            
            data.put(fieldKey, value);
            propertiesRead++;
            fieldIndex++;
        }
    }
    

    private WearableDevice createDevice(String deviceType, Map<String, String> data) {
        String id = data.getOrDefault("id", "");
        String userProfile = data.getOrDefault("userProfile", "Неизвестно");
        int batteryLevel = parseInt(data.get("batteryLevel"), 50);

        if (batteryLevel < Config.MIN_BATTERY) batteryLevel = Config.MIN_BATTERY;
        if (batteryLevel > Config.MAX_BATTERY) batteryLevel = Config.MAX_BATTERY;

        List<String> fieldsOrder = DeviceDataStructure.getFieldsOrderForType(deviceType);
        for (String fieldKey : fieldsOrder) {
            if (!data.containsKey(fieldKey)) {
                propertiesNotFound++;
            }
        }
        
        WearableDevice device = null;
        
        try {
            switch (deviceType.toLowerCase()) {
                case "watch":
                    device = createWatch(id, userProfile, batteryLevel, data);
                    break;
                case "fitnessband":
                    device = createFitnessBand(id, userProfile, batteryLevel, data);
                    break;
                case "tonometer":
                    device = createTonometer(id, userProfile, batteryLevel, data);
                    break;
            }
        } catch (Exception e) {
            System.err.println("Ошибка при создании устройства: " + e.getMessage());
        }
        
        return device;
    }

    private Watch createWatch(String id, String userProfile, int batteryLevel, Map<String, String> data) {
        Watch watch = new Watch(id.isEmpty() ? generateId() : id, userProfile, batteryLevel);

        if (data.containsKey("gpsEnabled")) {
            boolean gps = parseBoolean(data.get("gpsEnabled"));
            if (gps != watch.isGpsEnabled()) watch.toggleGPS();
        }
        
        if (data.containsKey("timeFormat")) {
            String format = data.get("timeFormat").toLowerCase();
            if (format.contains("12") && !watch.toString().contains("12h")) {
                watch.switchTimeFormat();
            }
        }
        
        if (data.containsKey("notificationsEnabled")) {
            boolean notif = parseBoolean(data.get("notificationsEnabled"));
            if (!notif && watch.isNotificationsEnabled()) watch.toggleNotifications();
        }
        
        if (data.containsKey("alarmCount")) {
            int alarms = parseInt(data.get("alarmCount"), 0);
            for (int i = 0; i < alarms; i++) watch.addAlarm();
        }
        
        if (data.containsKey("currentHour")) {
            int hour = parseInt(data.get("currentHour"), 12);
            if (hour >= 0 && hour <= 23) {
                watch.changeTimezone(hour - 12);
            }
        }
        
        if (data.containsKey("steps")) {
            int steps = parseInt(data.get("steps"), 0);
            watch.addSteps(steps);
        }
        
        return watch;
    }
    

    private FitnessBand createFitnessBand(String id, String userProfile, int batteryLevel, Map<String, String> data) {
        FitnessBand band = new FitnessBand(id.isEmpty() ? generateId() : id, userProfile, batteryLevel);
        
        if (data.containsKey("sleepTracking")) {
            boolean sleep = parseBoolean(data.get("sleepTracking"));
            if (sleep) band.toggleSleepTracking();
        }
        
        if (data.containsKey("heartMonitorActive")) {
            boolean heart = parseBoolean(data.get("heartMonitorActive"));
            if (heart) band.toggleHeartMonitor();
        }
        
        if (data.containsKey("stepsCount")) {
            int steps = parseInt(data.get("stepsCount"), 0);
            band.addSteps(steps);
        }
        
        return band;
    }
    

    private Tonometer createTonometer(String id, String userProfile, int batteryLevel, Map<String, String> data) {
        Tonometer tonometer = new Tonometer(id.isEmpty() ? generateId() : id, userProfile, batteryLevel);
        
        if (data.containsKey("memoryEnabled")) {
            boolean memory = parseBoolean(data.get("memoryEnabled"));
            if (!memory && tonometer.toString().contains("вкл")) {
                tonometer.toggleMemory();
            }
        }

        if (data.containsKey("systolicPressure") || data.containsKey("diastolicPressure") || 
            data.containsKey("lastPulse")) {
            tonometer.measurePressure();
        }
        
        if (data.containsKey("measurementsTaken")) {
            int measurements = parseInt(data.get("measurementsTaken"), 0);
            for (int i = 0; i < measurements; i++) {
                tonometer.measurePressure();
            }
        }
        
        return tonometer;
    }
    

    private boolean parseBoolean(String value) {
        if (value == null) return false;
        String lower = value.toLowerCase().trim();
        return lower.contains("вкл") || lower.contains("true") || 
               lower.contains("да") || lower.contains("yes") || 
               lower.equals("1");
    }
    

    private int parseInt(String value, int defaultValue) {
        if (value == null || value.trim().isEmpty()) return defaultValue;
        try {
            String cleaned = value.replaceAll("[^0-9-]", "").trim();
            if (cleaned.isEmpty()) return defaultValue;
            return Integer.parseInt(cleaned);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
    

    private String generateId() {
        return String.format("%03d", (int)(Math.random() * 1000));
    }
    

    public void printStatistics() {
        System.out.println("\n=== Статистика чтения файла ===");
        System.out.println("Найдено объектов: " + objectsFound);
        System.out.println("Успешно прочитано свойств: " + propertiesRead);
        System.out.println("Не найдено свойств: " + propertiesNotFound);
    }
    
    public int getObjectsFound() {
        return objectsFound;
    }
    
    public int getPropertiesRead() {
        return propertiesRead;
    }
    
    public int getPropertiesNotFound() {
        return propertiesNotFound;
    }
}

