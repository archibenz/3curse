package fileio;

import java.util.*;

/**
 * Класс для описания структуры данных устройств
 * Определяет ключевые фразы для поиска свойств каждого типа устройства
 */
public class DeviceDataStructure {
    public static final Map<String, List<String>> BASE_FIELDS_ALT = new HashMap<>();
    static {
        BASE_FIELDS_ALT.put("id", Arrays.asList("ID", "Id", "id", "Идентификатор"));
        BASE_FIELDS_ALT.put("userProfile", Arrays.asList("Пользователь", "User", "Имя", "Name", "Профиль", "Profile"));
        BASE_FIELDS_ALT.put("batteryLevel", Arrays.asList("Заряд", "Battery", "Батарея", "BatteryLevel", "Заряд батареи"));
    }

    public static final Map<String, String> BASE_FIELDS = new HashMap<>();
    static {
        BASE_FIELDS.put("id", "ID");
        BASE_FIELDS.put("userProfile", "Пользователь");
        BASE_FIELDS.put("batteryLevel", "Заряд");
    }

    public static final Map<String, List<String>> WATCH_FIELDS_ALT = new HashMap<>();
    static {
        WATCH_FIELDS_ALT.putAll(BASE_FIELDS_ALT);
        WATCH_FIELDS_ALT.put("gpsEnabled", Arrays.asList("GPS", "gps", "ГПС"));
        WATCH_FIELDS_ALT.put("timeFormat", Arrays.asList("Формат", "Format", "Формат времени", "TimeFormat"));
        WATCH_FIELDS_ALT.put("notificationsEnabled", Arrays.asList("Уведомления", "Notifications", "Уведомление"));
        WATCH_FIELDS_ALT.put("alarmCount", Arrays.asList("Будильников", "Alarms", "Будильник", "AlarmCount"));
        WATCH_FIELDS_ALT.put("currentHour", Arrays.asList("Час", "Hour", "Текущий час", "CurrentHour", "Время"));
        WATCH_FIELDS_ALT.put("steps", Arrays.asList("Шаги", "Steps", "Шаг"));
        WATCH_FIELDS_ALT.put("calories", Arrays.asList("Калорий", "Calories", "Калории", "CaloriesBurned"));
    }

    public static final Map<String, String> WATCH_FIELDS = new HashMap<>();
    static {
        WATCH_FIELDS.putAll(BASE_FIELDS);
        WATCH_FIELDS.put("gpsEnabled", "GPS");
        WATCH_FIELDS.put("timeFormat", "Формат");
        WATCH_FIELDS.put("notificationsEnabled", "Уведомления");
        WATCH_FIELDS.put("alarmCount", "Будильников");
        WATCH_FIELDS.put("currentHour", "Час");
        WATCH_FIELDS.put("steps", "Шаги");
        WATCH_FIELDS.put("calories", "Калорий");
    }

    public static final Map<String, List<String>> FITNESS_BAND_FIELDS_ALT = new HashMap<>();
    static {
        FITNESS_BAND_FIELDS_ALT.putAll(BASE_FIELDS_ALT);
        FITNESS_BAND_FIELDS_ALT.put("stepsCount", Arrays.asList("Шагов", "Steps", "StepsCount", "Шаги"));
        FITNESS_BAND_FIELDS_ALT.put("sleepTracking", Arrays.asList("Сон", "Sleep", "Трекер сна", "SleepTracking"));
        FITNESS_BAND_FIELDS_ALT.put("caloriesBurned", Arrays.asList("Калорий", "Calories", "CaloriesBurned", "Калории"));
        FITNESS_BAND_FIELDS_ALT.put("distanceKm", Arrays.asList("Дистанция", "Distance", "Дистанция км", "DistanceKm"));
        FITNESS_BAND_FIELDS_ALT.put("heartMonitorActive", Arrays.asList("Пульсометр", "HeartMonitor", "Пульс", "Heart"));
    }

    public static final Map<String, String> FITNESS_BAND_FIELDS = new HashMap<>();
    static {
        FITNESS_BAND_FIELDS.putAll(BASE_FIELDS);
        FITNESS_BAND_FIELDS.put("stepsCount", "Шагов");
        FITNESS_BAND_FIELDS.put("sleepTracking", "Сон");
        FITNESS_BAND_FIELDS.put("caloriesBurned", "Калорий");
        FITNESS_BAND_FIELDS.put("distanceKm", "Дистанция");
        FITNESS_BAND_FIELDS.put("heartMonitorActive", "Пульсометр");
    }

    public static final Map<String, List<String>> TONOMETER_FIELDS_ALT = new HashMap<>();
    static {
        TONOMETER_FIELDS_ALT.putAll(BASE_FIELDS_ALT);
        TONOMETER_FIELDS_ALT.put("systolicPressure", Arrays.asList("Систолическое", "Systolic", "Систола", "SystolicPressure"));
        TONOMETER_FIELDS_ALT.put("diastolicPressure", Arrays.asList("Диастолическое", "Diastolic", "Диастола", "DiastolicPressure"));
        TONOMETER_FIELDS_ALT.put("lastPulse", Arrays.asList("Пульс", "Pulse", "Последний пульс", "LastPulse"));
        TONOMETER_FIELDS_ALT.put("memoryEnabled", Arrays.asList("Память", "Memory", "Память включена"));
        TONOMETER_FIELDS_ALT.put("measurementsTaken", Arrays.asList("Измерений", "Measurements", "Количество измерений", "MeasurementsTaken"));
    }

    public static final Map<String, String> TONOMETER_FIELDS = new HashMap<>();
    static {
        TONOMETER_FIELDS.putAll(BASE_FIELDS);
        TONOMETER_FIELDS.put("systolicPressure", "Систолическое");
        TONOMETER_FIELDS.put("diastolicPressure", "Диастолическое");
        TONOMETER_FIELDS.put("lastPulse", "Пульс");
        TONOMETER_FIELDS.put("memoryEnabled", "Память");
        TONOMETER_FIELDS.put("measurementsTaken", "Измерений");
    }

    public static final String WATCH_KEYWORD = "Часы";
    public static final String FITNESS_BAND_KEYWORD = "Фитнес-браслет";
    public static final String TONOMETER_KEYWORD = "Тонометр";

    public static final List<String> WATCH_FIELDS_ORDER = Arrays.asList(
        "id", "userProfile", "batteryLevel", "gpsEnabled", "timeFormat", 
        "notificationsEnabled", "alarmCount", "currentHour", "steps", "calories"
    );
    
    public static final List<String> FITNESS_BAND_FIELDS_ORDER = Arrays.asList(
        "id", "userProfile", "batteryLevel", "stepsCount", "sleepTracking", 
        "caloriesBurned", "distanceKm", "heartMonitorActive"
    );
    
    public static final List<String> TONOMETER_FIELDS_ORDER = Arrays.asList(
        "id", "userProfile", "batteryLevel", "systolicPressure", "diastolicPressure", 
        "lastPulse", "memoryEnabled", "measurementsTaken"
    );
    
    /**
     * Получить порядок полей для указанного типа устройства
     */
    public static List<String> getFieldsOrderForType(String deviceType) {
        String type = deviceType.toLowerCase();
        if (type.equals("watch") || type.equals("часы")) {
            return WATCH_FIELDS_ORDER;
        } else if (type.equals("fitnessband") || type.equals("фитнес-браслет") || type.equals("браслет")) {
            return FITNESS_BAND_FIELDS_ORDER;
        } else if (type.equals("tonometer") || type.equals("тонометр")) {
            return TONOMETER_FIELDS_ORDER;
        } else {
            return Arrays.asList("id", "userProfile", "batteryLevel");
        }
    }
    
    /**
     * Получить структуру полей для указанного типа устройства
     */
    public static Map<String, String> getFieldsForType(String deviceType) {
        String type = deviceType.toLowerCase();
        if (type.equals("watch") || type.equals("часы")) {
            return WATCH_FIELDS;
        } else if (type.equals("fitnessband") || type.equals("фитнес-браслет") || type.equals("браслет")) {
            return FITNESS_BAND_FIELDS;
        } else if (type.equals("tonometer") || type.equals("тонометр")) {
            return TONOMETER_FIELDS;
        } else {
            return BASE_FIELDS;
        }
    }
    
    /**
     * Получить структуру полей с альтернативными названиями для указанного типа устройства
     */
    public static Map<String, List<String>> getFieldsAltForType(String deviceType) {
        String type = deviceType.toLowerCase();
        if (type.equals("watch") || type.equals("часы")) {
            return WATCH_FIELDS_ALT;
        } else if (type.equals("fitnessband") || type.equals("фитнес-браслет") || type.equals("браслет")) {
            return FITNESS_BAND_FIELDS_ALT;
        } else if (type.equals("tonometer") || type.equals("тонометр")) {
            return TONOMETER_FIELDS_ALT;
        } else {
            return BASE_FIELDS_ALT;
        }
    }
}

