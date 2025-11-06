package util;

import java.time.LocalDate;

public final class Config {
    private Config() {}

    public static final int CURRENT_YEAR = LocalDate.now().getYear();
    public static final double DEFAULT_SERVICE_LIFE_YEARS = 5.0;
    public static final int DEFAULT_BATTERY_HEALTH = 100;
}