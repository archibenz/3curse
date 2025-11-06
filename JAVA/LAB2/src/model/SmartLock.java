package model;

public class SmartLock {
    private int id;
    private String brand;
    private String model;
    private int productionYear;
    private String protocol;
    private int batteryHealth;
    private boolean locked;
    private double serviceLifeYears;

    public SmartLock() { }

    public SmartLock(
            int id,
            String brand,
            String model,
            int productionYear,
            String protocol,
            int batteryHealth,
            boolean locked,
            double serviceLifeYears
    ) {
        this.id = id;
        this.brand = brand;
        this.model = model;
        this.productionYear = productionYear;
        this.protocol = protocol;
        setBatteryHealth(batteryHealth);
        this.locked = locked;
        setServiceLifeYears(serviceLifeYears);
    }

    public void lock()  { this.locked = true; }
    public void unlock(){ this.locked = false; }

    public int yearsInUse(int currentYear) {
        return Math.max(0, currentYear - productionYear);
    }

    public void replaceBattery() { this.batteryHealth = 100; }

    public void degradeBattery(int byPercent) {
        this.batteryHealth = Math.max(0, this.batteryHealth - Math.abs(byPercent));
    }

    public int getId() { return id; }
    public void setId(int id) { this.id = id; }

    public String getBrand() { return brand; }
    public void setBrand(String brand) { this.brand = brand; }

    public String getModel() { return model; }
    public void setModel(String model) { this.model = model; }

    public int getProductionYear() { return productionYear; }
    public void setProductionYear(int productionYear) { this.productionYear = productionYear; }

    public String getProtocol() { return protocol; }
    public void setProtocol(String protocol) { this.protocol = protocol; }

    public int getBatteryHealth() { return batteryHealth; }
    public void setBatteryHealth(int batteryHealth) {
        if (batteryHealth < 0) batteryHealth = 0;
        if (batteryHealth > 100) batteryHealth = 100;
        this.batteryHealth = batteryHealth;
    }

    public boolean isLocked() { return locked; }
    public void setLocked(boolean locked) { this.locked = locked; }

    public double getServiceLifeYears() { return serviceLifeYears; }
    public void setServiceLifeYears(double serviceLifeYears) {
        if (serviceLifeYears < 0) serviceLifeYears = 0;
        this.serviceLifeYears = serviceLifeYears;
    }

    @Override
    public String toString() {
        return "SmartLock{" +
                "id=" + id +
                ", brand='" + brand + '\'' +
                ", model='" + model + '\'' +
                ", year=" + productionYear +
                ", protocol='" + protocol + '\'' +
                ", batteryHealth=" + batteryHealth + "%" +
                ", locked=" + locked +
                ", serviceLifeYears=" + serviceLifeYears +
                '}';
    }
}