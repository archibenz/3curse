package repository;

import model.LockDevice;

import java.util.List;

public interface DeviceRepository {
    List<LockDevice> loadDevices();
    void saveDevices(List<LockDevice> devices);
}
