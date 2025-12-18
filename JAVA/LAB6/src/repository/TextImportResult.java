package repository;

import model.LockDevice;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public record TextImportResult(
        String source,
        List<LockDevice> devices,
        int propertiesRead,
        int propertiesMissing,
        int linesScanned,
        List<String> messages
) {
    public TextImportResult {
        devices = devices == null ? List.of() : Collections.unmodifiableList(new ArrayList<>(devices));
        messages = messages == null ? List.of() : Collections.unmodifiableList(new ArrayList<>(messages));
    }

    public int objectsFound() { return devices.size(); }
}
