# БД "Служба спасения" (MySQL, БД `individ`)

## 1. Структура таблиц (деревом)

individ
├─ RescueStation          -- станции
│  ├─ StationID (PK, INT)
│  ├─ Name (NVARCHAR(100))
│  ├─ City (NVARCHAR(50))
│  ├─ Address (NVARCHAR(150))
│  └─ Phone (VARCHAR(20))
│
├─ Employee               -- сотрудники
│  ├─ EmployeeID (PK, INT)
│  ├─ FullName (NVARCHAR(100))
│  ├─ Position (NVARCHAR(50))
│  ├─ StationID (FK → RescueStation.StationID)
│  └─ HireDate (DATE)
│
├─ Vehicle                -- транспорт
│  ├─ VehicleID (PK, INT)
│  ├─ StationID (FK → RescueStation.StationID)
│  ├─ VehicleType (NVARCHAR(50))
│  ├─ PlateNumber (VARCHAR(15))
│  └─ IsAvailable (BIT)   -- 1 доступен, 0 недоступен
│
├─ EmergencyCall          -- вызовы
│  ├─ CallID (PK, INT)
│  ├─ CallTime (DATETIME)
│  ├─ City (NVARCHAR(50))
│  ├─ Address (NVARCHAR(150))
│  ├─ Description (NVARCHAR(200))
│  ├─ StationID (FK → RescueStation.StationID)
│  ├─ VehicleID (FK → Vehicle.VehicleID, NULL)
│  ├─ Status (NVARCHAR(20))           -- 'New', 'InProgress', 'Closed'
│  └─ ResponseTimeMinutes (INT, NULL) -- время реагирования
│
└─ StationOpenCalls       -- агрегат по открытым вызовам
   ├─ StationID (PK, FK → RescueStation.StationID)
   ├─ OpenCallsCount (INT)   -- сколько открытых вызовов
   └─ LastUpdate (DATETIME)  -- когда пересчитано

## 2. Хранимые процедуры

2.1. MapEmployeeVehicles(OUT out_str TEXT)
Назначение: формирует текстовый отчёт по всем сотрудникам и доступному им транспорту (IsAvailable = 1).
Параметры: OUT out_str TEXT — строка с отчётом.
Пример:
SET @res := '';
CALL MapEmployeeVehicles(@res);
SELECT @res;

2.2. RecalcStationOpenCalls()
Назначение: пересчитывает количество открытых вызовов (Status IN ('New','InProgress')) по станциям и заполняет StationOpenCalls.
Параметры: нет.
Пример:
CALL RecalcStationOpenCalls();
SELECT * FROM StationOpenCalls;

2.3. GetStationStats(IN p_station_id INT, OUT out_stats TEXT)
Назначение: возвращает статистику по станции (всего/открытые/закрытые/среднее время реагирования).
Параметры: IN p_station_id INT, OUT out_stats TEXT.
Пример:
SET @info := '';
CALL GetStationStats(1, @info);
SELECT @info;

2.4. ListOpenCalls()
Назначение: выводит все открытые вызовы (Status IN ('New','InProgress')).
Параметры: нет.
Пример:
CALL ListOpenCalls();