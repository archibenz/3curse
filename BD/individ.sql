/* ЛР7: Индивидуальное задание MySQL — "Служба спасения"
   БД: individ
   Таблицы: RescueStation, Employee, Vehicle, EmergencyCall, StationOpenCalls
   Процедуры: MapEmployeeVehicles, RecalcStationOpenCalls, GetStationStats, ListOpenCalls, GetEmployeeLastCalls
*/

-- 0) База данных
DROP DATABASE IF EXISTS individ;
CREATE DATABASE individ
  DEFAULT CHARACTER SET utf8mb4
  DEFAULT COLLATE utf8mb4_unicode_ci;
USE individ;

-- 1) Таблицы
DROP TABLE IF EXISTS StationOpenCalls;
DROP TABLE IF EXISTS EmergencyCall;
DROP TABLE IF EXISTS Vehicle;
DROP TABLE IF EXISTS Employee;
DROP TABLE IF EXISTS RescueStation;

CREATE TABLE RescueStation (
  StationID INT AUTO_INCREMENT PRIMARY KEY,
  Name     VARCHAR(100) NOT NULL,
  City     VARCHAR(50)  NOT NULL,
  Address  VARCHAR(150) NOT NULL,
  Phone    VARCHAR(20)  NOT NULL
) ENGINE=InnoDB;

CREATE TABLE Employee (
  EmployeeID INT AUTO_INCREMENT PRIMARY KEY,
  FullName   VARCHAR(100) NOT NULL,
  Position   VARCHAR(50)  NOT NULL,
  StationID  INT NOT NULL,
  HireDate   DATE NOT NULL,
  CONSTRAINT fk_employee_station
    FOREIGN KEY (StationID) REFERENCES RescueStation(StationID)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB;

CREATE TABLE Vehicle (
  VehicleID    INT AUTO_INCREMENT PRIMARY KEY,
  StationID    INT NOT NULL,
  VehicleType  VARCHAR(50)  NOT NULL,
  PlateNumber  VARCHAR(15)  NOT NULL,
  IsAvailable  BIT NOT NULL DEFAULT b'1',
  CONSTRAINT uq_vehicle_plate UNIQUE (PlateNumber),
  CONSTRAINT fk_vehicle_station
    FOREIGN KEY (StationID) REFERENCES RescueStation(StationID)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE=InnoDB;

CREATE TABLE EmergencyCall (
  CallID               INT AUTO_INCREMENT PRIMARY KEY,
  CallTime             DATETIME NOT NULL,
  City                 VARCHAR(50)  NOT NULL,
  Address              VARCHAR(150) NOT NULL,
  Description          VARCHAR(200) NOT NULL,
  StationID            INT NOT NULL,
  VehicleID            INT NULL,
  Status               VARCHAR(20) NOT NULL,   -- 'New', 'InProgress', 'Closed'
  ResponseTimeMinutes  INT NULL,               -- NULL для незакрытых
  CONSTRAINT chk_call_status CHECK (Status IN ('New','InProgress','Closed')),
  CONSTRAINT fk_call_station
    FOREIGN KEY (StationID) REFERENCES RescueStation(StationID)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT fk_call_vehicle
    FOREIGN KEY (VehicleID) REFERENCES Vehicle(VehicleID)
    ON UPDATE CASCADE ON DELETE SET NULL
) ENGINE=InnoDB;

CREATE TABLE StationOpenCalls (
  StationID      INT PRIMARY KEY,
  OpenCallsCount INT NOT NULL,
  LastUpdate     DATETIME NOT NULL,
  CONSTRAINT fk_stationopencalls_station
    FOREIGN KEY (StationID) REFERENCES RescueStation(StationID)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE=InnoDB;

-- 2) Процедуры
DELIMITER $$

/* 2.1 MapEmployeeVehicles(OUT out_str TEXT)
   По сотрудникам показывает доступный транспорт на их станции (IsAvailable=1) */
DROP PROCEDURE IF EXISTS MapEmployeeVehicles $$
CREATE PROCEDURE MapEmployeeVehicles(OUT out_str TEXT)
BEGIN
  DECLARE v_done INT DEFAULT 0;

  DECLARE v_emp_id INT;
  DECLARE v_full_name VARCHAR(100);
  DECLARE v_position VARCHAR(50);
  DECLARE v_station_id INT;
  DECLARE v_station_name VARCHAR(100);

  DECLARE v_vehicle_list TEXT;

  DECLARE cur CURSOR FOR
    SELECT e.EmployeeID, e.FullName, e.Position, e.StationID, s.Name
    FROM Employee e
    JOIN RescueStation s ON s.StationID = e.StationID
    ORDER BY e.EmployeeID;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET v_done = 1;

  SET out_str = '';

  OPEN cur;
  read_loop: LOOP
    FETCH cur INTO v_emp_id, v_full_name, v_position, v_station_id, v_station_name;
    IF v_done = 1 THEN
      LEAVE read_loop;
    END IF;

    SELECT
      GROUP_CONCAT(CONCAT(vh.VehicleType, ' (', vh.PlateNumber, ')') ORDER BY vh.VehicleID SEPARATOR ', ')
    INTO v_vehicle_list
    FROM Vehicle vh
    WHERE vh.StationID = v_station_id AND vh.IsAvailable = b'1';

    IF v_vehicle_list IS NULL OR v_vehicle_list = '' THEN
      SET v_vehicle_list = 'нет доступного транспорта';
    END IF;

    SET out_str = CONCAT(
      out_str,
      'Сотрудник #', v_emp_id, ': ', v_full_name, ' [', v_position, ']',
      ' | Станция: ', v_station_name, ' (ID=', v_station_id, ')',
      ' | Доступный транспорт: ', v_vehicle_list,
      '\n'
    );
  END LOOP;

  CLOSE cur;
END $$

/* 2.2 RecalcStationOpenCalls()
   Пересчитывает агрегат: открытые вызовы (New/InProgress) по станциям */
DROP PROCEDURE IF EXISTS RecalcStationOpenCalls $$
CREATE PROCEDURE RecalcStationOpenCalls()
BEGIN
  DECLARE v_done INT DEFAULT 0;
  DECLARE v_station_id INT;
  DECLARE v_open_cnt INT;

  DECLARE cur CURSOR FOR
    SELECT StationID FROM RescueStation ORDER BY StationID;

  DECLARE CONTINUE HANDLER FOR NOT FOUND SET v_done = 1;

  DELETE FROM StationOpenCalls;

  OPEN cur;
  station_loop: LOOP
    FETCH cur INTO v_station_id;
    IF v_done = 1 THEN
      LEAVE station_loop;
    END IF;

    SELECT COUNT(*)
      INTO v_open_cnt
    FROM EmergencyCall
    WHERE StationID = v_station_id
      AND Status IN ('New','InProgress');

    INSERT INTO StationOpenCalls(StationID, OpenCallsCount, LastUpdate)
    VALUES (v_station_id, v_open_cnt, NOW());
  END LOOP;

  CLOSE cur;
END $$

/* 2.3 GetStationStats(IN p_station_id INT, OUT out_stats TEXT)
   Статистика по станции: всего/открытых/закрытых/среднее время реагирования */
DROP PROCEDURE IF EXISTS GetStationStats $$
CREATE PROCEDURE GetStationStats(IN p_station_id INT, OUT out_stats TEXT)
BEGIN
  DECLARE v_station_name VARCHAR(100);
  DECLARE v_station_city VARCHAR(50);

  DECLARE v_total INT DEFAULT 0;
  DECLARE v_open INT DEFAULT 0;
  DECLARE v_closed INT DEFAULT 0;
  DECLARE v_avg_resp DECIMAL(10,2);

  SELECT Name, City
    INTO v_station_name, v_station_city
  FROM RescueStation
  WHERE StationID = p_station_id;

  IF v_station_name IS NULL THEN
    SET out_stats = CONCAT('Станция с ID=', p_station_id, ' не найдена');
  ELSE
    SELECT COUNT(*)
      INTO v_total
    FROM EmergencyCall
    WHERE StationID = p_station_id;

    SELECT COUNT(*)
      INTO v_open
    FROM EmergencyCall
    WHERE StationID = p_station_id
      AND Status IN ('New','InProgress');

    SELECT COUNT(*)
      INTO v_closed
    FROM EmergencyCall
    WHERE StationID = p_station_id
      AND Status = 'Closed';

    SELECT AVG(ResponseTimeMinutes)
      INTO v_avg_resp
    FROM EmergencyCall
    WHERE StationID = p_station_id
      AND Status = 'Closed'
      AND ResponseTimeMinutes IS NOT NULL;

    SET out_stats = CONCAT(
      'Станция ', v_station_name, ' (', v_station_city, ', ID=', p_station_id, '): ',
      'всего вызовов = ', v_total, ', ',
      'открытых = ', v_open, ', ',
      'закрытых = ', v_closed, ', ',
      'среднее время реагирования = ',
      IFNULL(FORMAT(v_avg_resp, 2), 'NULL'),
      ' минут'
    );
  END IF;
END $$

/* 2.4 ListOpenCalls()
   Список открытых вызовов */
DROP PROCEDURE IF EXISTS ListOpenCalls $$
CREATE PROCEDURE ListOpenCalls()
BEGIN
  SELECT
    CallID,
    CallTime,
    City,
    Address,
    Description,
    StationID,
    VehicleID,
    Status,
    ResponseTimeMinutes
  FROM EmergencyCall
  WHERE Status IN ('New','InProgress')
  ORDER BY CallTime DESC;
END $$

/* 2.5 GetEmployeeLastCalls(IN p_full_name VARCHAR(100))
   Последние 10 вызовов по станции, где работает сотрудник */
DROP PROCEDURE IF EXISTS GetEmployeeLastCalls $$
CREATE PROCEDURE GetEmployeeLastCalls(IN p_full_name VARCHAR(100))
BEGIN
  DECLARE v_station_id INT;
  DECLARE v_station_name VARCHAR(100);
  DECLARE v_station_city VARCHAR(50);

  SELECT e.StationID, s.Name, s.City
    INTO v_station_id, v_station_name, v_station_city
  FROM Employee e
  JOIN RescueStation s ON s.StationID = e.StationID
  WHERE e.FullName = p_full_name
  LIMIT 1;

  IF v_station_id IS NULL THEN
    SELECT CONCAT('Сотрудник "', p_full_name, '" не найден') AS ErrorMessage;
  ELSE
    SELECT
      c.CallID,
      c.CallTime,
      c.City,
      c.Address,
      c.Description,
      c.Status,
      c.ResponseTimeMinutes,
      v_station_name AS StationName,
      v_station_city AS StationCity,
      v.VehicleType  AS VehicleType,
      v.PlateNumber  AS VehiclePlate
    FROM EmergencyCall c
    LEFT JOIN Vehicle v ON v.VehicleID = c.VehicleID
    WHERE c.StationID = v_station_id
    ORDER BY c.CallTime DESC
    LIMIT 10;
  END IF;
END $$

/* 2.6 SeedTestData(IN p_stations INT, IN p_employees_per_station INT, IN p_vehicles_per_station INT, IN p_calls_per_station INT)
   Генерирует "в разы" больше данных для тестов. */
DROP PROCEDURE IF EXISTS SeedTestData $$
CREATE PROCEDURE SeedTestData(
  IN p_stations INT,
  IN p_employees_per_station INT,
  IN p_vehicles_per_station INT,
  IN p_calls_per_station INT
)
BEGIN
  DECLARE s INT DEFAULT 1;
  DECLARE i INT DEFAULT 1;

  DECLARE v_city VARCHAR(50);
  DECLARE v_station_id INT;

  DECLARE v_vehicle_id INT;
  DECLARE v_vehicle_count INT;
  DECLARE v_vehicle_offset INT;

  DECLARE v_status VARCHAR(20);
  DECLARE v_resp INT;

  DECLARE v_base_dt DATETIME;
  DECLARE v_call_dt DATETIME;

  -- На всякий: очищаем (порядок важен из-за FK)
  DELETE FROM StationOpenCalls;
  DELETE FROM EmergencyCall;
  DELETE FROM Vehicle;
  DELETE FROM Employee;
  DELETE FROM RescueStation;

  -- Станции
  SET s = 1;
  WHILE s <= p_stations DO
    SET v_city =
      CASE (s MOD 8)
        WHEN 0 THEN 'Москва'
        WHEN 1 THEN 'Санкт-Петербург'
        WHEN 2 THEN 'Казань'
        WHEN 3 THEN 'Нижний Новгород'
        WHEN 4 THEN 'Екатеринбург'
        WHEN 5 THEN 'Новосибирск'
        WHEN 6 THEN 'Самара'
        ELSE 'Ростов-на-Дону'
      END;

    INSERT INTO RescueStation(Name, City, Address, Phone)
    VALUES (
      CONCAT('ПС-', s, ' ', v_city),
      v_city,
      CONCAT('ул. Тестовая, д.', 10 + s),
      CONCAT('+7-900-', LPAD(s, 3, '0'), '-', LPAD(10 + s, 2, '0'), '-', LPAD(20 + s, 2, '0'))
    );

    SET s = s + 1;
  END WHILE;

  -- Сотрудники + транспорт + вызовы по каждой станции
  SET s = 1;
  WHILE s <= p_stations DO
    -- текущая StationID (так как AUTO_INCREMENT)
    SELECT StationID INTO v_station_id
    FROM RescueStation
    WHERE Name = CONCAT('ПС-', s, ' ',
      CASE (s MOD 8)
        WHEN 0 THEN 'Москва'
        WHEN 1 THEN 'Санкт-Петербург'
        WHEN 2 THEN 'Казань'
        WHEN 3 THEN 'Нижний Новгород'
        WHEN 4 THEN 'Екатеринбург'
        WHEN 5 THEN 'Новосибирск'
        WHEN 6 THEN 'Самара'
        ELSE 'Ростов-на-Дону'
      END
    )
    LIMIT 1;

    SELECT City INTO v_city FROM RescueStation WHERE StationID = v_station_id;

    -- сотрудники
    SET i = 1;
    WHILE i <= p_employees_per_station DO
      INSERT INTO Employee(FullName, Position, StationID, HireDate)
      VALUES (
        CONCAT('Сотрудник ', s, '-', i),
        CASE (i MOD 6)
          WHEN 0 THEN 'Диспетчер'
          WHEN 1 THEN 'Спасатель'
          WHEN 2 THEN 'Водитель'
          WHEN 3 THEN 'Врач'
          WHEN 4 THEN 'Инженер'
          ELSE 'Руководитель смены'
        END,
        v_station_id,
        DATE_SUB(CURDATE(), INTERVAL (200 + s*15 + i*7) DAY)
      );
      SET i = i + 1;
    END WHILE;

    -- транспорт
    SET i = 1;
    WHILE i <= p_vehicles_per_station DO
      INSERT INTO Vehicle(StationID, VehicleType, PlateNumber, IsAvailable)
      VALUES (
        v_station_id,
        CASE (i MOD 7)
          WHEN 0 THEN 'Пожарная машина'
          WHEN 1 THEN 'Скорая помощь'
          WHEN 2 THEN 'Катер'
          WHEN 3 THEN 'Вертолёт'
          WHEN 4 THEN 'Аварийный фургон'
          WHEN 5 THEN 'Квадроцикл'
          ELSE 'Дрон'
        END,
        -- гарантируем уникальность и длину <= 15
        CONCAT('T', LPAD(s,2,'0'), LPAD(i,3,'0'), 'AA'),
        IF((i MOD 4)=0, b'0', b'1')
      );
      SET i = i + 1;
    END WHILE;

    -- сколько транспорта на станции
    SELECT COUNT(*) INTO v_vehicle_count FROM Vehicle WHERE StationID = v_station_id;

    -- вызовы
    SET v_base_dt = DATE_SUB(NOW(), INTERVAL (60 + s*5) DAY);

    SET i = 1;
    WHILE i <= p_calls_per_station DO
      -- статус по шаблону: больше закрытых, но есть New/InProgress
      SET v_status =
        CASE (i MOD 10)
          WHEN 0 THEN 'New'
          WHEN 1 THEN 'InProgress'
          WHEN 2 THEN 'InProgress'
          ELSE 'Closed'
        END;

      -- время вызова: равномерно по минутам
      SET v_call_dt = DATE_ADD(v_base_dt, INTERVAL (i * (7 + (s MOD 5))) MINUTE);

      -- выбираем транспорт на станции "по кругу"; для части вызовов ставим NULL (не назначен)
      IF (i MOD 13) = 0 THEN
        SET v_vehicle_id = NULL;
      ELSE
        SET v_vehicle_offset = (i - 1) MOD v_vehicle_count;
        SELECT VehicleID INTO v_vehicle_id
        FROM Vehicle
        WHERE StationID = v_station_id
        ORDER BY VehicleID
        LIMIT v_vehicle_offset, 1;
      END IF;

      -- время реагирования только для Closed
      IF v_status = 'Closed' THEN
        SET v_resp = 4 + ((i + s) MOD 27); -- 4..30 минут
      ELSE
        SET v_resp = NULL;
      END IF;

      INSERT INTO EmergencyCall
        (CallTime, City, Address, Description, StationID, VehicleID, Status, ResponseTimeMinutes)
      VALUES
        (
          v_call_dt,
          v_city,
          CONCAT('ул. ', CASE (i MOD 8)
            WHEN 0 THEN 'Лесная'
            WHEN 1 THEN 'Садовая'
            WHEN 2 THEN 'Мира'
            WHEN 3 THEN 'Победы'
            WHEN 4 THEN 'Ленина'
            WHEN 5 THEN 'Гагарина'
            WHEN 6 THEN 'Невская'
            ELSE 'Центральная'
          END, ', д.', 1 + ((i * 3 + s) MOD 180)),
          CASE (i MOD 9)
            WHEN 0 THEN 'Задымление в помещении'
            WHEN 1 THEN 'Пожар в квартире'
            WHEN 2 THEN 'ДТП, пострадавшие'
            WHEN 3 THEN 'Утечка газа'
            WHEN 4 THEN 'Потеря сознания'
            WHEN 5 THEN 'Травма, нужна помощь'
            WHEN 6 THEN 'Пожар в офисе'
            WHEN 7 THEN 'Застрял лифт'
            ELSE 'Обнаружен подозрительный запах/дым'
          END,
          v_station_id,
          v_vehicle_id,
          v_status,
          v_resp
        );

      SET i = i + 1;
    END WHILE;

    SET s = s + 1;
  END WHILE;

  -- агрегаты
  CALL RecalcStationOpenCalls();
END $$

DELIMITER ;

CALL SeedTestData(10, 15, 8, 120);

-- 4) Примеры проверок (можно выполнять вручную)
-- SELECT COUNT(*) AS stations FROM RescueStation;
-- SELECT COUNT(*) AS employees FROM Employee;
-- SELECT COUNT(*) AS vehicles FROM Vehicle;
-- SELECT COUNT(*) AS calls_total FROM EmergencyCall;
-- SELECT * FROM StationOpenCalls ORDER BY StationID;

-- SET @res := '';
-- CALL MapEmployeeVehicles(@res);
-- SELECT @res;

-- SET @info := '';
-- CALL GetStationStats(1, @info);
-- SELECT @info;

-- CALL ListOpenCalls();

-- CALL GetEmployeeLastCalls('Сотрудник 1-1');