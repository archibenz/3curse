@echo off
REM --------------------------------------------------------------------
REM  setup.bat – автоматическая установка зависимостей + сборка проекта
REM  Проверено на Windows 10/11.  Требуется свободные ~3 ГБ на диске C:.
REM --------------------------------------------------------------------
setlocal EnableDelayedExpansion

:: 1. Находим или ставим MSYS2
echo [*] Checking for MSYS2…
set "MSYS_ROOT="
for %%d in ("%ProgramFiles%\MSYS2" "C:\msys64") do (
    if exist "%%~d\usr\bin\bash.exe" set "MSYS_ROOT=%%~d"
)
if not defined MSYS_ROOT (
    echo     → MSYS2 not found. Downloading installer…
    set "MSYS2_URL=https://github.com/msys2/msys2-installer/releases/latest/download/msys2-x86_64-20240116.exe"
    powershell -NoProfile -Command ^
      "Invoke-WebRequest -Uri '%MSYS2_URL%' -OutFile '%TEMP%\msys2_inst.exe'"
    start /wait "" "%TEMP%\msys2_inst.exe" /silent
    if errorlevel 1 (
        echo error: MSYS2 installer failed. Aborting.
        exit /b 1
    )
    set "MSYS_ROOT=C:\msys64"
)

set "BASH=%MSYS_ROOT%\usr\bin\bash.exe"
echo     → MSYS2 found at %MSYS_ROOT%

:: 2. Обновляем MSYS2, ставим компилятор + Qt 6 + SFML + make
echo [*] Installing build toolchain…
%BASH% -lc "pacman -Syuu --noconfirm"
%BASH% -lc "pacman -S --needed --noconfirm ^
             base-devel ^
             mingw-w64-ucrt-x86_64-toolchain ^
             mingw-w64-ucrt-x86_64-qt6 ^
             mingw-w64-ucrt-x86_64-sfml ^
             mingw-w64-ucrt-x86_64-pkgconf" || (
    echo error: package install failed. Aborting.
    exit /b 1
)

:: 3. Конвертируем текущий путь в Unix-вид
for /f "usebackq tokens=* delims=" %%P in (`"%BASH%" -lc "cygpath -au \"%cd%\""` ) do set "PRJ_DIR=%%P"

:: 4. Сборка (make по-умолчанию берёт Makefile из корня)
echo [*] Building project…
%BASH% -lc "cd '%PRJ_DIR%' && make -j$(nproc)" || (
    echo error: build failed.
    exit /b 1
)

echo.
echo [✓] Build complete.
echo     ──────────────────────────────────────────────
echo     Запуск:
echo       • двойной клик main.exe из %cd% (если Makefile кладёт exe рядом)
echo       • или   %BASH% -lc \"cd '%PRJ_DIR%' && make run\"
echo.
pause
endlocal