#!/usr/bin/env bash
#
# setup.sh – one-click installer + builder for the maze project
# Поддерживает:  macOS (Homebrew), Debian/Ubuntu, Fedora/RHEL, Arch/Manjaro, openSUSE
# Для Windows (MSYS2) инструкция внизу скрипта.
#
set -e

# ── вспомогательные функции ────────────────────────────────────────────────
need_cmd()  { command -v "$1" >/dev/null 2>&1; }
err()       { echo "error: $*" >&2; exit 1; }
sudo_wrap() { if need_cmd sudo; then sudo "$@"; else "$@"; fi; }

install_with_apt() {
  sudo_wrap apt update
  sudo_wrap apt install -y build-essential cmake pkg-config qt6-base-dev
}

install_with_dnf() {
  sudo_wrap dnf install -y @development-tools cmake pkgconf-pkg-config qt6-qtbase-devel
}

install_with_pacman() {
  sudo_wrap pacman -Sy --needed --noconfirm base-devel cmake pkgconf qt6-base
}

install_with_brew() {
  brew update
  brew install cmake pkg-config qt
}

install_with_zypper() {
  sudo_wrap zypper refresh
  sudo_wrap zypper install -y --type pattern devel_C_C++
  sudo_wrap zypper install -y cmake pkgconf qt6-base-devel
}

# ── выбор пакетного менеджера ──────────────────────────────────────────────
echo "[*] Detecting package manager…"
if   need_cmd brew;   then PM=brew
elif need_cmd apt;    then PM=apt
elif need_cmd dnf;    then PM=dnf
elif need_cmd pacman; then PM=pacman
elif need_cmd zypper; then PM=zypper
else err "No supported package manager (apt, dnf, pacman, zypper, brew) found."
fi
echo "    → $PM"

# ── установка зависимостей ─────────────────────────────────────────────────
echo "[*] Installing build dependencies…"
case $PM in
  apt)     install_with_apt    ;;
  dnf)     install_with_dnf    ;;
  pacman)  install_with_pacman ;;
  brew)    install_with_brew   ;;
  zypper)  install_with_zypper ;;
esac
echo "[*] Dependencies installed."

# ── сборка проекта ─────────────────────────────────────────────────────────
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_DIR="$ROOT_DIR/build"

echo "[*] Building project with CMake…"
cmake -S "$ROOT_DIR" -B "$BUILD_DIR" -DCMAKE_BUILD_TYPE=Release
cmake --build "$BUILD_DIR" --config Release
echo "[*] Build complete. Run the binary from '$BUILD_DIR'."

# ── подсказка для Windows/MSYS2 ────────────────────────────────────────────
cat <<'EOF'

--------------------------------------------------------------------
Windows / MSYS2 users:
  1. Install MSYS2 from https://www.msys2.org
  2. Open the "MSYS2 MinGW UCRT64" shell and run:

     pacman -Syu --needed mingw-w64-ucrt-x86_64-toolchain \
                      mingw-w64-ucrt-x86_64-qt6 \
                      mingw-w64-ucrt-x86_64-sfml \
                      mingw-w64-ucrt-x86_64-pkgconf

  3. Then execute:

     cmake -S . -B build
     cmake --build build
--------------------------------------------------------------------
EOF
