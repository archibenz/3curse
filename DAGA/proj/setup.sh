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
  sudo_wrap apt install -y build-essential pkg-config qt6-base-dev libsfml-dev
}

install_with_dnf() {
  sudo_wrap dnf install -y @development-tools pkgconf-pkg-config qt6-qtbase-devel sfml-devel
}

install_with_pacman() {
  sudo_wrap pacman -Sy --needed --noconfirm base-devel pkgconf sfml qt6-base
}

install_with_brew() {
  brew update
  brew install pkg-config qt sfml
}

install_with_zypper() {
  sudo_wrap zypper refresh
  sudo_wrap zypper install -y --type pattern devel_C_C++
  sudo_wrap zypper install -y pkgconf qt6-base-devel sfml-devel
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
echo "[*] Building project with make…"
make
echo "[*] Build complete. Run './main' or 'make run' to start."

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

  3. Then execute   make   from the project directory.
--------------------------------------------------------------------
EOF