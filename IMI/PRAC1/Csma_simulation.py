"""
Скрипт 2.3. Оценка производительности протокола np-CSMA
Переписан с MATLAB на Python. Интерактивная версия.
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')  # без GUI — работает на любой macOS
import matplotlib.pyplot as plt
import os
import sys
import subprocess
import tempfile

# ── Параметры протокола np-CSMA ──────────────────────────────
STANDBY   = 0
TRANSMIT  = 1
COLLISION = 2
TOTAL     = 1000

Brate = 0.25e6
Plen  = 500
Ttime = Plen / Brate
Dtime = 0.1
delay = Dtime * Ttime
Mnum  = 100

# Временная папка для превью графиков
TEMP_DIR = tempfile.mkdtemp(prefix="csma_")


def choose_traffic():
    """Выбор диапазона нагрузки G."""
    print("\n╔══════════════════════════════════════════════╗")
    print("║   Выберите диапазон нагрузки G:              ║")
    print("╠══════════════════════════════════════════════╣")
    print("║  1) G = 0.1..1 + 1..20  [как в книге]        ║")
    print("║  2) G = 0.1 .. 5   (низкая нагрузка)         ║")
    print("║  3) G = 0.1 .. 10  (средняя нагрузка)        ║")
    print("║  4) G = 0.1 .. 20  (высокая нагрузка)        ║")
    print("║  5) Ввести свой диапазон                     ║")
    print("╚══════════════════════════════════════════════╝")

    choice = input("Ваш выбор [1]: ").strip() or "1"

    if choice == "1":
        return np.concatenate([np.arange(0.1, 1.0 + 0.01, 0.2),
                               np.arange(1, 20 + 0.01, 1)])
    elif choice == "2":
        return np.arange(0.1, 5.0 + 0.01, 0.5)
    elif choice == "3":
        return np.arange(0.1, 10.0 + 0.01, 1.0)
    elif choice == "4":
        return np.arange(0.1, 20.0 + 0.01, 1.0)
    elif choice == "5":
        g_start = float(input("  G начало: "))
        g_end   = float(input("  G конец:  "))
        g_step  = float(input("  G шаг:    "))
        return np.arange(g_start, g_end + 1e-9, g_step)
    else:
        return np.concatenate([np.arange(0.1, 1.0 + 0.01, 0.2),
                               np.arange(1, 20 + 0.01, 1)])


def run_simulation(G):
    """Запуск моделирования для заданного вектора G."""
    num_points = len(G)
    Traffic = np.zeros(num_points)
    S       = np.zeros(num_points)
    Delay   = np.zeros(num_points)

    print(f"\nМоделирование: {num_points} точек, TOTAL={TOTAL} кадров, {Mnum} станций")
    print("─" * 55)

    for indx in range(num_points):
        g = G[indx]
        Tint = -Ttime / np.log(1 - g / Mnum)
        Rint = Tint

        Spnum = 0
        Splen = 0
        Tplen = 0
        Wtime = 0.0

        mgtime = -Tint * np.log(1 - np.random.rand(Mnum))
        mtime  = mgtime.copy()
        Mstate = np.zeros(Mnum, dtype=int)
        Mplen  = np.full(Mnum, Plen)
        now_time = np.min(mtime)
        Mstime = np.zeros(Mnum)

        while Spnum < TOTAL:
            idx = np.where((mtime == now_time) & (Mstate == TRANSMIT))[0]
            if len(idx) > 0:
                Spnum += 1
                Splen += np.sum(Mplen[idx])
                Wtime += np.sum(now_time - mgtime[idx])
                Mstate[idx] = STANDBY
                mgtime[idx] = now_time - Tint * np.log(1 - np.random.rand(len(idx)))
                mtime[idx]  = mgtime[idx]

            idx = np.where((mtime == now_time) & (Mstate == COLLISION))[0]
            if len(idx) > 0:
                Mstate[idx] = STANDBY
                mtime[idx]  = now_time - Rint * np.log(1 - np.random.rand(len(idx)))

            idx = np.where((mtime == now_time) & (Mstate == STANDBY))[0]
            if len(idx) > 0:
                Tplen += np.sum(Mplen[idx])
                for ii in range(len(idx)):
                    jj = idx[ii]
                    idx1 = np.where(
                        (Mstime + delay <= now_time) &
                        (now_time <= Mstime + delay + Ttime)
                    )[0]
                    if len(idx1) == 0:
                        Mstate[jj] = TRANSMIT
                        Mstime[jj] = now_time
                        mtime[jj]  = now_time + Mplen[jj] / Brate
                    else:
                        mtime[jj] = now_time - Rint * np.log(1 - np.random.rand())

            idx = np.where((Mstate == TRANSMIT) | (Mstate == COLLISION))[0]
            if len(idx) > 1:
                Mstate[idx] = COLLISION

            now_time = np.min(mtime)

        Traffic[indx] = Tplen / Brate / now_time
        S[indx]       = Splen / Brate / now_time
        Delay[indx]   = Wtime / TOTAL * Brate / Plen

        pct = (indx + 1) / num_points * 100
        print(f"  [{pct:5.1f}%]  G={g:5.1f}  Traffic={Traffic[indx]:.4f}  S={S[indx]:.4f}  D={Delay[indx]:.1f}")

    print("─" * 55)
    print("Моделирование завершено!")
    return Traffic, S, Delay


def build_figure(Traffic, S, Delay, title_suffix=""):
    """Построение графиков, возвращает fig."""
    alpha = 0.1
    Stheory = Traffic * np.exp(-alpha * Traffic) / \
              (Traffic * (1 + 2 * alpha) + np.exp(-alpha * Traffic))

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(9, 8))

    ax1.plot(Traffic, Stheory, '-k', linewidth=1.5, label='теория')
    ax1.plot(Traffic, S, 'ro', markersize=5, label='моделирование')
    ax1.set_xlabel('G')
    ax1.set_ylabel('S')
    ax1.legend()
    ax1.set_title(f'Производительность протокола CSMA {title_suffix}')
    ax1.grid(True)

    ax2.plot(Traffic, Delay, '-bo', markersize=5)
    ax2.set_xlabel('G')
    ax2.set_ylabel('D')
    ax2.set_title(f'Задержка в протоколе CSMA {title_suffix}')
    ax2.grid(True)

    plt.tight_layout()
    return fig


def show_plot(fig, run_number):
    """Сохраняет во временный файл и открывает в Preview."""
    tmp_path = os.path.join(TEMP_DIR, f"csma_preview_{run_number}.png")
    fig.savefig(tmp_path, dpi=150)
    subprocess.Popen(["open", tmp_path])
    print(f"  График открыт.")
    return tmp_path


def save_plot(fig):
    """Сохранение графика в выбранный файл."""
    default = "csma_result.png"
    name = input(f"  Имя файла [{default}]: ").strip() or default
    if not name.lower().endswith(('.png', '.pdf', '.svg', '.jpg')):
        name += '.png'
    fig.savefig(name, dpi=150)
    full = os.path.abspath(name)
    print(f"  ✓ Сохранено: {full}")


def post_menu(fig, preview_path):
    """Меню после завершения моделирования."""
    while True:
        print("\n╔══════════════════════════════════════════════╗")
        print("║            Что делать дальше?                ║")
        print("╠══════════════════════════════════════════════╣")
        print("║  1) Сохранить текущий график                 ║")
        print("║  2) Новое моделирование (удалить график)     ║")
        print("║  3) Новое моделирование (оставить график)    ║")
        print("║  4) Выход                                    ║")
        print("╚══════════════════════════════════════════════╝")

        ch = input(">>> ").strip()

        if ch == "1":
            save_plot(fig)
        elif ch == "2":
            plt.close(fig)
            if os.path.exists(preview_path):
                os.remove(preview_path)
                print("  Превью удалено.")
            return "new"
        elif ch == "3":
            plt.close(fig)
            return "new"
        elif ch == "4":
            plt.close("all")
            # чистим все превью
            for f in os.listdir(TEMP_DIR):
                os.remove(os.path.join(TEMP_DIR, f))
            os.rmdir(TEMP_DIR)
            print("\nДо свидания!")
            return "exit"
        else:
            print("  Неверный ввод, попробуйте снова.")


# ── Главный цикл программы ───────────────────────────────────
def main():
    run_number = 0
    print("=" * 55)
    print("  np-CSMA: Дискретно-событийное моделирование")
    print("=" * 55)

    while True:
        run_number += 1
        G = choose_traffic()
        Traffic, S, Delay = run_simulation(G)

        suffix = f"(#{run_number})" if run_number > 1 else ""
        fig = build_figure(Traffic, S, Delay, suffix)
        preview_path = show_plot(fig, run_number)

        action = post_menu(fig, preview_path)
        if action == "exit":
            break


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        plt.close("all")
        print("\nПрервано (Ctrl+C).")
        sys.exit(0)