#!/usr/bin/env python3
"""
Строит график «кол-во потоков — время» из CSV,
который создаёт our::test_generation_time_by_thread_num().
"""
import sys, csv, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

if len(sys.argv) != 3:
    sys.exit("usage: gen_speed_plot.py <csv> <png>")

csv_file, out_png = sys.argv[1], sys.argv[2]
from collections import defaultdict, OrderedDict

mode_labels = {
    "single_thread": "Без потоков",
    "sync_shared": "Синхр. общий лабиринт",
    "no_sync_independent": "Без синхр. независимые лабиринты",
}

with open(csv_file, newline="") as f:
    rdr = csv.DictReader(f)
    has_mode = "Режим" in rdr.fieldnames
    has_threads = "Количество потоков" in rdr.fieldnames

    if has_mode and has_threads:
        acc = defaultdict(lambda: defaultdict(list))
        for row in rdr:
            mode = row["Режим"]
            th = int(row["Количество потоков"])
            tm = float(row["время"])
            acc[mode][th].append(tm)

        plt.figure()
        for mode, data in acc.items():
            threads = sorted(data.keys())
            avg_times = [sum(data[t]) / len(data[t]) for t in threads]
            plt.plot(threads, avg_times, marker="o", label=mode_labels.get(mode, mode))
        plt.legend()
    elif has_mode:
        acc = defaultdict(list)
        for row in rdr:
            mode = row["Режим"]
            tm = float(row["время"])
            acc[mode].append(tm)

        modes = list(acc.keys())
        avg_times = [sum(acc[m]) / len(acc[m]) for m in modes]
        labels = [mode_labels.get(m, m) for m in modes]
        plt.figure()
        plt.bar(labels, avg_times, color=["#5DADE2", "#F5B041"])
        plt.xticks(rotation=10)
    else:
        acc = defaultdict(list)
        for row in rdr:
            th = int(row["Количество потоков"])
            tm = float(row["время"])
            acc[th].append(tm)

        threads = sorted(acc.keys())
        avg_times = [sum(acc[t]) / len(acc[t]) for t in threads]
        plt.figure()
        plt.plot(threads, avg_times, marker="o")

plt.xlabel("Количество потоков")
plt.ylabel("Среднее время, мс")
plt.title("Сравнение скорости генерации")
plt.grid(True)
plt.tight_layout()
plt.savefig(out_png)
