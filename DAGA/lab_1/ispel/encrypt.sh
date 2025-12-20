#!/usr/bin/env bash
# encrypt.sh - простая лабораторная: 3 алгоритма, замер времени, статистика
# Алгоритмы: caesar (байтовый сдвиг), xor (повторяющийся ключ), reverse (полный реверс байтов)

FILES_DIR="./files"
OUT_DIR="./output"
EN_DIR="$OUT_DIR/encrypted"
DEC_DIR="$OUT_DIR/decrypted"
STATS_FILE="$OUT_DIR/stats.csv"

CAESAR_SHIFT=3
XOR_KEY="mysecretkey"

mkdir -p "$EN_DIR" "$DEC_DIR"

# Заголовок CSV
echo "file,algorithm,time_ms,encrypted_size_bytes,decryption_ok" > "$STATS_FILE"

# helper: время в наносекундах (fallback на секунды, если system date не поддерживает %N)
time_ns() {
  date +%s%N 2>/dev/null || echo "$(( $(date +%s) * 1000000000 ))"
}

# ====== Алгоритмы (используем perl для корректной байтовой обработки) ======

caesar_encrypt() {
  local shift="$1"; local infile="$2"; local outfile="$3"
  perl -e 'binmode STDIN; binmode STDOUT; $s=shift; while(read(STDIN,$c,1)){ print chr((ord($c)+$s) & 0xFF) }' "$shift" < "$infile" > "$outfile"
}
caesar_decrypt() {
  local shift="$1"; local infile="$2"; local outfile="$3"
  perl -e 'binmode STDIN; binmode STDOUT; $s=shift; while(read(STDIN,$c,1)){ print chr((ord($c)-$s) & 0xFF) }' "$shift" < "$infile" > "$outfile"
}

xor_process() {
  local key="$1"; local infile="$2"; local outfile="$3"
  perl -e 'binmode STDIN; binmode STDOUT; $k=shift; $kl=length($k); $i=0; while(read(STDIN,$c,1)){ print chr(ord($c) ^ ord(substr($k, $i % $kl, 1))); $i++ }' "$key" < "$infile" > "$outfile"
}
# xor decrypt == xor encrypt (симметричный)
xor_encrypt() { xor_process "$@"; }
xor_decrypt() { xor_process "$@"; }

reverse_process() {
  local infile="$1"; local outfile="$2"
  perl -0777 -e 'binmode STDIN; binmode STDOUT; local $/; $d = <STDIN>; print scalar reverse $d' < "$infile" > "$outfile"
}
reverse_encrypt() { reverse_process "$@"; }
reverse_decrypt() { reverse_process "$@"; }

# ====== Основной цикл: пробегаем по файлам и алгоритмам ======
algorithms=("caesar" "xor" "reverse")

# Проверка наличия файлов
shopt -s nullglob
files=( "$FILES_DIR"/*.txt )
if [ ${#files[@]} -eq 0 ]; then
  echo "Нет файлов в $FILES_DIR. Положи туда .txt файлы или сгенерируй (см. инструкцию)."
  exit 1
fi

for file in "${files[@]}"; do
  fname=$(basename "$file")
  for algo in "${algorithms[@]}"; do
    enc_file="$EN_DIR/${fname}_${algo}.enc"
    dec_file="$DEC_DIR/${fname}_${algo}.dec"

    start_ns=$(time_ns)

    case "$algo" in
      caesar)
        caesar_encrypt "$CAESAR_SHIFT" "$file" "$enc_file"
        ;;
      xor)
        xor_encrypt "$XOR_KEY" "$file" "$enc_file"
        ;;
      reverse)
        reverse_encrypt "$file" "$enc_file"
        ;;
      *)
        echo "Неизвестный алгоритм: $algo" ; continue
        ;;
    esac

    end_ns=$(time_ns)
    elapsed_ms=$(( (end_ns - start_ns) / 1000000 ))

    # дешифруем
    case "$algo" in
      caesar) caesar_decrypt "$CAESAR_SHIFT" "$enc_file" "$dec_file" ;;
      xor) xor_decrypt "$XOR_KEY" "$enc_file" "$dec_file" ;;
      reverse) reverse_decrypt "$enc_file" "$dec_file" ;;
    esac

    # проверка совпадения оригинала и расшифрованного
    dec_ok="no"
    if cmp -s "$file" "$dec_file"; then dec_ok="yes"; fi

    # размер зашифрованного файла (байты)
    enc_size=$(stat -c%s "$enc_file" 2>/dev/null || wc -c < "$enc_file" | tr -d ' ')

    echo "$fname,$algo,$elapsed_ms,$enc_size,$dec_ok" >> "$STATS_FILE"

    printf "Файл: %s  Алго: %s  time: %d ms  size: %s  ok:%s\n" "$fname" "$algo" "$elapsed_ms" "$enc_size" "$dec_ok"
  done
done

echo "Готово. Статистика -> $STATS_FILE"

# ======== Построение графика с помощью gnuplot ========
GNUPLOT_SCRIPT="$OUT_DIR/plot_commands.gp"
GRAPH_FILE="$OUT_DIR/stat_graph.png"

# фильтруем данные по алгоритмам (создаём временные CSV)
grep ",caesar," "$OUT_DIR/stats.csv" > "$OUT_DIR/caesar.csv"
grep ",xor," "$OUT_DIR/stats.csv" > "$OUT_DIR/xor.csv"
grep ",reverse," "$OUT_DIR/stats.csv" > "$OUT_DIR/reverse.csv"

cat > "$GNUPLOT_SCRIPT" <<'EOF'
set datafile separator ","
set terminal png size 1000,600
set output "output/stat_graph.png"
set title "Время шифрования файлов"
set xlabel "Файлы"
set ylabel "Время (мс)"
set key outside
set grid
set style data linespoints
set xtics rotate by -45

plot \
  "output/caesar.csv" using 0:3 title "Caesar" with linespoints lw 2 lc rgb "blue", \
  "output/xor.csv" using 0:3 title "XOR" with linespoints lw 2 lc rgb "red", \
  "output/reverse.csv" using 0:3 title "Reverse" with linespoints lw 2 lc rgb "green"
EOF

echo "Рисуем график..."
gnuplot "$GNUPLOT_SCRIPT" 2>/dev/null && echo "✅ График сохранён: $GRAPH_FILE" || echo "⚠️ Ошибка при построении графика"

