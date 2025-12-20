for i in $(seq 1 10); do
  base64 /dev/urandom | head -c $((1024 * i)) > "files/text${i}.txt"
done
