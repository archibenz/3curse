import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

/**
 * Лабораторная работа №4 — Использование регулярных выражений
 * Извлечение телефонных номеров из HTML-файла
 */
public class PhoneExtractor {

    private static final String PHONE_REGEX =
        "(\\+7|8)[\\s\\-]?\\(?\\d{3}\\)?[\\s\\-]?\\d{3}[\\s\\-]?\\d{2}[\\s\\-]?\\d{2}";

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Использование: java PhoneExtractor <имя_файла>");
            System.out.println("Пример: java PhoneExtractor page.html");
            return;
        }
        String filename = args[0];
        try {
            byte[] bytes = Files.readAllBytes(Paths.get(filename));
            String content = new String(bytes, StandardCharsets.UTF_8);
            List<String> phones = extractPhones(content);
            System.out.println("Файл: " + filename);
            System.out.println("Использованное регулярное выражение:");
            System.out.println("  " + PHONE_REGEX);
            System.out.println("─".repeat(50));
            System.out.println("Найдено уникальных телефонных номеров: " + phones.size());
            System.out.println("─".repeat(50));
            for (int i = 0; i < phones.size(); i++) {
                System.out.printf("%2d. %s%n", i + 1, phones.get(i));
            }
        } catch (IOException e) {
            System.err.println("Ошибка при чтении файла: " + e.getMessage());
            System.exit(1);
        }
    }

    // Нормализация: оставляем только цифры, приводим к виду 7XXXXXXXXXX
    static String normalize(String phone) {
        String digits = phone.replaceAll("[^\\d]", "");
        if (digits.startsWith("8")) {
            digits = "7" + digits.substring(1);
        }
        return digits;
    }

    // Извлечение уникальных телефонных номеров из текста
    static List<String> extractPhones(String text) {
        Pattern pattern = Pattern.compile(PHONE_REGEX);
        Matcher matcher = pattern.matcher(text);
        // ключ — нормализованный номер, значение — первое красивое представление
        Map<String, String> unique = new LinkedHashMap<>();
        while (matcher.find()) {
            String raw = matcher.group().trim();
            String key = normalize(raw);
            unique.putIfAbsent(key, raw);
        }
        return new ArrayList<>(unique.values());
    }
}