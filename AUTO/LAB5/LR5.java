import java.util.*;
import java.util.regex.*;

/**
 * Лабораторная работа №5 — Лексический анализатор для языка Java.
 * Разбивает входную строку на токены с помощью регулярных выражений.
 */
public class LR5 {

    // Перечисление всех типов токенов, которые умеет распознавать лексер
    enum TokenType {
        COMMENT,        // однострочный // и многострочный /* */
        STRING,         // строковый литерал "..."
        CHAR,           // символьный литерал '.'
        REAL_NUMBER,    // вещественное число (3.14, 1.0e5)
        INT_NUMBER,     // целое число (42, 0xFF, 0b101)
        KEYWORD,        // ключевое слово (if, while, class, ...)
        BOOLEAN,        // логический литерал (true, false)
        NULL_LITERAL,   // null-литерал
        IDENTIFIER,     // имя переменной, метода, класса
        OPERATOR,       // оператор (+, ==, &&, ...)
        SEPARATOR,      // разделитель ({ } ( ) ; , .)
        WHITESPACE,     // пробельные символы
        UNKNOWN         // нераспознанный символ
    }

    // Класс-обёртка для хранения пары (тип токена, значение лексемы)
    static class Token {
        final TokenType type;
        final String value;

        Token(TokenType type, String value) {
            this.type = type;
            this.value = value;
        }

        @Override
        public String toString() {
            // Экранируем спецсимволы для читаемого вывода в консоль
            String display = value.replace("\n", "\\n")
                                  .replace("\t", "\\t");
            return String.format("%-14s  %s", type, display);
        }
    }

    // Множество всех ключевых слов Java (48 штук) для быстрой проверки через contains()
    private static final Set<String> KEYWORDS = new HashSet<>(Arrays.asList(
        "abstract", "assert", "boolean", "break", "byte", "case",
        "catch", "char", "class", "continue", "default", "do",
        "double", "else", "enum", "extends", "final", "finally",
        "float", "for", "if", "implements", "import", "instanceof",
        "int", "interface", "long", "native", "new", "package",
        "private", "protected", "public", "return", "short",
        "static", "strictfp", "super", "switch", "synchronized",
        "this", "throw", "throws", "transient", "try", "void",
        "volatile", "while"
    ));

    /*
     * Единое регулярное выражение, объединяющее паттерны всех типов токенов
     * через оператор альтернативы (|). Каждый паттерн помещён в именованную
     * группу (?<ИМЯ>...) для определения типа сработавшего совпадения.
     *
     * Порядок альтернатив задаёт приоритет распознавания:
     *   1) COMMENT  — чтобы содержимое комментария не стало другими токенами
     *   2) STRING   — чтобы содержимое строки не разбилось на части
     *   3) CHAR     — аналогично строкам
     *   4) REAL     — до INT, иначе 3.14 разобьётся на 3, точку и 14
     *   5) INT      — целые числа (десятичные, hex, binary)
     *   6) WORD     — буквенные последовательности (далее делим на KW/ID)
     *   7) OP       — операторы (сначала длинные >>>, потом короткие +)
     *   8) SEP      — разделители
     *   9) WS       — пробельные символы
     */
    private static final Pattern TOKEN_PATTERN = Pattern.compile(
        "(?<COMMENT>//[^\\n]*|/\\*[\\s\\S]*?\\*/)" +
        "|(?<STRING>\"(?:[^\"\\\\]|\\\\.)*\")" +
        "|(?<CHAR>'(?:[^'\\\\]|\\\\.)')" +
        "|(?<REAL>\\d+\\.\\d*(?:[eE][+-]?\\d+)?)" +
        "|(?<INT>0[xX][0-9a-fA-F]+|0[bB][01]+|\\d+)" +
        "|(?<WORD>[A-Za-z_$][A-Za-z0-9_$]*)" +
        "|(?<OP>>>>|<<=|>>=|==|!=|<=|>=|&&|\\|\\||\\+\\+|--|<<|>>" +
        "|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|->" +
        "|[+\\-*/%&|^~!<>=?:])" +
        "|(?<SEP>[{}()\\[\\];,.])" +
        "|(?<WS>\\s+)"
    );

    /**
     * Основной метод лексического анализа.
     * Проходит по строке слева направо, на каждой позиции ищет совпадение
     * с TOKEN_PATTERN и определяет тип токена по сработавшей именованной группе.
     */
    public static List<Token> tokenize(String input) {
        List<Token> tokens = new ArrayList<>();
        Matcher m = TOKEN_PATTERN.matcher(input);
        int pos = 0; // текущая позиция во входной строке

        while (pos < input.length()) {
            // Ищем совпадение начиная с текущей позиции
            if (m.find(pos) && m.start() == pos) {
                String value = m.group(); // найденная лексема
                TokenType type;

                // Определяем тип по сработавшей именованной группе
                if (m.group("COMMENT") != null) {
                    type = TokenType.COMMENT;
                } else if (m.group("STRING") != null) {
                    type = TokenType.STRING;
                } else if (m.group("CHAR") != null) {
                    type = TokenType.CHAR;
                } else if (m.group("REAL") != null) {
                    type = TokenType.REAL_NUMBER;
                } else if (m.group("INT") != null) {
                    type = TokenType.INT_NUMBER;
                } else if (m.group("WORD") != null) {
                    // Слово найдено — нужна доп. проверка: это KW, boolean, null или ID?
                    if ("true".equals(value) || "false".equals(value)) {
                        type = TokenType.BOOLEAN;
                    } else if ("null".equals(value)) {
                        type = TokenType.NULL_LITERAL;
                    } else if (KEYWORDS.contains(value)) {
                        type = TokenType.KEYWORD;
                    } else {
                        type = TokenType.IDENTIFIER;
                    }
                } else if (m.group("OP") != null) {
                    type = TokenType.OPERATOR;
                } else if (m.group("SEP") != null) {
                    type = TokenType.SEPARATOR;
                } else if (m.group("WS") != null) {
                    type = TokenType.WHITESPACE;
                } else {
                    type = TokenType.UNKNOWN;
                }

                tokens.add(new Token(type, value));
                pos = m.end(); // сдвигаемся за конец найденной лексемы
            } else {
                // Символ не подошёл ни под один паттерн — помечаем как UNKNOWN
                tokens.add(new Token(TokenType.UNKNOWN,
                    String.valueOf(input.charAt(pos))));
                pos++;
            }
        }
        return tokens;
    }

    public static void main(String[] args) {
        // Если аргументы не переданы — используем тестовый фрагмент кода
        String input = (args.length > 0)
            ? String.join(" ", args)
            : "int x = 42; // инициализация\n"
            + "if (x >= 10 && x < 100) {\n"
            + "    double pi = 3.14;\n"
            + "    String msg = \"hello\\nworld\";\n"
            + "    System.out.println(msg);\n"
            + "}";

        System.out.println("Входная строка:");
        System.out.println("─".repeat(60));
        System.out.println(input);
        System.out.println("─".repeat(60));
        System.out.println();

        // Запускаем лексический анализ
        List<Token> tokens = tokenize(input);

        // Выводим результат (пробелы пропускаем — они не несут нагрузки)
        System.out.println("Результат лексического анализа:");
        System.out.println("─".repeat(60));
        System.out.printf("%-14s  %s%n", "ТИП ТОКЕНА", "ЗНАЧЕНИЕ");
        System.out.println("─".repeat(60));

        for (Token t : tokens) {
            if (t.type != TokenType.WHITESPACE) {
                System.out.println(t);
            }
        }
        System.out.println("─".repeat(60));

        // Подсчёт статистики: сколько токенов каждого типа найдено
        Map<TokenType, Integer> stats = new TreeMap<>();
        for (Token t : tokens) {
            if (t.type != TokenType.WHITESPACE) {
                stats.merge(t.type, 1, Integer::sum);
            }
        }
        System.out.println("\nСтатистика:");
        int total = 0;
        for (var e : stats.entrySet()) {
            System.out.printf("  %-14s: %d%n", e.getKey(), e.getValue());
            total += e.getValue();
        }
        System.out.println("  Всего токенов : " + total);
    }
}
