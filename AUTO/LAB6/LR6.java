import java.util.*;

public class LR6 {

    // Типы токенов
    enum TokenType {
        WHILE, LPAREN, RPAREN, LBRACE, RBRACE,
        VAR, INT_LIT, LESS, GREATER, EQUAL, NOT_EQUAL,
        PLUS, MINUS, ASSIGN, SEMICOLON, EOF
    }

    // Лексический анализатор
    static List<String[]> tokenize(String input) {
        List<String[]> tokens = new ArrayList<>();
        int i = 0;
        while (i < input.length()) {
            char c = input.charAt(i);
            if (Character.isWhitespace(c)) { i++; continue; }

            if (input.startsWith("while", i) &&
                (i+5 >= input.length() || !Character.isLetterOrDigit(input.charAt(i+5)))) {
                tokens.add(new String[]{"WHILE", "while"});
                i += 5; continue;
            }
            if (input.startsWith("==", i)) {
                tokens.add(new String[]{"EQUAL", "=="});
                i += 2; continue;
            }
            if (input.startsWith("!=", i)) {
                tokens.add(new String[]{"NOT_EQUAL", "!="});
                i += 2; continue;
            }

            switch (c) {
                case '(': tokens.add(new String[]{"LPAREN","("}); break;
                case ')': tokens.add(new String[]{"RPAREN",")"}); break;
                case '{': tokens.add(new String[]{"LBRACE","{"}); break;
                case '}': tokens.add(new String[]{"RBRACE","}"}); break;
                case '<': tokens.add(new String[]{"LESS","<"}); break;
                case '>': tokens.add(new String[]{"GREATER",">"}); break;
                case '+': tokens.add(new String[]{"PLUS","+"}); break;
                case '-': tokens.add(new String[]{"MINUS","-"}); break;
                case '=': tokens.add(new String[]{"ASSIGN","="}); break;
                case ';': tokens.add(new String[]{"SEMICOLON",";"}); break;
                default:
                    if (Character.isDigit(c)) {
                        int start = i;
                        while (i < input.length() && Character.isDigit(input.charAt(i))) i++;
                        tokens.add(new String[]{"INT_LIT", input.substring(start, i)});
                        continue;
                    }
                    if (Character.isLetter(c) || c == '_') {
                        int start = i;
                        while (i < input.length() &&
                               (Character.isLetterOrDigit(input.charAt(i)) || input.charAt(i)=='_')) i++;
                        tokens.add(new String[]{"VAR", input.substring(start, i)});
                        continue;
                    }
                    System.out.println("  Неизвестный символ: " + c);
                    return null;
            }
            i++;
        }
        tokens.add(new String[]{"EOF", ""});
        return tokens;
    }

    // Поля парсера
    static List<String[]> tokens;
    static int pos;

    static String currentType() { return tokens.get(pos)[0]; }
    static String currentVal()  { return tokens.get(pos)[1]; }

    static boolean match(String type) {
        if (currentType().equals(type)) { pos++; return true; }
        return false;
    }

    // S -> while ( C ) { L }
    static boolean parseS() {
        return match("WHILE") && match("LPAREN") && parseC()
            && match("RPAREN") && match("LBRACE") && parseL()
            && match("RBRACE");
    }

    // C -> E R E
    static boolean parseC() {
        return parseE() && parseR() && parseE();
    }

    // R -> < | > | == | !=
    static boolean parseR() {
        return match("LESS") || match("GREATER")
            || match("EQUAL") || match("NOT_EQUAL");
    }

    // E -> var | int
    static boolean parseE() {
        return match("VAR") || match("INT_LIT");
    }

    // L -> A ; L' , L' -> A ; L' | eps
    static boolean parseL() {
        if (!parseA()) return false;
        if (!match("SEMICOLON")) return false;
        while (!currentType().equals("RBRACE") && !currentType().equals("EOF")) {
            if (!parseA()) return false;
            if (!match("SEMICOLON")) return false;
        }
        return true;
    }

    // A -> var = E
    static boolean parseA() {
        return match("VAR") && match("ASSIGN") && parseE();
    }

    public static void main(String[] args) {
        String[] tests = {
            "while (x < 10) { y = 5; }",
            "while (a != b) { x = 1; y = 2; }",
            "while (i > 0) { count = i; }",
            "while x < 10) { y = 5; }",
            "while (x < ) { y = 5; }",
            "while (x < 10) { y = ; }"
        };

        for (String test : tests) {
            System.out.println("Вход: " + test);
            List<String[]> t = tokenize(test);
            if (t == null) {
                System.out.println("Результат: ошибка лексического анализа\n");
                continue;
            }
            System.out.print("Токены: ");
            for (String[] tk : t) {
                if (!tk[0].equals("EOF"))
                    System.out.print(tk[0] + "(" + tk[1] + ") ");
            }
            System.out.println();
            tokens = t;
            pos = 0;
            boolean ok = parseS() && currentType().equals("EOF");
            System.out.println("Результат: " +
                (ok ? "строка СООТВЕТСТВУЕТ грамматике"
                    : "строка НЕ соответствует грамматике"));
            System.out.println();
        }
    }
}
