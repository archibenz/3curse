import java.util.*;

public class ЛР1_gramm {

    static Map<Character, List<String>> rules = new HashMap<>();
    static Random rng = new Random();

    static boolean hasNonterminal(String chain) {
        for (char c : chain.toCharArray()) {
            if (rules.containsKey(c))
                return true;
        }
        return false;
    }

    static String generateWord() {
        String current = "S";
        int step = 0;

        System.out.printf("%-6s%-14s%s%n", "Шаг", "Правило", "Цепочка");
        System.out.println("-".repeat(40));
        System.out.printf("%-6s%-14s%s%n", "", "", current);

        while (hasNonterminal(current)) {
            for (int i = 0; i < current.length(); i++) {
                char c = current.charAt(i);
                if (rules.containsKey(c)) {
                    List<String> options = rules.get(c);
                    String replacement = options.get(rng.nextInt(options.size()));
                    String ruleStr = c + " → " + replacement;

                    current = current.substring(0, i)
                            + replacement
                            + current.substring(i + 1);

                    step++;
                    System.out.printf("%-6d%-14s%s%n", step, ruleStr, current);
                    break;
                }
            }
        }

        return current;
    }

    public static void main(String[] args) {
        rules.put('S', List.of("AA"));
        rules.put('A', List.of("aAb", "ab"));

        System.out.println("============================================");
        System.out.println("  Порождающая грамматика (Вариант 1)");
        System.out.println("============================================");
        System.out.println();
        System.out.println("Правила грамматики:");
        System.out.println("  S → AA");
        System.out.println("  A → aAb");
        System.out.println("  A → ab");
        System.out.println();

        for (int trial = 1; trial <= 5; trial++) {
            System.out.println("--- Порождение слова #" + trial + " ---");
            String word = generateWord();
            System.out.println();
            System.out.println("Результат: " + word);
            System.out.println("Длина слова: " + word.length());
            System.out.println();
        }
    }
}
