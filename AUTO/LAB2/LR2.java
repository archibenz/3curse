import java.util.*;

/**
 * Лабораторная работа №2 — «Конечные автоматы»
 * Вариант 1: распознавание языка L = { w ∈ {0,1}* | w заканчивается на "00" }
 * Реализация детерминированного конечного автомата (ДКА) с тремя состояниями.
 */
public class LR2 {

    // Функция переходов δ, заданная таблично: δ(qi, j) = transitions[i][j]
    // Индекс строки — номер текущего состояния, индекс столбца — входной символ
    static int[][] transitions = {
        {1, 0},  // δ(q0, 0) = q1,  δ(q0, 1) = q0
        {2, 0},  // δ(q1, 0) = q2,  δ(q1, 1) = q0
        {2, 0}   // δ(q2, 0) = q2,  δ(q2, 1) = q0
    };

    // Начальное состояние q0
    static int startState = 0;

    // Множество допускающих состояний F = {q2}
    static Set<Integer> acceptStates = Set.of(2);

    /**
     * Моделирование работы ДКА на входном слове.
     * Последовательно применяется функция переходов δ к каждому символу.
     * Выводится протокол переходов и результат распознавания.
     */
    static void checkWord(String word) {
        int state = startState;

        System.out.printf("%-6s%-12s%s%n", "Шаг", "Символ", "Состояние");
        System.out.println("-".repeat(30));
        System.out.printf("%-6s%-12s%s%n", "", "", "q" + state);

        // Обработка входной цепочки: на каждом шаге выполняется переход δ(state, symbol)
        for (int i = 0; i < word.length(); i++) {
            int symbol = word.charAt(i) - '0'; // преобразование символа в индекс столбца
            int nextState = transitions[state][symbol];
            System.out.printf("%-6d%-12s%s%n", i + 1, word.charAt(i), "q" + nextState);
            state = nextState;
        }

        System.out.println();
        System.out.println("Конечное состояние: q" + state);

        // Проверка принадлежности: слово ∈ L(M) ⟺ конечное состояние ∈ F
        if (acceptStates.contains(state)) {
            System.out.println("Результат: слово \"" + word + "\" ПРИНАДЛЕЖИТ языку L");
        } else {
            System.out.println("Результат: слово \"" + word + "\" НЕ ПРИНАДЛЕЖИТ языку L");
        }
    }

    public static void main(String[] args) {
        System.out.println("=============================================");
        System.out.println("  Конечный автомат (Вариант 1)");
        System.out.println("=============================================");
        System.out.println();
        System.out.println("Язык L: все слова над {0, 1}, заканчивающиеся на \"00\"");
        System.out.println("Состояния: q0 (начальное), q1, q2 (допускающее)");
        System.out.println();
        System.out.println("Таблица переходов:");
        System.out.println("  q0 --0--> q1    q0 --1--> q0");
        System.out.println("  q1 --0--> q2    q1 --1--> q0");
        System.out.println("  q2 --0--> q2    q2 --1--> q0");
        System.out.println();

        // Тестовый набор: 4 слова ∈ L (заканчиваются на "00"), 2 слова ∉ L
        String[] testWords = {"100", "1100", "0100", "111", "00", "10010"};

        for (int i = 0; i < testWords.length; i++) {
            System.out.println("--- Проверка слова #" + (i + 1) + ": \"" + testWords[i] + "\" ---");
            checkWord(testWords[i]);
            System.out.println();
        }
    }
}