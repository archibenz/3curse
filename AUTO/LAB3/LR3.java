import java.util.*;

/**
 * Лабораторная работа №3 — «Автоматы Мура и Мили»
 * Вариант 1: вендинговый автомат, монеты 1, 2, 5, 10 руб., товар 2 руб.
 * Реализация автомата Мили: выход определяется парой (состояние, вход).
 */
public class LR3 {

    // Допустимые номиналы монет; индекс = столбец в таблицах δ и λ
    static int[] coins = {1, 2, 5, 10};

    // Таблица переходов δ(qi, coins[j]) = nextState[i][j]
    static int[][] nextState = {
        {1, 0, 0, 0},  // q0: +1→q1, +2→q0, +5→q0, +10→q0
        {0, 0, 0, 0}   // q1: +1→q0, +2→q0, +5→q0, +10→q0
    };

    // Таблица выходов λ(qi, coins[j]): размер сдачи; -1 = «ничего не выдавать»
    static int[][] output = {
        {-1, 0, 3, 8},  // q0
        { 0, 1, 4, 9}   // q1
    };

    static String formatOutput(int out) {
        if (out == -1) return "—";
        if (out == 0)  return "ВЫДАТЬ ТОВАР";
        return "ВЫДАТЬ ТОВАР + сдача " + out + " руб";
    }

    // Моделирование автомата: на каждом шаге вычисляются δ и λ
    static void simulate(int[] seq) {
        int state = 0;
        System.out.printf("%-6s%-12s%-10s%-30s%n", "Шаг", "Монета", "Сост.", "Выход");
        System.out.println("-".repeat(58));
        System.out.printf("%-6s%-12s%s%n", "", "", "q" + state);
        for (int i = 0; i < seq.length; i++) {
            int idx = -1;
            for (int j = 0; j < coins.length; j++)
                if (coins[j] == seq[i]) idx = j;
            int out = output[state][idx];
            int ns  = nextState[state][idx];
            System.out.printf("%-6d%-12s%-10s%-30s%n",
                i + 1, seq[i] + " руб", "q" + ns, formatOutput(out));
            state = ns;
        }
    }

    public static void main(String[] args) {
        System.out.println("============================================");
        System.out.println("  Автомат Мили — Вендинговый автомат (Вар.1)");
        System.out.println("============================================");
        System.out.println("Монеты: 1, 2, 5, 10 руб  |  Цена товара: 2 руб");
        System.out.println();

        // Тестовые последовательности монет
        int[][] tests  = {{2}, {1,1}, {1,2}, {5}, {10}, {1,5}};
        String[] labels = {
            "Тест #1: [2]",    "Тест #2: [1, 1]",
            "Тест #3: [1, 2]", "Тест #4: [5]",
            "Тест #5: [10]",   "Тест #6: [1, 5]"
        };
        for (int t = 0; t < tests.length; t++) {
            System.out.println("--- " + labels[t] + " ---");
            simulate(tests[t]);
            System.out.println();
        }
    }
}
