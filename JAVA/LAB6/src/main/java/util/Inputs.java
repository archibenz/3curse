package util;

import java.util.*;

public final class Inputs {
    private static final Scanner SC = new Scanner(System.in);

    private Inputs() {}

    public static int readInt(String prompt) {
        while (true) {
            System.out.print(prompt);
            String s = SC.nextLine().trim();
            try {
                return Integer.parseInt(s);
            } catch (NumberFormatException e) {
                System.out.println("Нужно целое число.");
            }
        }
    }

    public static int readIntInRange(String prompt, int min, int max) {
        while (true) {
            int v = readInt(prompt);
            if (v < min || v > max) {
                System.out.printf("Диапазон %d..%d%n", min, max);
            } else return v;
        }
    }

    public static String readString(String prompt) {
        System.out.print(prompt);
        return SC.nextLine();
    }

    public static boolean readYesNo(String prompt) {
        while (true) {
            System.out.print(prompt + " (y/n): ");
            String s = SC.nextLine().trim().toLowerCase();
            if (s.equals("y") || s.equals("д")) return true;
            if (s.equals("n") || s.equals("н")) return false;
            System.out.println("Ответьте y/n");
        }
    }

    public static List<Integer> readDigitsList(int size) {
        while (true) {
            System.out.print("Введите " + size + " цифр через пробел: ");
            String line = SC.nextLine().trim();
            String[] parts = line.split("\\s+");
            if (parts.length != size) {
                System.out.println("Нужно ровно " + size + " значения.");
                continue;
            }
            List<Integer> out = new ArrayList<>(size);
            boolean ok = true;
            for (String p : parts) {
                try {
                    int d = Integer.parseInt(p);
                    if (d < 0 || d > 9) { ok = false; break; }
                    out.add(d);
                } catch (NumberFormatException e) { ok = false; break; }
            }
            if (ok) return out;
            System.out.println("Только цифры 0..9.");
        }
    }
}
