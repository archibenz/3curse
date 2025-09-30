import java.util.*;

public class test {
    private static final Scanner SC = new Scanner(System.in);

    public static void main(String[] args){
        System.out.println("Vvod massivov:");
        int[] a = readArray(3, "A (3elements)");
        int[] b = readArray(5, "B (5elements)");
        int[] c = readArray(4, "C (4elements)");
        int[] d = readArray(3, "D (3elements)");

        System.out.println("Ishodniye massivi:");
        printArray("A", a);
        printArray("B", b);
        printArray("C", c);
        printArray("D", d);

        multiply(a);multiply(b);multiply(c);multiply(d);

        System.out.println("Massivi s izmenennim znakom:");
        printArray("A", a);
        printArray("B", b);
        printArray("C", c);
        printArray("D", d);

        int productA = multiplyArray(a);
        int productB = multiplyArray(b);
        int productC = multiplyArray(c);
        int productD = multiplyArray(d);

        System.out.println("Proizvedenie A = " + productA);
        System.out.println("Proizvedenie B = " + productB);
        System.out.println("Proizvedenie C = " + productC);
        System.out.println("Proizvedenie D = " + productD);

    }

    private static int[] readArray(int n, String txt) {
        int[] arr = new int[n];
        System.out.printf("Vvedite %d chisla dlya massiva %s:%n", n, txt);
        for (int i = 0; i < n; i++) {
            arr[i] = readInt(String.format("[%d] = ", i));
        }
        return arr;
    }
    private static void printArray(String title, int[] a) {
        System.out.printf("%s: %s%n", title, Arrays.toString(a));
    }
    private static int readInt(String arr) {
        while (true) {
            System.out.print(arr);
            if (SC.hasNextInt()) {
                return SC.nextInt();
            } else {
                System.out.println("Oshibka vvoda. Vvedite celoe chislo.");
                SC.next();
            }
        }
    }
    private static void multiply(int[] arr) {
        for (int i = 0; i < arr.length; i++) {
            arr[i] = arr[i] * -1;
        }
    }
    private static int multiplyArray(int[] arr) {
        int product = 1;
        for (int value : arr) {
            product *= value;
        }
        return product;
    }
}