import java.util.*;

public class lab1 {
    private static final Scanner SC = new Scanner(System.in);

    public static void main(String[] args) {
        System.out.println("статические массивы");
        int[] a = readArray(3, "A (3 элемента)");
        int[] b = readArray(5, "B (5 элементов)");
        int[] c = readArray(4, "C (4 элемента)");
        int[] d = readArray(3, "D (3 элемента)");

        System.out.println("Исходные массивы:");
        printArray("A", a);
        printArray("B", b);
        printArray("C", c);
        printArray("D", d);

        changeSign(a); changeSign(b); changeSign(c); changeSign(d);
        System.out.println("\nПосле смены знака:");
        printArray("A", a);
        printArray("B", b);
        printArray("C", c);
        printArray("D", d);

        long prodA = product(a);
        long prodB = product(b);
        long prodC = product(c);
        long prodD = product(d);
        System.out.printf("\nПроизведения массивов: A=%d, B=%d, C=%d, D=%d%n", prodA, prodB, prodC, prodD);

        int[] merged = mergeArrays(a, b, c, d);
        printArray("Новый массив (A+B+C+D)", merged);

        System.out.println("\nдинамические массивы");
        List<Integer> l1 = readList(3, "L1 (ArrayList, 3 элемента)", true);   
        List<Integer> l2 = readList(5, "L2 (LinkedList, 5 элементов)", false); 
        List<Integer> l3 = readList(4, "L3 (ArrayList, 4 элемента)", true);  
        List<Integer> l4 = readList(3, "L4 (LinkedList, 3 элемента)", false); 

        System.out.println("Исходные списки:");
        printList("L1", l1);
        printList("L2", l2);
        printList("L3", l3);
        printList("L4", l4);

        changeSign(l1); changeSign(l2); changeSign(l3); changeSign(l4);
        System.out.println("\nПосле смены знака (List):");
        printList("L1", l1);
        printList("L2", l2);
        printList("L3", l3);
        printList("L4", l4);

        long p1 = product(l1);
        long p2 = product(l2);
        long p3 = product(l3);
        long p4 = product(l4);
        System.out.printf("\nПроизведения списков: L1=%d, L2=%d, L3=%d, L4=%d%n", p1, p2, p3, p4);

        List<Integer> lMerged = mergeLists(l1, l2, l3, l4);
        printList("Новый список (L1+L2+L3+L4)", lMerged);
    }

    private static int[] readArray(int n, String label) {
        int[] arr = new int[n];
        System.out.printf("Введите %d целых числа для массива %s:%n", n, label);
        for (int i = 0; i < n; i++) {
            arr[i] = readInt(String.format("[%d] = ", i));
        }
        return arr;
    }

    private static void printArray(String name, int[] a) {
        System.out.printf("%s: %s%n", name, Arrays.toString(a));
    }

    private static void changeSign(int[] a) {
        for (int i = 0; i < a.length; i++) a[i] = -a[i];
    }

    private static long product(int[] a) {
        long p = 1L;
        for (int v : a) p *= v;
        return p;
    }

    private static int[] mergeArrays(int[]... arrays) {
        int total = 0;
        for (int[] x : arrays) total += x.length;
        int[] res = new int[total];
        int k = 0;
        for (int[] x : arrays) for (int v : x) res[k++] = v;
        return res;
    }

    private static List<Integer> readList(int n, String label, boolean arrayList) {
        final List<Integer> list = arrayList ? new ArrayList<>() : new LinkedList<>();
        System.out.printf("Введите %d целых числа для списка %s:%n", n, label);
        for (int i = 0; i < n; i++) list.add(readInt(String.format("[%d] = ", i)));
        return list;
    }

    private static void printList(String name, List<Integer> list) {
        System.out.printf("%s: %s%n", name, list.toString());
    }

    private static void changeSign(List<Integer> list) {
        ListIterator<Integer> it = list.listIterator();
        while (it.hasNext()) {
            int val = it.next();
            it.set(-val);
        }
    }

    private static long product(List<Integer> list) {
        long p = 1L;
        for (int v : list) p *= v;
        return p;
    }

    private static List<Integer> mergeLists(List<Integer>... lists) {
        List<Integer> res = new ArrayList<>();
        for (List<Integer> lst : lists) res.addAll(lst);
        return res;
    }

    private static int readInt(String prompt) {
        while (true) {
            System.out.print(prompt);
            if (SC.hasNextInt()) {
                return SC.nextInt();
            } else {
                System.out.println("Ошибка ввода. Введите целое число.");
                SC.next();
            }
        }
    }
}
