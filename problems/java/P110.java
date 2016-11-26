package problems.java;

import mango123.math.EratosthenesSieve;

public class P110 {

    private static long n;

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // a number with >8000000 factors
        n = 2l * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 * 29 * 31 * 37 * 41 * 43 * 47;
        int[] ps = new EratosthenesSieve(100).toArray();
        count(1, 1, 50, ps, 0);
        System.out.println(n);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

    private static void count(long current, int count, int pow, int[] ps, int i) {
        int k = pow;
        for (int j = 0; j < pow; j++) {
            current *= ps[i];
            if (current >= n) {
                k = j;
                break;
            }
            count(current, count * (2 * j + 3), j + 1, ps, i + 1);
            if (count * (2 * j + 3) > 8000000) {
                n = current;
                return;
            }
        }
    }
}
