package problems.java;

import mango123.math.EulerMath;

public class P86 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // binary search M by hand...
        int M = 1818;
        int c = 0;
        for (long i = 2; i <= M; i += 1) {
            for (long j = 1; j < i; j++) {
                if (EulerMath.GCD(i, j) == 1 && (i + j) % 2 == 1) {
                    long t1 = i * i - j * j;
                    long t2 = 2 * i * j;
                    long a = Math.min(t1, t2);
                    long b = Math.max(t1, t2);
                    for (long a1 = a, b1 = b; a1 <= M || b1 <= M; a1 += a, b1 += b) {
                        if (a1 <= M) {
                            c += Math.max(0, (2 * a1 - b1) / 2 + 1);
                        }
                        if (b1 <= M) {
                            c += a1 / 2;
                        }
                    }
                }
            }
        }
        System.out.println(c);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
