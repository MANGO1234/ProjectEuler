package problems.java;

import mango123.util.ArraysUtils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class P146 {

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        int[] MS = new int[] { 2, 3, 5, 7, 11, 13, 17, 19 };
        int M = (int) ArraysUtils.product(MS);
        boolean[] w = new boolean[M];
        Arrays.fill(w, true);
        for (int i : MS) {
            for (int j = 0; j < M; j += i) {
                w[j] = false;
            }
        }
        List<Integer> ns = new ArrayList<>();
        for (int i = 0; i < w.length; i++) {
            if (w[(i + 1) % M] && w[(i + 3) % M] && w[(i + 7) % M] && w[(i + 9) % M] && w[(i + 13) % M] && w[(i + 27) % M]) {
                ns.add(i);
            }
        }
        w = ArraysUtils.intListToBool(M, ns);
        int k = 0;
        for (long i = 0; i < M; i++) {
            if (w[(int) (i * i % M)]) {
                long[] p1 = new long[] { 1, 3, 7, 9, 13, 27 };
                long[] p2 = new long[] { 5, 11, 15, 17, 19, 21, 23, 25 };
                for (long j = i; j <= 150000000; j += M) {
                    long sq = j * j;
                    boolean p = true;
                    for (long add : p1) {
                        BigInteger t = BigInteger.valueOf(sq + add);
                        if (!t.isProbablePrime(20)) {
                            p = false;
                            break;
                        }
                    }
                    if (p) {
                        for (long add : p2) {
                            BigInteger t = BigInteger.valueOf(sq + add);
                            if (t.isProbablePrime(20)) {
                                p = false;
                                break;
                            }
                        }
                    }
                    if (p) {
                        System.out.println("." + j);
                        k += j;
                    }
                }
            }
        }
        System.out.println(k);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

}
