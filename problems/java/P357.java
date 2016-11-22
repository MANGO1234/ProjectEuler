package problems.java;

import mango123.math.EratosthenesSieve;

public class P357 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();
        EratosthenesSieve sieve = new EratosthenesSieve(100000000);
        long k = 0;
        int[] primes = sieve.toArray();
        // somewhere on forum, brute forcing is fast vs what I did below
        for (int p : primes) {
            int n = p - 1;
            boolean fit = true;
            for (int j = 1; j * j <= n; j++) {
                if (n % j == 0) {
                    if (!sieve.isPrime(j + n / j)) {
                        fit = false;
                        break;
                    }
                }
            }
            if (fit) {
                k += n;
            }
        }

        // so what I learned: notsquarefree has no impact whatsoever
        // factorsOfNumSorted is much slower than just brute force ~6s vs 1.3s
        //        int[] ps = new EratosthenesSieve(10000).toArray();
        //        boolean[] notsquarefree = new boolean[100000001];
        //        for (int p : ps) {
        //            int p2 = p * p;
        //            for (int i = p2; i < 100000000; i += p2) {
        //                notsquarefree[i] = true;
        //            }
        //        }
        //        for (int p : sieve.toArray()) {
        //            if (!notsquarefree[p - 1]) {
        //                long[] fs = PrimesUtil.factorsOfNumSorted(p - 1);
        //                boolean fit = true;
        //                for (int j = 0; j < fs.length / 2; j++) {
        //                    if (!sieve.isPrime((int) (fs[j] + fs[fs.length - 1 - j]))) {
        //                        fit = false;
        //                        break;
        //                    }
        //                }
        //                if (fit) {
        //                    k += p - 1;
        //                }
        //            }
        //        }
        System.out.println(k + 1);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

}
