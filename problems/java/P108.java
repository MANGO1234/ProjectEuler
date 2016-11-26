package problems.java;

import mango123.math.PrimesUtil;

public class P108 {

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // well obviously for 110 i need to do it differently
        // probably bottom up using primes multiplication instead of top down using factoring
        for (int i = 0; i < 1000000; i++) {
            long[] primes = PrimesUtil.primeFactorsOf(i);
            int len = 1;
            long last = 0;
            int lastCount = 0;
            for (long prime : primes) {
                if (prime == last) {
                    lastCount++;
                } else {
                    len *= lastCount * 2 + 1;
                    last = prime;
                    lastCount = 1;
                }
            }
            len *= lastCount * 2 + 1;
            if (len > 2 * 1000) {
                System.out.println(i);
                break;
            }
        }

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

}
