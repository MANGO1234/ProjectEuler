package problems.java;

import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;

public class P429 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        EratosthenesSieve sieve = new EratosthenesSieve(100000000);
        long k = 1;
        for (int p : sieve.toArray()) {
            int n = 0;
            int t = 100000000;
            while (t > 1) {
                t /= p;
                n += t;
            }
            k = k * (1 + EulerMath.powMod(p, n * 2, 1000000009)) % 1000000009;
        }
        System.out.println(k);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
