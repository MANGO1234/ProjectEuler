import mango123.math.EratosthenesSieve;

import java.util.stream.IntStream;

public class EulerMain {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        long MOD = 10000000000000000l;
        int[] primes = new EratosthenesSieve(5000).toArray();
        int MAX = IntStream.of(primes).sum() + 1;

        long[] t = new long[MAX];
        int currentMax = 0;
        t[0] = 1;
        for (int i = 0; i < primes.length; i++) {
            currentMax += primes[i];
            for (int j = currentMax; j >= primes[i]; j--) {
                t[j] = (t[j] + t[j - primes[i]]) % MOD;
            }
            System.out.println(primes[i]);
        }

        int[] moreprimes = new EratosthenesSieve(MAX).toArray();
        long sum = 0;
        for (int j = 0; j < moreprimes.length && moreprimes[j] < t.length; j++) {
            sum = (sum + t[moreprimes[j]]) % MOD;
        }
        System.out.println(sum);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}