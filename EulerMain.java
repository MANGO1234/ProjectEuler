import mango123.math.BitEratosthenesSieve;
import mango123.math.EratosthenesSieve;
import mango123.math.PrimeSearcher;

public class EulerMain {
    public static void main(String[] args) {
        PrimeSearcher sieve = new BitEratosthenesSieve(300000000);
        System.out.println(sieve.numberOfPrimesFound());
        sieve = new EratosthenesSieve(300000000);
        System.out.println(sieve.numberOfPrimesFound());
        System.out.println("*************************");

        long START, END, k;

        START = System.currentTimeMillis();
        sieve = new EratosthenesSieve(300000000);
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
        System.out.println(sieve.numberOfPrimesFound());

        k = 0;
        START = System.currentTimeMillis();
        for (int i = 0; i < 300000000; i++) {
            if (sieve.isPrime(i)) {
                k++;
            }
        }
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
        System.out.println(k);

        k = 0;
        START = System.currentTimeMillis();
        for (int i = 0; i < 300000000; i++) {
            if (sieve.isPrimeUnsafe(i)) {
                k++;
            }
        }
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
        System.out.println(k);

        START = System.currentTimeMillis();
        sieve = new BitEratosthenesSieve(300000000);
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
        System.out.println(sieve.numberOfPrimesFound());
    }
}