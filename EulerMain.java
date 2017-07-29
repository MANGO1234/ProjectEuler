import mango123.math.BitEratosthenesSieve;
import mango123.math.EratosthenesSieve;
import mango123.math.PrimeSearcher;

public class EulerMain {
    public static void main(String[] args) {
        PrimeSearcher sieve = new BitEratosthenesSieve(300000000);
        System.out.println(sieve.numberOfPrimesFound());
        PrimeSearcher sieve2 = new EratosthenesSieve(300000000);
        System.out.println(sieve2.numberOfPrimesFound());
        System.out.println("*************************");

        long START, END, k;
        int prime, prime2;

        START = System.currentTimeMillis();
        sieve = new EratosthenesSieve(300000000);
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
        System.out.println(sieve.numberOfPrimesFound());

        START = System.currentTimeMillis();
        prime = sieve.firstPrime();
        while (prime != -1) {
            prime = sieve.nextPrime();
        }
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");

        START = System.currentTimeMillis();
        sieve2 = new BitEratosthenesSieve(300000000);
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
        System.out.println(sieve2.numberOfPrimesFound());

        START = System.currentTimeMillis();
        prime = sieve2.firstPrime();
        while (prime != -1) {
            prime = sieve2.nextPrime();
        }
        END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}