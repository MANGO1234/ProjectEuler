package problems.java;

import mango123.math.EratosthenesSieve;

public class P128 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        EratosthenesSieve sieve = new EratosthenesSieve(1000000);
        int i = 2;
        int k = 2; //first ring remove edge case 1 and 2
        // only the hexagonal vertically from 1 and the hexagons to the right of it is possible
        while (true) {
            if (sieve.isPrime(6 * i + 1) && sieve.isPrime(6 * i - 1) && sieve.isPrime(12 * i + 5)) {
                k++;
            }
            if (k == 2000) {
                long t = i;
                System.out.println(3 * t * (t - 1) + 2);
                break;
            }
            if (sieve.isPrime(6 * i + 5) && sieve.isPrime(6 * i - 1) && sieve.isPrime(12 * i - 7)) {
                k++;
            }
            if (k == 2000) {
                long t = i;
                System.out.println(3 * t * (t - 1) + 2);
                break;
            }
            i++;
        }

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
