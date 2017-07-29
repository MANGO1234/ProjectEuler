package mango123.math;

import java.util.BitSet;

/**
 * A bit version of the EratosthenesSieve. 8 times more search range.
 *
 * @author De Li
 * @version 0.1
 */
public class BitEratosthenesSieve implements PrimeSearcher {

    // sieve: holds the sieve of boolean value for primes
    // ** The sieve indexes correspond to odd numbers starting with 3.
    //    e.g. index 0 is the number 3, index 1 is the number 5, etc.
    //    To convert an index to the number it represents, multiply it by 2 then add 3.
    private BitSet sieve;

    // sieveSize: the size of the sieve it represents
    // numberOfPrimeInSieve: number of primes found
    // actualSieveSize: b/c BitSieve has a capacity (e.g. uses nearest multiple of 64)
    // this will be used to store the actual amount of bits use in BitSet
    private int sieveSize = 0;
    private int numOfPrimesInSieve = 0;

    /**
     * Creates a EratosthenesSieve up to n. n is minimum 10.
     * <p>
     * If n is smaller than 10, this will use 10 as the value of n.
     *
     * @param n the size of the sieve to be constructed
     */
    public BitEratosthenesSieve(int n) {
        createSieve(n < 10 ? 10 : n);
    }

    /**
     * Returns the nth prime.
     * <p>
     * This will return -1 if the sieve is not large enough to contain the nth prime.
     * In that case, you can try increasing the sieve's size with .expandSearchUpTo(int n).
     * <p>
     * Note: this is inefficient and slower than using .firstPrime() and .nextPrime()
     *
     * @param n
     * @return the nth prime
     */
    @Override
    public int nthPrime(int n) {
        if (n < 1 || n > numOfPrimesInSieve) {
            return -1;
        } else if (n == 1) {
            return 2;
        }

        int count = 1;
        int index = sieve.nextSetBit(0);
        do {
            if (++count == n) {
                return (index << 1) + 3;
            }
            index = sieve.nextSetBit(index + 1);
        } while (true);
    }

    /**
     * Determines whether a number is prime.
     * <p>
     * Note: If the number is larger than the sieve's size, a larger sieve will be
     * automatically created to check whether it's prime or not. Note this can be very
     * expensive.
     *
     * @param n the number to be checked
     * @return a boolean value containing whether it's prime or not
     */
    @Override
    public boolean isPrime(int n) {
        //manually account for 2 and negative/even numbers
        if ((n & 1) == 0) {
            return n == 2;
        } else if (n < 2) {
            return false;
        } else if (n <= sieveSize) {
            //if it's within the sieve, just return the value inside the sieve
            return sieve.get((n >> 1) - 1);
        } else {
            //otherwise, test primality using .testLargeNumber()
            return testLargeNumber(n);
        }
    }

    /**
     * Increase the sieve's size up to n.
     * <p>
     * This is pretty inefficient as it creates a new sieve of n to replace the internal sieve.
     *
     * @param n if n is smaller than the sieve's current size, this method does nothing
     */
    @Override
    public void expandSearchUpTo(int n) {
        if (n > sieveSize) {
            createSieve(n);
        }
    }

    /**
     * Returns the number of primes found in the sieve.
     *
     * @return number of primes in sieve
     */
    public int numberOfPrimesFound() {
        return numOfPrimesInSieve;
    }

    /**
     * Returns the sieve's size.
     *
     * @return the sieve's size
     */
    @Override
    public int size() {
        return sieveSize;
    }

    /**
     * Return all the primes in the sieve, listed in ascending numeric order, as an integer array.
     */
    @Override
    public int[] toArray() {
        int[] primes = new int[numOfPrimesInSieve];
        primes[0] = 2;
        int index = 0, count = 1;
        while (index != -1) {
            primes[count++] = (index << 1) + 3;
            index = sieve.nextSetBit(index + 1);
        }
        return primes;
    }

    //create a new sieve of size n. Use in the constructor and .expandSearchUpTo();
    private void createSieve(int size) {
        int actualSieveSize = size / 2;
        sieve = new BitSet(actualSieveSize);
        sieve.set(0, actualSieveSize);

        //Optimizations include:
        // - starts off at i * i while sieving (small gain)
        // - no multiples of 2 (respectable gain, not only in performance, but also in memory)
        //***note*** all the +3, -3, *2, /2 etc. is to account for using indexes to represent odd numbers
        int index = -1;
        for (int bound = (((int) Math.sqrt(size)) >> 1) - 1; ; ) {
            index = sieve.nextSetBit(index + 1);
            if (index > bound) {
                break;
            }

            for (int skip = (index << 1) + 3, j = (skip * skip - 3) >> 1; j < actualSieveSize; j += skip) {
                sieve.set(j, false);
            }
        }

        //update information about the sieve
        sieveSize = size;
        numOfPrimesInSieve = sieve.cardinality() + 1; // +1 to account for the prime 2
    }

    //this test whether a number beyond the sieve for primality using simple prime divisibilty test.
    //The sieve will be resized if there is not enough prime to determine the number's primality
    private boolean testLargeNumber(int number) {
        //if there are not enough primes, then expand the sieve
        if (sieveSize < number) {
            expandSearchUpTo((int) Math.sqrt(number));
        }

        int bound = (((int) Math.sqrt(number) + 1) >> 1) - 1;
        for (int i = 0; i >= 0 && i < bound; ) {
            if (number % ((i << 1) + 3) == 0) {
                return false;
            }
            i = sieve.nextSetBit(i + 1);
        }
        return true;
    }

    //**************************************************************************************
    // ************************************ Iteration **************************************
    //**************************************************************************************
    private int currentPrimeIndex;

    @Override
    public int firstPrime() {
        currentPrimeIndex = 0;
        return 2;
    }

    @Override
    public int nextPrime() {
        if (currentPrimeIndex < 0) {
            return -1;
        }
        int returnIndex = sieve.nextSetBit(currentPrimeIndex);
        if (returnIndex < 0) {
            currentPrimeIndex = returnIndex;
            return -1;
        } else {
            currentPrimeIndex = returnIndex + 1;
            return (returnIndex << 1) + 3;
        }
    }
}
