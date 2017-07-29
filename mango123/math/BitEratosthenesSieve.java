package mango123.math;

import java.util.BitSet;

/**
 * A bit version of the EratosthenesSieve. 8 times more search range.
 *
 * @author De Li
 * @version 0.1
 */
public class BitEratosthenesSieve implements PrimeSearcher {

    //sieve: holds the sieve of boolean value for primes
    // ** The sieve indexes correspond to odd numbers starting with 3.
    //    e.g. index 0 is the number 3, index 1 is the number 5, etc.
    //    To convert an index to the number it represents, multiply it by 2 then add 3.
    private BitSet sieve;

    //sieveSize: the size of the sieve it represents
    //numberOfPrimeInSieve: number of primes found
    //actualSieveSize: b/c BitSieve has a capacity (e.g. uses 64 bit as returned by .size() when ask only for
    //49 bit of storage), this will be used to store the actual amount of bits use in BitSet
    private int sieveSize = 0,
            numOfPrimesInSieve = 0,
            actualSieveSize = 0;

    /**
     * Creates a EratosthenesSieve up to n. n is minimum 100.
     * <p>
     * If n is smaller or equal to 100, this will use 100 as the value of n.
     *
     * @param n the size of the sieve to be constructed
     */
    public BitEratosthenesSieve(int n) {
        if (n < 10)
            n = 10;
        createSieve(n);
    }

    /**
     * Returns the nth prime.
     * <p>
     * This will return -1 if the sieve is not large enough to contain the nth prime. In that case, you can
     * try increasing the sieve's size with .expandSearchUpTo(int n).
     * <p>
     * Note: this is inefficient and slower than using .nextPrime() and .previousPrime() in looping, so use
     * them instead if you need performance
     *
     * @param n
     * @return the nth prime
     */
    @Override
    public int nthPrime(int n) {
        if (n < 1 || n > numOfPrimesInSieve)
            return -1; //n need to be within 0 and numOfPrimesInSieve
        if (n == 1)
            return 2;                            //account manually for n = 1 which returns 2

        for (int index = -1, len = actualSieveSize, count = 1 /* count = 1 for not including 2 in the array */; index < len; ) {
            index = sieve.nextSetBit(index + 1);
            if (++count >= n)
                return (index << 1) + 3; //i * 2 + 3 to account for using only odd numbers from 3
        }
        return -1; //compiler demands this
    }

    /**
     * Determines whether a number is prime.
     * <p>
     * Note: If the number is larger than the sieve's size, a larger sieve will be automatically created to
     * check whether it's prime or not. Note this can be very expensive. Recommend using PrimesUtil.isPrime()
     * for more general purpose checking.
     *
     * @param n the number to be checked
     * @return a boolean value containing whether it's prime or not
     */
    @Override
    public boolean isPrime(int n) {
        //manually account for 2 and negative/even numbers
        //**note: n & 1 is equivalent to n % 2 == 0, but much faster
        if (n == 2)
            return true;
        else if ((n & 1) == 0 || n < 2)
            return false;

            //if it's within the sieve, just return the value inside the sieve
        else if (n <= sieveSize)
            return sieve.get((n >> 1) - 1);

            //otherwise, test primality using .testLargeNumber()
        else
            return testLargeNumber(n);
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
        if (n <= sieveSize)
            return;
        createSieve(n);
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
        int index = 0, count = 0;
        while (index != -1) {
            primes[++count] = (index << 1) + 3;
            index = sieve.nextSetBit(index + 1);
        }
        ;
        return primes;
    }

    //create a new sieve of size n. Use in the constructor and .expandSearchUpTo();
    private void createSieve(int size) {
        actualSieveSize = ((size >> 1) + 1) - 1;
        sieve = new BitSet(actualSieveSize);
        sieve.set(0, actualSieveSize);

        //Start sieve, if u don't know how the sieve works, search it up
        //Optimizations include:
        // - starts off at i * i while sieving (small gain)
        // - no multiples of 2 (respectable gain, not only in performance, but also in memory)
        //***note*** all the +3, -3, *2, /2 etc. is to account for using indexes to represent odd numbers
        int index = -1;
        for (int bound = (((int) Math.sqrt(size)) >> 1) - 1 /* bound as in the array's bound */; ; ) {
            index = sieve.nextSetBit(index + 1); //this continue to get next true in the BitSet (a prime)
            if (index > bound)
                break;

            for (int skip = (index << 1) + 3, j = (skip * skip - 3) >> 1; j < actualSieveSize; j += skip) {
                sieve.set(j, false);
            }
        }

        //update information about the sieve
        sieveSize = size;
        numOfPrimesInSieve = sieve.cardinality() + 1; //+1 to account for the prime 2
    }

    //this test whether a number beyond the sieve for primality using simple prime divisibilty test.
    //The sieve will be resized if there is not enough prime to determine the number's primality
    private boolean testLargeNumber(int number) {
        //if there are not enough primes, then expand the sieve
        if (sieveSize < number) {
            expandSearchUpTo((int) Math.sqrt(number));
        }

        int bound = (((int) Math.sqrt(number) + 1) >> 1) - 1,
                i = 0;
        for (; i >= 0 && i < bound; ) {
            if (number % ((i << 1) + 3) == 0)
                return false;
            i = sieve.nextSetBit(i + 1);
        }
        return true;
    }

    //*******************************************************************************************************
    //**********************************************Iteration************************************************
    //*******************************************************************************************************
    //this holds the informations for the iterator methods
    // * currentPrime: the current prime the iteration cursor is on
    // * Index: index the current prime is in the boolean array. Index represents 2 when it's -1.
    // * NthPrime: the current prime's number (1 -> 1st prime, 2 -> end prime etc.)
    private int currentPrime = 0,
            currentPrimeIndex = -2,
            currentNthPrime = 0;

    @Override
    /**
     * Returns the next prime in the sieve. It's an iterator along with .previousPrime().
     *
     * Using them is more efficient than .nthPrime(int n) if you are accessing the primes array a lot.
     *
     * Other methods related to this includes .currentIndexOfPrime() and .currentPrime().
     *
     * Note: if you use .previousPrime() until the beginning of the sieve (the method will return -1) and then
     * call this, the first prime (2) will be returned.
     * @return the next prime. This is -1 if the end of the sieve had been reached
     */
    public int nextPrime() {
        //if it's at the beginning of iteration (currentPrimeIndex < -1), return 2
        if (currentPrimeIndex < -1) {
            currentPrimeIndex = -1;
            currentNthPrime = 1;
            return 2;
        }

        //else just use the BitSet's nextSetBit() to find the primes
        int returnIndex = sieve.nextSetBit(currentPrimeIndex + 1);
        if (returnIndex != -1) {
            ++currentNthPrime;
            currentPrimeIndex = returnIndex;
            currentPrime = (currentPrimeIndex << 1) + 3;
            return currentPrime;
        }

        //once end of sieve had been reached, currentPrimeIndex would retain the index of the lastPrime
        //which would affect how .previousPrime() would work, this is to correct tha
        currentPrimeIndex = actualSieveSize;

        //-1 is only returns when the end of the sieve has been reached
        return -1;
    }

    /**
     * Returns the previous prime in the sieve. It's an iterator along with .nextPrime().
     * <p>
     * This cannot be guaranteed to be faster than using .nthPrime() although i assume it would be on
     * average faster.
     * <p>
     * Other methods related to this includes .currentIndexOfPrime() and .currentPrime().
     * <p>
     * Note: if you use .previousPrime() until the end of the sieve (the method will return -1) and then call
     * this, the first prime (2) will be returned.
     *
     * @return the next prime. This is -1 if the end of the sieve had been reached
     */
    @Override
    public int previousPrime() {
        //if the there is a previous prime bigger than 2 (currentPrimeIndex > 0), find the previous prime
        if (currentPrimeIndex > 0) {
            while (--currentPrimeIndex >= 0) {
                if (sieve.get(currentPrimeIndex)) {
                    --currentNthPrime;
                    currentPrime = (currentPrimeIndex << 1) + 3;
                    return currentPrime;
                }
            }
        }

        //if the currentIndex is 0, then the previous prime would be 2
        else if (currentPrimeIndex == 0) {
            currentPrimeIndex = -2;
            currentNthPrime = 1;
            currentPrime = 2;
            return 2;
        }

        //this only returns when the beginning of the sieve has been reached
        return -1;
    }

    /**
     * The current prime's position among the primes. (e.g. 2 returns 1 b/c it's the 1st prime, 3 returns 2, etc.)
     *
     * @return the prime's position among the primes
     */
    @Override
    public int indexOfCurrentPrime() {
        return currentNthPrime;
    }

    /**
     * Returns the current prime the iteration cursor is on.
     *
     * @return the current prime
     */
    @Override
    public int currentPrime() {
        return currentPrime;
    }

    /**
     * Set the iteration cursor used by .nextPrime() and .previousPrime() to the beginning.
     * <p>
     * After calling this method, .nextPrime() would return 2, .previousPrime() would return -1,
     * .currentPrime() would return 0 and .currentPrimeIndex() would return 0.
     */
    @Override
    public void resetIteration() {
        currentPrime = 0;
        currentPrimeIndex = -2;
        currentNthPrime = 0;
    }
}
