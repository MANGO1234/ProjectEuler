package mango123.math;

import java.util.Arrays;

/**
 * A simple EratosthenesSieve using boolean array.
 * <p>
 * It was a learning project, so it sucks.
 *
 * @author De Li
 * @version 1.0
 */
public class EratosthenesSieve implements PrimeSearcher {
    // sieve: holds the sieve of boolean value for primes
    // ** The sieve indexes correspond to odd numbers starting with 3.
    //    e.g. index 0 is the number 3, index 1 is the number 5, etc.
    //    To convert an index to the number it represents, multiply it by 2 then add 3.
    private boolean[] sieve;

    // sieveSize: the actual size the sieve represent, not sieve.length
    // (if the sieve is size 100, sieve.length would be 49)
    // numberOfPrimeInSieve: number of primes in the sieve (starting with 1 to
    // account for the prime 2, see .countPrimes())
    private int sieveSize = 0,
            numOfPrimesInSieve = 1;

    /**
     * Creates an EratosthenesSieve up to n. n is minimum 10.
     * <p>
     * If n is smaller or equal to 10, this will use 10 as the value of n.
     *
     * @param n the size of the sieve to be constructed
     */
    public EratosthenesSieve(int n) {
        createSieveOfSize(n < 10 ? 10 : n);
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
        int len = sieve.length;
        for (int i = 0; i < len; ++i) {
            if (sieve[i]) {
                if (++count >= n) {
                    return (i << 1) + 3; //i * 2 + 3 to account for using only odd numbers from 3
                }
            }
        }
        return -1; //compiler demands this
    }

    /**
     * Determines whether a number is prime.
     * <p>
     * Note: If the number is larger than the sieve's size, a larger sieve will be
     * automatically created to check whether it's prime or not. Note this can be
     * very expensive.
     *
     * @param n the number to be checked
     * @return a boolean value containing whether it's prime or not
     */
    @Override
    public boolean isPrime(int n) {
        //manually account for 2 and negative/even numbers
        //**note: n & 1 is equivalent to n % 2 == 0, but much faster
        if ((n & 1) == 0) {
            return n == 2;
        } else if (n < 2) {
            return false;
        } else if (n <= sieveSize) {
            //if it's within the sieve, just return the value inside the sieve
            return sieve[(n >> 1) - 1];
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
            createSieveOfSize(n);
        }
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
     * Returns the number of primes found in the sieve.
     *
     * @return number of primes in sieve
     */
    @Override
    public int numberOfPrimesFound() {
        return numOfPrimesInSieve;
    }

    /**
     * Return all the primes in the sieve, listed in ascending numeric order, as an integer array.
     */
    @Override
    public int[] toArray() {
        int[] primes = new int[numOfPrimesInSieve];
        primes[0] = 2;
        for (int i = 0, len = sieve.length, count = 0; i < len; ++i) {
            if (sieve[i])
                primes[++count] = (i << 1) + 3;
        }
        return primes;
    }

    //create a new sieve of size n. Use in the constructor and .expandSearchUpTo();
    private void createSieveOfSize(int size) {
        int len = ((size + 1) >> 1) - 1;
        sieve = new boolean[len]; //no even number will be in the array to save space
        Arrays.fill(sieve, true);

        //Start sieving, if u don't know how the sieve works, search it up
        //Optimizations include:
        // - starts off at i * i while sieving (small gain in speed)
        // - no multiples of 2 (respectable gain, not only in speed, but also in memory)
        //***note*** all the +3, -3, *2, /2 etc. is to account for using indexes to represent odd numbers
        for (int i = 0, bound = (((int) Math.sqrt(size)) >> 1) - 1 /* bound as in the array's bound */; i <= bound; ++i) {
            if (sieve[i] == true) {
                for (int skip = (i << 1) + 3, j = (skip * skip - 3) >> 1; j < len; j += skip) {
                    sieve[j] = false;
                }
            }
        }

        //update the sieve's size and the number of primes in the sieve
        sieveSize = size;
        countPrimes();
    }

    //count how many primes is in the sieve. Use in the constructor and .createSieveOfSize().
    //lastCountPostion is used so we don't have to re-count already counted region when .expandSearchUpTo()
    //is used
    private int lastCountPosition = 0;

    private void countPrimes() {
        int count = 0,
                len = sieve.length;

        for (int i = lastCountPosition; i < len; ++i) {
            if (sieve[i])
                ++count;
        }
        lastCountPosition = len;
        numOfPrimesInSieve += count;
    }

    //this test whether a number beyond the sieve for primality using simple prime divisibilty test.
    //The sieve will be resized if there is not enough prime to determine the number's primality
    private boolean testLargeNumber(int number) {
        //if there are not enough primes, then expand the sieve
        if (sieveSize < number) {
            expandSearchUpTo((int) Math.sqrt(number));
        }

        int bound = (((int) Math.sqrt(number) + 1) >> 1) - 1;
        for (int i = 0; i < bound; ++i) {
            if (sieve[i] && (number % ((i << 1) + 3)) == 0)
                return false;
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

    /**
     * Returns the next prime in the sieve. It's an iterator along with .previousPrime().
     * <p>
     * Using them is more efficient than .nthPrime(int n) if you are accessing the primes array a lot.
     * <p>
     * Other methods related to this includes .currentIndexOfPrime() and .currentPrime().
     * <p>
     * Note: if you use .previousPrime() until the beginning of the sieve (the method will return -1) and then
     * call this, the first prime (2) will be returned.
     *
     * @return the next prime. This is -1 if the end of the sieve had been reached
     */
    @Override
    public int nextPrime() {
        int len = sieve.length;

        //if it's at the beginning of iteration (currentPrimeIndex < -1), return 2
        if (currentPrimeIndex < -1) {
            currentPrimeIndex = -1;
            currentNthPrime = 1;
            return 2;
        }

        //else if there is still possible primes left (currentPrimeIndex < len),
        //search for it down the sieve
        else if (currentPrimeIndex < len) {
            while (++currentPrimeIndex < len) {
                if (sieve[currentPrimeIndex]) {
                    ++currentNthPrime;
                    currentPrime = (currentPrimeIndex << 1) + 3;
                    return currentPrime;
                }
            }
        }

        //-1 is only returns when the end of the sieve has been reached
        return -1;
    }

    /**
     * Returns the next prime in the sieve. It's an iterator along with .previousPrime().
     * <p>
     * Using them is more efficient than .nthPrime(int n) if you are accessing the primes array a lot.
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
                if (sieve[currentPrimeIndex]) {
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
        currentNthPrime = 0;
        currentPrimeIndex = -2;
    }
}
