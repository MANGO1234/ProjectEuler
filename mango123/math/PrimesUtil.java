package mango123.math;
import java.util.Arrays;

/**
 * A class with utility methods that are related to primes.
 * Requires a PrimeSearcher to function. An EratosthenesSieve of size 1000000 is provided by default.
 * @author De Li
 *
 */
public class PrimesUtil {
	//primeSearcher: this finds all the primes used by PrimesUtils
	//primes: an array from primeSearcher containing the primes it found so far, it's much quicker for 
	//        internal use
	//primeRange: cache of primeSeacher.size()
	//primalityRange: this is a cache of how far various method like .isPrimeFactorsOf() can test for primality
	//               without needing to expand the primeSearcher (since it would be expensive to do so)
	private static PrimeSearcher primeSearcher = new EratosthenesSieve(1000000);
	private static int[] primes = primeSearcher.toArray();
	private static long primeRange = primeSearcher.size(),
					       primalityRange = primeRange * primeRange;
	private static final double LOG2 = Math.log(2);
	
	private static void updatePrimesInfo() {
		primes = primeSearcher.toArray();
		primeRange = primeSearcher.size();
		primalityRange = primeRange * primeRange;
	}

	private static void expandPrimeSearcher(long num) {
		int range = (int) Math.sqrt(num);
		primeSearcher.expandSearchUpTo(range);
		updatePrimesInfo();
	}
	
	//this make sure there are enough primes to do a primality test, factoring, etc by expanding the array
	//if the number exceeds the primalityRange
	private static void makeSureThereIsEnoughPrimeToDoPrimalityTest(long num) {
		if (num > primalityRange) expandPrimeSearcher(num);
	}

	/**
	 * Returns all the prime factors of a number in an integer array, sorted.
	 * 
	 * Example: 480 -> [2, 2, 2, 2, 2, 3, 5]
	 * @param num
	 * @return a sorted integer array containing all the prime factors of num
	 */
	public static int[] primeFactorsOf(long num) {
		num = Math.abs(num);
		
		//if it's within range of primeSearcher and is a prime (so as not to trigger an expensive call from
		//.isPrime() or smaller than 2
		if ((num < primeRange && primeSearcher.isPrime((int) num)) || num < 2) return new int[0];
		
		makeSureThereIsEnoughPrimeToDoPrimalityTest(num);

		//factors has been initialized with the max number of prime factors num can have
		int[] factors = new int[((int) (Math.log(num) / LOG2)) + 1];
		
		int count = -1, //keep track of factors index
			i = -1,     //keep track of primes index
			tempPrime;

		//loop until num becomes 1, in which case we got all the factors
		while (num != 1) {
			tempPrime = primes[++i];
			
			//loop: to get multiples of the same prime factors (e.g. 8 have prime factors 2 * 2 * 2)
			while (num % tempPrime == 0) {
				factors[++count] = tempPrime;
				num /= tempPrime;
			}
		}

		return Arrays.copyOf(factors, count + 1);
	}
	
	/**
	 * Returns all the <i>unique</i> prime factors of a number in an integer array, sorted.
	 * 
	 * Example: 480 -> [2, 3, 5] even though it's prime factorization is 2^5 * 3 * 5
	 * @param num
	 * @return a sorted integer array containing all the prime factors of num
	 */
	public static int[] uniquePrimeFactorsOf(long num) {
		num = Math.abs(num);
		
		//if it's within range of primeSearcher and is a prime (so as not to trigger an expensive call from
		//.isPrime() or smaller than 2
		if ((num < primeRange && primeSearcher.isPrime((int) num)) || num < 2) return new int[0];
		
		makeSureThereIsEnoughPrimeToDoPrimalityTest(num);

		//factors has been initialized with the max number of prime factors num can have
		int[] factors = new int[((int) (Math.log(num) / LOG2)) + 1];
		
		int count = -1, //keep track of factors index
			i = -1,     //keep track of primes index
			tempPrime;

		while (num != 1) {
			tempPrime = primes[++i];

			if (num % tempPrime == 0) {
				factors[++count] = tempPrime;
				num /= tempPrime;
				
				//loop through until there is no more multiples of this prime factor
				while (num % tempPrime == 0) {
					num /= tempPrime;
				}
			}
		}

		return Arrays.copyOf(factors, count + 1);
	}

	public static boolean isPrime(long n) {
		if (n < primeRange) return primeSearcher.isPrime((int) n); // note that primeRange should always be smaller than Integer.MAX
		makeSureThereIsEnoughPrimeToDoPrimalityTest(n);
		int searchUpto = (int) Math.sqrt(n);
		for (int i = 0; primes[i] <= searchUpto; i++) {
			if (n % primes[i] == 0) return false;
		}
		return true;
	}
}
