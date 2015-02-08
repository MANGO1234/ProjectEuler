package mango123.math;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * A class with all the (hopefully and maybe useful) math stuff written for the Eulor Project (kept them for
 * future uses). They are really simple though...
 * @author De Li
 * @version 1.0
 */
public final class EulerMath {
	//prevent initialization of object
	private EulerMath() {}

	//a cache of pow of 10, up to the number of digits in int
	private static final int[] INT_POW_10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};
	//a cache of pow of 10, up to the number of digits in long
	private static final long[] LONG_POW_10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
										    10000000000l, 100000000000l, 1000000000000l, 10000000000000l,
									        100000000000000l, 1000000000000000l, 10000000000000000l,
									        100000000000000000l, 1000000000000000000l};
	
	//cache of the factorials from 1 to 20 (20! is the maximum a long can hold)
	//Use in the method factorial(int n) -> faster and no calculation
	private final static long[] factorials = {1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 
											  39916800, 479001600,  6227020800l, 87178291200l, 1307674368000l,
											  20922789888000l, 355687428096000l, 6402373705728000l,
											  121645100408832000l, 2432902008176640000l};
	/**
	 * Calculates the factorial of a number.
	 * 20! is the maximum this function can get, as otherwise it will overflow long.
	 * If you pass a number bigger than 20 or smaller than 0, 0 will be returned.
	 * If you need a larger factorial, use bigFactorial which returns a BigInteger instance of the factorial.
	 * @param n the number
	 * @return the factorial of n
	 */
	public static long factorial(int n) {
		if (n < 0 && n > 20) return 0;
		return factorials[n];
	}
	
	/**
	 * Find the nCr of two integers.
	 * nCr means the number of ways to choose n element(s) out of a set of r element(s).
	 * The formula is n!/(r! * (n-r)!).
	 * It can get large quickly, so be careful choosing the n and r. Use nCrBig for large n and r values.
	 * @param n
	 * @param r
	 * @return a long containing
	 */
	public static long nCr(int n, int r) {
		int difference = n - r;
		if (difference > r) {
			int temp = difference;
			difference = r;
			r = temp;
		}
		long product = n;
		n--; // this sets it up so product /= i always work in loop
		for (int i = 2; i <= difference; ++i, --n) {
			product *= n;
			product /= i;
		}
		return product;
	}
	
	//TODO: optimize divisorsOf
	/**
	 * Finds all the proper divisors of an integer (unsorted).
	 * The proper divisors of a number is all the number smaller than it that can divide evenly into it (inlcuding 1).
	 * @param number The integer you wanted the proper divisors of
	 * @return An int[] containing the proper divisors of number
	 */
	public static int[] divisorsOf(int number) {
		//if number is less than 0, return a empty integer array
		if (number < 0) return new int[0];
		ArrayList<Integer> divisors = new ArrayList<Integer>();

		
		//cache variable, is used to determine the upper bound of testing possible divisors
		int bound = (int) Math.sqrt(number);
		
		//if the sqrt itself is a divisor, add it and reduce bound so it won't be counted again
		if (bound * bound == number) {
			divisors.add(bound);
			--bound;
		}

		//find all the proper divisors
		for (int i = 1; i <= bound; ++i) {
			int tempNum = number / i;
			if (tempNum * i == number) {
				divisors.add(i);
				divisors.add(tempNum);
			}
		}

		//convert the divisors to correct type (ArrayList<Integer> to int[])
		int[] iDivisors = new int[divisors.size()];
		for (int i = -1, len = divisors.size(); ++i < len;) {
			iDivisors[i] = divisors.get(i).intValue();
		}
		return iDivisors;
	}

	/**
	 * Finds all the divisors of an integer (unsorted).
	 * e.g. 9 -> [1, 3, 9] (note: may not be sorted)
	 * @param number The integer you wanted the proper divisors of
	 * @return An int[] containing the proper divisors of number
	 */
	public static long[] divisorsOf2(long number) {
		//** the comments are the same as the int[] properDivisorsOf(int number) version,
		//so go check out that instead

		if (number <= 0) return new long[0];

		ArrayList<Long> divisors = new ArrayList<Long>();
		long bound = (long) Math.sqrt(number);

		if (bound * bound == number) {
			divisors.add(bound);
			--bound;
		}

		//find all the proper divisors
		for (int i = 1; i <= bound; ++i) {
			long tempNum = number / i;
			if (tempNum * i == number) {
				divisors.add((long) i);
				divisors.add(tempNum);
			}
		}

		long[] iDivisors = new long[divisors.size()];
		for (int i = -1, len = divisors.size(); ++i < len;) {
			iDivisors[i] = divisors.get(i).longValue();
		}
		return iDivisors;
	}


	
	/**
	 * Finds all the proper divisors of a long integer (unsorted).
	 * The proper divisors of a number is all the number smaller than it that can divide evenly into it (inlcuding 1).
	 * @param number The long integer you wanted the proper divisors of
	 * @return An long[] containing the proper divisors of number
	 */
	public static long[] divisorsOf(long number) {
		//** the comments are the same as the int[] properDivisorsOf(int number) version,
		//so go check out that instead
		
		if (number <= 0) return new long[0];
		
		ArrayList<Long> divisors = new ArrayList<Long>();
		long bound = (long) Math.sqrt(number);

		if (bound * bound == number) {
			divisors.add(bound);
			--bound;
		}

		//find all the proper divisors
		for (int i = 1; i <= bound; ++i) {
			long tempNum = number / i;
			if (tempNum * i == number) {
				divisors.add((long) i);
				divisors.add(tempNum);
			}
		}

		long[] iDivisors = new long[divisors.size()];
		for (int i = -1, len = divisors.size(); ++i < len;) {
			iDivisors[i] = divisors.get(i).longValue();
		}
		return iDivisors;
	}
	
	/**
	 * Finds the nth triangle number (n has a limit beware roughly sqrt(Long.MAX))
	 * @param n
	 * @return the nth triangle number
	 */
	public static long nthTriangleNumber(int n) {
		return (long) n * (n + 1) >> 1;
	}


	/**
	 * Determines whether a number is a triangle number or not.
	 * @param n
	 * @return a boolean indicating whether it's a triangle number or not
	 */
	public static boolean isTriangleNumber(long n) {
		//the inverse function of formula for triangle numbers minus some steps
		//(no need to -1 and /2 later because if square root is whole, then n is triangle)
		return Math.sqrt((n << 3) + 1) % 1 == 0;
	}

	
	/**
	 * Finds the nth pentagonal number (beware has limit of n)
	 * @param n
	 * @return the nth pentagonal number
	 */
	public static long nthPentagonalNumber(int n) {
		return (long) n * (n + n + n - 1) >> 1;
	}

	
	/**
	 * Determines whether a number is a pentagonal number or not.
	 * @param n
	 * @return a boolean value indicating whether a number is a pentagonal number or not
	 */
	public static boolean isPentagonalNumber(long n) {
		//the inverse function of formula for pentagonal numbers
		return ((Math.sqrt(((n << 4) + (n << 3)) + 1) + 1) / 6) % 1 == 0;
	}

	/**
	 * Finds the nth hexagonal number
	 * Note: n = 32768 is the limit of this method, as an n bigger than it would cause overflow of long during calculation
	 * @param n
	 * @return the nth hexagonal number
	 */
	public static long nthHexagonalNumber(int n) {
		return n * n * 2 - n;
	}

	/**
	 * Determines whether a number is a hexagonal number or not.
	 * @param n
	 * @return a boolean value indicating whether a number is a hexagonal number or not
	 */
	public static boolean isHexagonalNumber(long n) {
		//the inverse function of formula for hexagonal numbers
		return ((Math.sqrt(((n << 3) + 1)) + 1) / 4) % 1 == 0;
	}
	
	/**
	 * Finds the greatest common factor between two long integers.
	 * @param a
	 * @param b
	 * @return a long integer that is the GCD of the two parameters
	 */
	public static long GCD(long a, long b) {
		while (b != 0) {
			long temp = a;
			a = b;
			b = temp % a;
		}
		return a;
		//using the Euclidean algorithm to find it
	}

	/**
	 * Finds the greatest common factor between two integers.
	 * @param a
	 * @param b
	 * @return a long integer that is the GCD of the two parameters
	 */
	public static int GCD(int a, int b) {
		//using the Euclidean algorithm to find it
		while (b != 0) {
			int temp = a;
			a = b;
			b = temp % a;
		}
		return a;
	}

	/**
	 * Finds the least common multiple of two long integers.
	 * @param a
	 * @param b
	 * @return the LCM of the two parameters
	 */
	public static long LCM(long a, long b) {
		return a/ GCD(a, b) * b;
	}
	
	/**
	 * Finds the least common multiple of two integers.
	 * @param a
	 * @param b
	 * @return the LCM of the two parameters
	 */
	public static int LCM(int a, int b) {
		return a / GCD(a, b) * b;
	}
	
	/**
	 * Is number square?
	 * @param num
	 * @return boolean indicating whether num is square
	 */
	public static boolean isSquare(long num) {
		return Math.sqrt(num) % 1 == 0; //optimize someday...
	}

	/**
	 * Return n^pow using fast exponentiation. Doesn't do any overflow check.
	 * @param n
	 * @param pow
	 * @return n^pow
	 */
	public static long pow(long n, int pow) {
		long temp = 1;
		while (pow != 0) {
			if ((pow & 1) == 1) {
				temp = (temp * n);
			}
			n = n * n;
			pow >>= 1;
		}
		return temp;
	}
	
	//**************************************************************************************************
	//**********************************************Primes**********************************************
	//**************************************************************************************************
	
	public static boolean[] primesUpToRawFormat(int n) {
		boolean[] isPrime = new boolean[n + 1];
		Arrays.fill(isPrime, true);
		isPrime[0] = false;
		isPrime[1] = false;
		
		for (int i = 4, len = n + 1; i <= len; i += 2) {
			isPrime[i] = false;
		}

		for (int i = 2, bound = (int) Math.sqrt(n); ++i <= bound;) {
			if (isPrime[i] == true) {
				for (int j = i * i, skip = i << 1; j < n; j += skip) {
					isPrime[j] = false;
				}
			}
		}
		return isPrime;
	}
	
	public static int[] primesUpTo(int n) {
		boolean[] primesUpTo = primesUpToRawFormat(n);
	
		//get all the primes into an array
		// * using a crude form of prime number theorem to estimate the number of the primes (3 from Integer.MAX_VALUE)
		// * we will delete empty spaces later
		int[] primes = new int[(int) (n / (Math.log(n) - 3))];
		
		//keep track of how many primes we got
		int numberOfPrimes = -1;

		//get the primes
		for (int i = 1, len = primesUpTo.length; ++i < len;) {
			if (primesUpTo[i]) primes[++numberOfPrimes] = i;
		}
		
		//return the array without the null values
		return Arrays.copyOf(primes, numberOfPrimes);
	}

	/**
	 * Extracts the indexes of true value from a boolean array
	 * @param booleans
	 * @return an integer array containing all the indexes of true value in array
	 */
	public static int[] extractIntsFromBooleanArray(boolean[] booleans) {
		int count = 0;
		for (int i = 0; i < booleans.length; i++) {
			if (booleans[i]) ++count;
		}
		int[] integers = new int[count];
		
		count = 0;
		for (int i = 0; i < booleans.length; i++) {
			if (booleans[i]) integers[count++] = i;
		}
		return integers;
	}
}
