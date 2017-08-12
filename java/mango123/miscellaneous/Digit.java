package mango123.miscellaneous;

/**
 * Contains utility methods regarding digits of an integer like digitsOf(int integer).
 * @author De Ll
 *
 */
public abstract class Digit {
	
	//a cache of power of 10 for method use in instances
	private static final int[] intPow10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};
	//a cache of power of 10 for method use in methods
	private static final long[] longPow10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
										    10000000000l, 100000000000l, 1000000000000l, 10000000000000l,
									        100000000000000l, 1000000000000000l, 10000000000000000l,
									        100000000000000000l, 1000000000000000000l};
	
	/**
	 * Calculate the number of digits of the integer passed in.
	 * @param integer
	 * @return the number of digits in integer
	 */
	public static int numOfDigits(int integer) {
		//optimize -> comparison & subtraction v.s. convert to string & find length
		//(which can give 50% to 100% speed improvement depending on the length of the integer)
		//(the longer the integer is, the faster this method is compare to String.length())
		integer = Math.abs(integer);
		
		for (int digit = 9; digit > 0; --digit) {
			if (integer >= intPow10[digit]) {
				return digit + 1; //+1 compensate for using 9 digits instead of 10 at the beginning
			}
		}
		return 1;
	}
	
	
	/**
	 * Calculate the number of digits of the long passed in.
	 * @param integer
	 * @return the number of digits in integer
	 */
	public static int numOfDigits(long longV) {
		//optimize -> comparison & subtraction v.s. convert to string & find length
		//(which can give 50% to 100% speed improvement depending on the length of the integer)
		//(the longer the integer is, the faster this method is compare to String.length())
		longV = Math.abs(longV);

		for (int digit = 18; digit > 0; --digit) {
			if (longV >= longPow10[digit]) {
				return digit + 1; //+1 compensate for using 9 digits instead of 10 at the beginning
			}
		}
		return 1;
	}
	
	/**
	 * Returns the digits of an integer in an array of ints.
	 * 
	 * e.g. 1234 -> [1, 2, 3, 4]
	 * @param integer
	 * @return
	 */
	public static int[] digitsOf(int integer) {
		int numOfDigits = numOfDigits(integer),
			tempDigit, tempPow;
		int[] digits = new int[numOfDigits];

		for (int i = 0, pow = numOfDigits - 1; pow > 0; ++i, --pow) {
			//the tempDigit and tempPow is used to reduce modulus and division to subtraction,
			//increments and array access
			tempPow = intPow10[pow];
			tempDigit = 0;

			while (integer >= tempPow) {
				integer -= tempPow;
				++tempDigit;
			}
			digits[i] = tempDigit;
		}

		digits[numOfDigits - 1] = integer;
		return digits;
	}

	/**
	 * Returns the digits of a long in an array of ints.
	 * 
	 * e.g. 1234 -> [1, 2, 3, 4]
	 * @param integer
	 * @return the digits of a long
	 */
	public static int[] digitsOf(long longV) {
		int numOfDigits = numOfDigits(longV), tempDigit;
		int[] digits = new int[numOfDigits];
		long tempPow;

		for (int i = 0, pow = numOfDigits - 1; pow > 0; ++i, --pow) {
			//the tempDigit and tempPow is used to reduce modulus and division to subtraction,
			//increments and array access
			tempPow = longPow10[pow];
			tempDigit = 0;

			while (longV >= tempPow) {
				longV -= tempPow;
				++tempDigit;
			}
			digits[i] = tempDigit;
		}

		digits[numOfDigits - 1] = (int) longV;
		return digits;
	}
}
