package mango123.miscellaneous;

import java.util.Arrays;

/**
 * A wrapper class for int that would provide some handy values like getting the digits of a number.
 * 
 * This is essentially re-factor method from EulerMath since I find there are lots of repeated code to do
 * math stuff, making them kind of slow. 
 * @author De Li
 *
 */
public class LongDigit {
	private long value;
	
	 //holds the number of digits of the integer
	private int numOfDigits = 0;

	//holds the digits in the integer
	private int[] digits;

	/**
	 * Create a DLInteger holding the long passed in.
	 * @param integer
	 */
	public LongDigit(long longV) {
		this.value = longV;
		this.digits = Digit.digitsOf(longV);
		this.numOfDigits = digits.length;
	}
	
	/**
	 * Returns the long value this instance of DLLong is holding.
	 * @return long value this instance of DLLong is holding
	 */
	public long longValue() {
		return value;
	}

	/**
	 * Returns the number of digits the long have.
	 * @return the number of digits the long have.
	 */
	public int numberOfDigits() {
		return numOfDigits;
	}

	/**
	 * Returns the digits of the long in an array of integers.
	 * 
	 * e.g. if this instance of DLLong contains 123456789123, this would return [1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3]
	 * @return the digits of the long this object contains
	 */
	public int[] digits() {
		//return a copy so user can't change the digits
		return Arrays.copyOf(digits, numOfDigits);
	}
	
	/**
	 * Returns whether the long is palindromic or not.
	 * @return a boolean value on whether the integer this instance of DLLong contains is palindromic or not
	 */
	public boolean isPalindromic() {
		for (int i = 0, j = numOfDigits - 1; i < j; ++i, --j) {
			if (digits[i] != digits[j]) return false;
		}
		return true;
	}

	/**
	 * Returns whether this DLLong is a permutation to another DLLong.
	 * @param longV
	 * @return a boolean value containing whether this is a permutation to another long.
	 */
	public boolean isPermutationTo(LongDigit longV) {
		if (this.numOfDigits != longV.numberOfDigits()) return false;
		
		int[] digitsB = longV.digits();
		int search;
		boolean notFound;
		for (int i = 0; i < numOfDigits; ++i) {
			search = digits[i];
			
			notFound = true;
			for (int j = 0; j < numOfDigits; ++j ) {
				if (digitsB[j] == search)  {
					notFound = false;
					digitsB[j] = -1;
					break;
				}
			}
			
			if (notFound) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Returns whether the integer contained by this DLInteger is a permutation to another integer.
	 * 
	 * (Trivia: This implicitly change the long passed in to another DLLong and use the overloaded
	 *  method .isPermutationTo(DLLong longV) to do the work)
	 * @param longV
	 * @return a boolean value containing whether this is a permutation to another long
	 */
	public boolean isPermutationTo(int longV) {
		return isPermutationTo(new LongDigit(longV));
	}
}
