package mango123.miscellaneous;

import java.util.Arrays;

// TODO what was I thnking, need to refactor into helper amthods why the fk are those in class
/**
 * A wrapper class for int that would provide some handy values like getting the digits of a number.
 * 
 * This is essentially re-factor method from EulerMath since I find there are lots of repeated code to do
 * math stuff, making them kind of slow. 
 * @author De Li
 *
 */
public class IntegerDigit {
	private int value;
	
	 //holds the number of digits of the integer
	private int numOfDigits = 0;

	//holds the digits in the integer
	private int[] digits;

	/**
	 * Create a DLInteger holding the integer passed in.
	 * @param integer
	 */
	public IntegerDigit(int integer) {
		//note: methods see super class DLNumber
		this.value = integer;
		this.digits = Digit.digitsOf(integer);
		this.numOfDigits = digits.length;
	}
	
	/**
	 * Returns the int value this instance of DLInteger is wrapping.
	 * @return int of this instance of DLInteger
	 */
	public int intValue() {
		return value;
	}

	/**
	 * Returns the number of digits the int have.
	 * @return the number of digits the int have.
	 */
	public int numberOfDigits() {
		return numOfDigits;
	}
	
	/**
	 * Returns the digits of the int in an array of integers.
	 * 
	 * e.g. if the wrapper contains 2345, this would return [2, 3, 4, 5]
	 * @return the digits of the int this object contains
	 */
	public int[] digits() {
		//return a copy so user can't change the digits
		return Arrays.copyOf(digits, numOfDigits);
	}
	
	/**
	 * Returns whether the integer is palindromic or not.
	 * @return a boolean value on whether the integer this instance of DLIntger contains is palindromic or not
	 */
	public boolean isPalindromic() {
		for (int i = 0, j = numOfDigits - 1; i < j; ++i, --j) {
			if (digits[i] != digits[j]) return false;
		}
		return true;
	}

	/**
	 * Returns whether this DLInteger is a permutation to another DLInteger.
	 * @param integer
	 * @return a boolean value containing whether this is a permutation to another integer.
	 */
	public boolean isPermutationTo(IntegerDigit integer) {
		if (this.numOfDigits != integer.numberOfDigits()) return false;
		
		int[] digitsB = integer.digits();

		int search;
		boolean notFound;
		for (int i = 0, len = numOfDigits; i < len; ++i) {
			search = digits[i];
			
			notFound = true;
			for (int j = 0; j < len; ++j ) {
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
	 * (Trivia: This implicitly change the integer passed in to another DLInteger and use the overloaded
	 *  method .isPermutationTo(DLInteger integer) to do the work)
	 * @param integer
	 * @return a boolean value containing whether this is a permutation to another integer.
	 */
	public boolean isPermutationTo(int integer) {
		return isPermutationTo(new IntegerDigit(integer));
	}
}
