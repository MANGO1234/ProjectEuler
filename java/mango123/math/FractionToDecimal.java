package mango123.math;
import java.util.Arrays;

/**
 * A simple class to get the digits of a fraction between 0 & 1.
 * Written for Project Euler.
 * @author De Li
 * @version 1.0
 */
public class FractionToDecimal {
	private long denominator, numerator,
				 currentRemainder;
	private int currentNumberOfDigits;

	/**
	 * This creates a new FractionToDecimal object with a specified long integer numerator and denominator.
	 * @param numerator the numerator of the fraction. This parameter cannot be bigger than the numerator or 0, else it will throw an IllegalArgumentException
	 * @param denominator the denominator of the fraction. This will throw an IllegalArgumentException if you pass a number <= 0 for this parameter
	 * @throws IllegalArgumentsException
	 */
	public FractionToDecimal(long numerator, long denominator) throws IllegalArgumentException {
		if (denominator == 0) throw new IllegalArgumentException("FractionToDecimal constructor: denominator cannot be zero!");
		else if (numerator >= denominator) throw new IllegalArgumentException("FractionToDecimal constructor: since fraction need to be between 0 and 1, numerator cannot be bigger or equal to the denominator!");
		else if (denominator < 0 || numerator < 0) throw new IllegalArgumentException("FractionToDecimal constructor: numerator and denominator both must be positive!");

		this.denominator = denominator;
		this.numerator = numerator;
		currentNumberOfDigits = 0;
		currentRemainder = numerator;
	}

	/**
	 * Gets the current amount of digits calculated so far.
	 * (i.e. calculate to 0.1234 with be return 4)
	 * @return the current amount of digits calculated
	 */
	public int getCurrentNumberOfDigits() {
		return currentNumberOfDigits;
	}
	
	/**
	 * Gets the numerator of the fraction
	 * @return the numerator
	 */
	public long getNumerator() {
		return numerator;
	}
	
	/**
	 * Gets the denominator of the fraction.
	 * @return the denominator
	 */
	public long getDenominator() {
		return denominator;
	}
	
	/**
	 * Calculates the next digit of the fraction.
	 * @return a byte containing the next digit of the fraction
	 */
	public byte nextDigit() {
		//this = multiply by ten, but reduces calculation time
		currentRemainder = (currentRemainder << 3) + (currentRemainder << 1);
		
		//this reduce calculation time by %50+ instead of integer division and modulus
		byte digit = 0;
		while (currentRemainder >= denominator) {
			currentRemainder -= denominator;
			++digit;
		}
		
		++currentNumberOfDigits;
		return digit;
	}
	
	
	/**
	 * Calculates the next n digits of the fraction.
	 * @param n the number of digits you want (cannot be smaller or equal to 0)
	 * @return a byte array containing the next n digits of the fraction
	 */
	public byte[] nextDigits(int n) {
		byte[] digits = new byte[n];
		
		for (int i = -1; ++i < n;) {
			digits[i] = nextDigit();
		}
		
		return digits;
	}
	
	/**
	 * If the fraction is repeating, this finds and returns the length of the repeating part.
	 * If the fraction is not repeating, this returns 0.
	 * Note: if the number of repeating digits exceeds int, this will throw an exception (unlikely because of the numbers involve)
	 * @return the length of the repeating (if the fraction is repeating) part of the fraction
	 */
	public int repeatingLength() {
		//create a temporary to hold the current remainder 
		long tempRemainder = currentRemainder;

		int count = 0;
		int length;
		
		long[] remainders = new long[30]; //30 is random

		//this keep looping until a remainder is found to have been calculated before,
		//then the loop will end and this will return the length of the distance between them
		do {
			//if remainder is 0, the fraction has finished and is not repeating, returns 0 for length
			if (tempRemainder == 0) return 0;

			//at the beginning b/c otherwise loop condition will always be true
			remainders[count] = tempRemainder;

			//this = multiply by ten, but reduces calculation time
			tempRemainder = (tempRemainder << 1) + (tempRemainder << 3);

			//this reduce calculation time by %50+ instead of integer division and modulus
			while (tempRemainder >= denominator) {
				tempRemainder -= denominator;
			}

			//just in case remainders's capacity is exceeded
			if (++count >= remainders.length) remainders = Arrays.copyOf(remainders, remainders.length + 30);
			
			//see if the remainder has been fond before (method will return less than 0)
			length = findRepeatingLength(remainders, tempRemainder, count);
		} while (length < 0);
		
		return length;
	}
	
	//used in .repeatingLength() to search the remainders array
	//note: numberOfRemainders = no need to loop through rest of array. It is equal to count in .repeatingLength()
	private int findRepeatingLength(long[] remainders, long remainder, int numberOfRemainders) {
		for (int i = -1, len = remainders.length; ++i < len;) {
			if (remainders[i] == remainder) return numberOfRemainders - i;
		}
		return -1;
	}
}
