package mango123.util;

import java.math.BigInteger;

/**
 * Since I'm a beginner, I just got the code from the Internet. Credit to the guy that wrote this.
 * I did change the comments and slightly optimize some parts.
 * 
 * This class systematically generates permutations from 1 up to a length of n
 * (n must be <= 20, if n needs to be > 20, see the one using BigInteger: BigPermutationGenerator)
 * 
 * (Warning: do not make n too large as n! can get large incredibly large very quickly)
 * 
 * @author Michael Gilleland (http://www.merriampark.com/perm.htm)
 */

public class PermutationGenerator {
	private int[] a;
	private BigInteger numLeft;
	private BigInteger total;

	/**
	 * Creates a PermutationGenerator from 1 to n
	 * @param n the number of items in the list you want permutations of
	 */
	public PermutationGenerator (int n) {
		if (n < 1 && n > 20) {	throw new IllegalArgumentException ("PermutationGenerator can only accept argument n >= 1 && n <=20");    }
        total = getFactorial(n);
        initialize(n);
    }

    /**
     * Creates a PermutationGenerator with a list of unique integers
     *
     * @param arr list of unique integers
     */
    public PermutationGenerator(int[] arr) {
        if (arr.length < 1 && arr.length > 20) {
            throw new IllegalArgumentException("PermutationGenerator can only accept argument arr.length >= 1 && arr.length <=20");
        }
        total = getFactorial(arr.length);
        initialize(arr);
    }

    private void initialize(int n) {
        a = new int[n];
        for (int i = 0; i < a.length; i++) {
            a[i] = i;
        }
        numLeft = new BigInteger(total.toString());
    }

    //------------------------------------------------
    // Reset with a unique list of numbers for permutation
    //------------------------------------------------
    private void initialize(int[] arr) {
        a = ArraysUtils.sortAndRemoveDuplicates(arr);
        if (a.length != arr.length) {
            throw new IllegalArgumentException("Need unique list of numbers");
        }
        numLeft = new BigInteger(total.toString());
    }

    //------------------------------------------------
    // Return number of permutations not yet generated
    //------------------------------------------------

	  public BigInteger getNumLeft () {
	    return numLeft;
	  }

	  //------------------------------------
	  // Return total number of permutations
	  //------------------------------------

	  public BigInteger getTotal () {
	    return total;
	  }

	  //-----------------------------
	  // Are there more permutations?
	  //-----------------------------

	  public boolean hasMore () {
	    return numLeft.compareTo (BigInteger.ZERO) == 1;
	  }

	  //------------------
	  // Compute factorial
	  //------------------

	  private static BigInteger getFactorial (int n) {
	    BigInteger fact = BigInteger.ONE;
	    for (int i = n; i > 1; i--) {
	      fact = fact.multiply (new BigInteger (Integer.toString (i)));
	    }
	    return fact;
	  }

	  //--------------------------------------------------------
	  // Generate next permutation (algorithm from Rosen p. 284)
	  //--------------------------------------------------------

	  public int[] getNext () {

	    if (numLeft.equals (total)) {
	      numLeft = numLeft.subtract (BigInteger.ONE);
	      return a;
	    }

	    int temp;

	    // Find largest index j with a[j] < a[j+1]

	    int j = a.length - 2;
	    while (a[j] > a[j+1]) {
	      j--;
	    }

	    // Find index k such that a[k] is smallest integer
	    // greater than a[j] to the right of a[j]

	    int k = a.length - 1;
	    while (a[j] > a[k]) {
	      k--;
	    }

	    // Interchange a[j] and a[k]

	    temp = a[k];
	    a[k] = a[j];
	    a[j] = temp;

	    // Put tail end of permutation after jth position in increasing order

	    int r = a.length - 1;
	    int s = j + 1;

	    while (r > s) {
	      temp = a[s];
	      a[s] = a[r];
	      a[r] = temp;
	      r--;
	      s++;
	    }

	    numLeft = numLeft.subtract (BigInteger.ONE);
	    return a;

	  }
}
