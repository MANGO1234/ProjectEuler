package mango123.util;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * My array utility methods.
 * For Project Euler.
 */
public class ArraysUtils {
	//********************************************************************************************************
	//*************************************Convert Primitive Wrappers to Primitive****************************
	//********************************************************************************************************
	/**
	 * Convert a wrapper Character[] array to a primitive char[] array.
	 * @param array a Character[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static char[] toPrimitive(Character[] array) {
		if (array == null) { return null; }

		int len = array.length;
		char[] primitives = new char[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].charValue();
		}
		return primitives;
	}
	
	/**
	 * Convert a wrapper Integer[] array to a primitive int[] array.
	 * @param array an Integer[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static int[] toPrimitive(Integer[] array) {
		if (array == null) { return null; }
		
		int len = array.length;
		int[] primitives = new int[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].intValue();
		}
		return primitives;
	}
	
	/**
	 * Convert a wrapper Long[] array to a primitive long[] array.
	 * @param array a Long[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static long[] toPrimitive(Long[] array) {
		if (array == null) { return null; }

		int len = array.length;
		long[] primitives = new long[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].longValue();
		}
		return primitives;
	}
	
	/**
	 * Convert a wrapper Double[] array to a primitive double[] array.
	 * @param array a Double[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static double[] toPrimitive(Double[] array) {
		if (array == null) { return null; }

		int len = array.length;
		double[] primitives = new double[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].doubleValue();
		}
		return primitives;
	}
	
	
	/**
	 * Convert a wrapper Float[] array to a primitive float[] array.
	 * @param array a Float[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static float[] toPrimitive(Float[] array) {
		if (array == null) { return null; }

		int len = array.length;
		float[] primitives = new float[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].floatValue();
		}
		return primitives;
	}

	/**
	 * Convert a wrapper Short[] array to a primitive short[] array.
	 * @param array a Short[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static short[] toPrimitive(Short[] array) {
		if (array == null) { return null; }

		int len = array.length;
		short[] primitives = new short[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].shortValue();
		}
		return primitives;
	}

	/**
	 * Convert a wrapper Byte[] array to a primitive byte[] array.
	 * @param array a Short[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static byte[] toPrimitive(Byte[] array) {
		if (array == null) { return null; }

		int len = array.length;
		byte[] primitives = new byte[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].byteValue();
		}
		return primitives;
	}

	/**
	 * Convert a wrapper Boolean[] array to a primitive boolean[] array.
	 * @param array a Boolean[] array. It this is null, null is returned
	 * @return the primitive version of the wrapper array
	 */
	public static boolean[] toPrimitive(Boolean[] array) {
		if (array == null) { return null; }

		int len = array.length;
		boolean[] primitives = new boolean[len];
		for (int i = 0; i < len; ++i) {
			primitives[i] = array[i].booleanValue();
		}
		return primitives;
	}

	//********************************************************************************************************
	//*************************************Convert Primitive to Primitive Wrappers****************************
	//********************************************************************************************************
	/**
	 * Convert a char[] array to the wrapper Character[] array.
	 * @param array a char[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Character[] toWrapper(char[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Character[] wrappers = new Character[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Character.valueOf(array[i]); }
		return wrappers;
	}
	
	/**
	 * Convert a int[] array to the wrapper Integer[] array.
	 * @param array an int[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Integer[] toWrapper(int[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Integer[] wrappers = new Integer[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Integer.valueOf(array[i]); }
		return wrappers;
	}
	
	/**
	 * Convert a long[] array to the wrapper Long[] array.
	 * @param array an long[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Long[] toWrapper(long[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Long[] wrappers = new Long[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Long.valueOf(array[i]); }
		return wrappers;
	}
	
	/**
	 * Convert a double[] array to the wrapper Double[] array.
	 * @param array an double[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Double[] toWrapper(double[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Double[] wrappers = new Double[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Double.valueOf(array[i]); }
		return wrappers;
	}
	
	/**
	 * Convert a float[] array to the wrapper Float[] array.
	 * @param array an float[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Float[] toWrapper(float[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Float[] wrappers = new Float[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Float.valueOf(array[i]); }
		return wrappers;
	}

	/**
	 * Convert a short[] array to the wrapper Short[] array.
	 * @param array an short[] array
	 * @return the wrapper version of the primitive array
	 */
	public static Short[] toWrapper(short[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Short[] wrappers = new Short[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Short.valueOf(array[i]); }
		return wrappers;
	}

	
	/**
	 * Convert a byte[] array to the wrapper Byte[] array.
	 * @param array an byte[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Byte[] toWrapper(byte[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Byte[] wrappers = new Byte[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Byte.valueOf(array[i]); }
		return wrappers;
	}

	/**
	 * Convert a boolean[] array to the wrapper Boolean[] array.
	 * @param array an boolean[] array. It this is null, null is returned
	 * @return the wrapper version of the primitive array
	 */
	public static Boolean[] toWrapper(boolean[] array) {
		if (array == null) { return null; }

		int len = array.length;
		Boolean[] wrappers = new Boolean[len];
		for (int i = 0; i < len; ++i) { wrappers[i] = Boolean.valueOf(array[i]); }
		return wrappers;
	}
	
	//********************************************************************************************************
	//********************************Remove duplicates retaining order (slower)******************************
	//********************************************************************************************************
	//* Note: no removeDuplicates for boolean[] because face it, true/false value... not much need

	/**
	 * Creates a new array of chars with all duplicate elements from the original array removed.
	 * Order is retained for each of the elements.
	 * If you do not care about the order, use sortAndRemoveDuplicates(Type array) because it's faster.
	 * @param array
	 * @return a copy of the original array with no duplicate elements
	 */
	public static char[] removeDuplicates(char[] array) {
		//in case it's an empty array
		if (array.length == 0) return new char[0];
		
		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<Character> set = new LinkedHashSet<Character>(Arrays.asList(toWrapper(array))); 

		//convert it back to a primitive char array and return it
		Character[] noDuplicates = new Character[set.size()]; 
		set.toArray(noDuplicates);
		return toPrimitive(noDuplicates);
	}
	
	/**
	 * Creates a new array of integers with all duplicate elements from the original array removed.
	 * Order is retained for each of the elements.
	 * If you do not care about the order, use sortAndRemoveDuplicates(Type array) because it's faster.
	 * @param array
	 * @return a copy of the original array with no duplicate elements
	 */
	public static int[] removeDuplicates(int[] array) {
		//in case it's an empty array
		if (array.length == 0) return new int[0];
		
		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<Integer> set = new LinkedHashSet<Integer>(Arrays.asList(toWrapper(array))); 

		//convert it back to a primitive integer array and return it
		Integer[] noDuplicates = new Integer[set.size()]; 
		set.toArray(noDuplicates);
		return toPrimitive(noDuplicates);
	}
	
	/**
	 * Creates a new array of longs with all duplicate elements from the original array removed.
	 * Order is retained for each of the elements.
	 * If you do not care about the order, use sortAndRemoveDuplicates(Type array) because it's faster.
	 * @param array
	 * @return a copy of the original array with no duplicate elements
	 */
	public static long[] removeDuplicates(long[] array) {
		//in case it's an empty array
		if (array.length == 0) return new long[0];
		
		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<Long> set = new LinkedHashSet<Long>(Arrays.asList(toWrapper(array))); 

		//convert it back to a primitive long array and return it
		Long[] noDuplicates = new Long[set.size()]; 
		set.toArray(noDuplicates);
		return toPrimitive(noDuplicates);
	}

	/**
	 * Creates a new array of doubles with all duplicate elements from the original array removed.
	 * Order is retained for each of the elements.
	 * If you do not care about the order, use sortAndRemoveDuplicates(Type array) because it's faster.
	 * @param array
	 * @return a copy of the original array with no duplicate elements
	 */
	public static double[] removeDuplicates(double[] array) {
		//in case it's an empty array
		if (array.length == 0) return new double[0];
		
		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<Double> set = new LinkedHashSet<Double>(Arrays.asList(toWrapper(array))); 

		//convert it back to a primitive short array and return it
		Double[] noDuplicates = new Double[set.size()]; 
		set.toArray(noDuplicates);
		return toPrimitive(noDuplicates);
	}

	/**
	 * Creates a new array of shorts with all duplicate elements from the original array removed.
	 * Order is retained for each of the elements.
	 * If you do not care about the order, use sortAndRemoveDuplicates(Type array) because it's faster.
	 * @param array
	 * @return a copy of the original array with no duplicate elements
	 */
	public static short[] removeDuplicates(short[] array) {
		//in case it's an empty array
		if (array.length == 0) return new short[0];
		
		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<Short> set = new LinkedHashSet<Short>(Arrays.asList(toWrapper(array))); 

		//convert it back to a primitive integer array and return it
		Short[] noDuplicates = new Short[set.size()]; 
		set.toArray(noDuplicates);
		return toPrimitive(noDuplicates);
	}
	
	/**
	 * Creates a new array of bytes with all duplicate elements from the original array removed.
	 * Order is retained for each of the elements.
	 * If you do not care about the order, use sortAndRemoveDuplicates(Type array) because it's faster.
	 * @param array
	 * @return a copy of the original array with no duplicate elements
	 */
	public static byte[] removeDuplicates(byte[] array) {
		//in case it's an empty array
		if (array.length == 0) return new byte[0];

		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<Byte> set = new LinkedHashSet<Byte>(Arrays.asList(toWrapper(array))); 

		//convert it back to a primitive byte array and return it
		Byte[] noDuplicates = new Byte[set.size()]; 
		set.toArray(noDuplicates);
		return toPrimitive(noDuplicates);
	}
	
	/**
	 * Creates a new array of objects with all duplicate elements from the original array removed.
	 * However, if the original array has a length of 0, the original array will be returned.
	 * Order is retained for each of the elements.
	 * 
	 * ***NOTE***
	 * The object you sent in need to has its hashCode and equals override.
	 * Essentially, this method uses LinkedHashSet to remove the duplicates, and linkedHashSet goes through these steps to determine whether two objects are equal.
	 *  1. Checked whether two object's hashCode using .hashCode() are the same or not.
	 *  	* If they aren't, they are not equal and the object is added.
	 *  	* If they are, go to step 2.
	 *  2. Check the two elements with .equals().
	 *  	* If it returns true, they are equal and the object is not added.
	 *      * Else, the object is added.
	 *      
	 *  Therefore, in order to remove duplicates elements, you need to override hashCode and equals.
	 *  (This is only true if you are checking for equality and not identity)
	 * @param array
	 * @return a copy of the original array with no duplicate elements, unless the original array has a length of 0, in which case the original array is returned.
	 */
	@SuppressWarnings({"unchecked"})
	public static <T> T[] removeDuplicates(T[] array) {
		//in case it's an empty array
		if (array.length == 0) return array;
		
		//create a linked hash set that would remove all the duplicates of array while retaining order
		//-> need to convert to Wrapper to use it though
		Set<T> set = new LinkedHashSet<T>(Arrays.asList(array)); 

		//return the array (involves getting the class the array contains and creating a new instance of an array of the class)
		return set.toArray((T[]) Array.newInstance(array.getClass().getComponentType(), set.size()));
	}

	//********************************************************************************************************
	//*************************************Remove duplicates, sorted (faster)*********************************
	//********************************************************************************************************
	/**
	 * Creates a new char array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * ok i don't know why i thought this is faster == sort is like O(n log n) and normal duplicates are like
	 * O(n) gg
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static char[] sortAndRemoveDuplicates(char[] array) {
		int len = array.length,
			count = 0;
		char previous = 0;
		
		//create a sorted copy
		char[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		char[] noDuplicates = new char[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}
	
	/**
	 * Creates a new integer array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static int[] sortAndRemoveDuplicates(int[] array) {
		if (array.length == 0) return array;

		int len = array.length,
			previous = 0,
			count = 0;
		
		//create a sorted copy
		int[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		int[] noDuplicates = new int[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}
	
	/**
	 * Creates a new long array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static long[] sortAndRemoveDuplicates(long[] array) {
		int len = array.length,
			count = 0;
		long previous = 0;
		
		//create a sorted copy
		long[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		long[] noDuplicates = new long[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}
	
	/**
	 * Creates a new double array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static double[] sortAndRemoveDuplicates(double[] array) {
		int len = array.length,
			count = 0;
		double previous = 0;
		
		//create a sorted copy
		double[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		double[] noDuplicates = new double[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}
	
	/**
	 * Creates a new float array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static float[] sortAndRemoveDuplicates(float[] array) {
		int len = array.length,
			count = 0;
		float previous = 0;
		
		//create a sorted copy
		float[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		float[] noDuplicates = new float[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}
	
	/**
	 * Creates a new short array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static short[] sortAndRemoveDuplicates(short[] array) {
		int len = array.length,
			count = 0;
		short previous = 0;
		
		//create a sorted copy
		short[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		short[] noDuplicates = new short[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}
	
	/**
	 * Creates a new byte array from the original array that has been sorted and duplicates removed.
	 * This is faster than removeDuplicates(Type array), so if you don't care about ordering, use this.
	 * @param array
	 * @return a new array that has its duplicates removed and sorted
	 */
	public static byte[] sortAndRemoveDuplicates(byte[] array) {
		int len = array.length,
			count = 0;
		byte previous = 0;
		
		//create a sorted copy
		byte[] copy = Arrays.copyOf(array, len);
		Arrays.sort(copy);

		//remove the duplicates -> they will be together
		byte[] noDuplicates = new byte[len];
		previous = copy[0];
		noDuplicates[0] = previous;
		
		for (int i = 1; i < len; ++i) {
			if (previous != copy[i]) {
				previous = copy[i];
				noDuplicates[++count] = previous;
			}
		}
		return Arrays.copyOf(noDuplicates, count + 1);
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static int[] reverse(int[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			int temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static long[] reverse(long[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			long temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static char[] reverse(char[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			char temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static short[] reverse(short[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			short temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static byte[] reverse(byte[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			byte temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static boolean[] reverse(boolean[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			boolean temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static float[] reverse(float[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			float temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static double[] reverse(double[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			double temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

	/**
	 * Reverse a section of array locally (does not create new array!) identified by start and end.
	 * Note: start is inclusive, end is exlusive, similar to Arrays.copyOf();
	 * @param start start index
	 * @param end end index
	 * @return the original array
	 */
	public static <T> T[] reverse(T[] arr, int start, int end) {
		int upto = (start + end) / 2;
		for (int i = start; i < upto; i++) {
			T temp = arr[i];
			arr[i] = arr[end - (i - start) - 1];
			arr[end - (i - start) - 1] = temp;
		}
		return arr;
	}

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(int[] arr, int element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(long[] arr, long element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(char[] arr, char element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(short[] arr, short element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(byte[] arr, byte element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(boolean[] arr, boolean element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(float[] arr, float element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static int indexOf(double[] arr, double element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @param index positive int, start looking at that index
     * @return the original array
     */
    public static <T> int indexOf(T[] arr, T element, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("index < 0");
        }
        for (int i = index; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(int[] arr, int element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(long[] arr, long element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(char[] arr, char element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(short[] arr, short element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(byte[] arr, byte element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(boolean[] arr, boolean element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(float[] arr, float element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static int indexOf(double[] arr, double element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Find index of element in array. Return -1 if not found.
     *
     * @param arr array
     * @param element element to look for
     * @return the original array
     */
    public static <T> int indexOf(T[] arr, T element) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == element) {
                return i;
            }
        }
        return -1;
    }

	// temp
	public static long max(long[] arr) {
		long k = arr[0];
		for (int i = 1; i < arr.length; i++) {
			k = Math.max(k, arr[i]);
		}
		return k;
	}

	public static long min(long[] arr) {
		long k = arr[0];
		for (int i = 1; i < arr.length; i++) {
			k = Math.min(k, arr[i]);
		}
		return k;
	}
}
