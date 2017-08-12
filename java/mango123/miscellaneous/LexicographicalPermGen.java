package mango123.miscellaneous;

import java.util.Arrays;

/**
 * Permutation generator using the lexicographical algorithm.
 */
public class LexicographicalPermGen<T extends Comparable<T>> implements InPlacePermGen<T> {
	private T[] arr;
	private boolean firstPerm = true;

	public LexicographicalPermGen(T[] arr) {
		this.arr = arr;
		Arrays.sort(arr);
	}

	@Override
	public boolean hasNextPerm() {
		for (int i = arr.length - 2; i >= 0; i--) {
			if (arr[i].compareTo(arr[i + 1]) < 0) return true;
		}
		return false;
	}

	@Override
	public void nextPerm() {
		if (firstPerm) {
			firstPerm = false;
			return;
		}

		// find k
		int k = -1;
		for (int i = arr.length - 2; i >= 0; i--) {
			if (arr[i].compareTo(arr[i + 1]) < 0) {
				k = i;
				break;
			}
		}
		if (k == -1) return;

		// find m
		int m = arr.length - 1;
		while (m > k && arr[m].compareTo(arr[k]) <= 0) m--;

		// swap and reverse
		T temp = arr[m]; arr[m] = arr[k]; arr[k] = temp;
		k++; m = arr.length - 1;
		while (k <= m) {
			temp = arr[m]; arr[m] = arr[k]; arr[k] = temp;
			k++; m--;
		}
	}

	public void reset() {
		Arrays.sort(arr);
		firstPerm = false;
	}
}
