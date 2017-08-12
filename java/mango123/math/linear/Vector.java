package mango123.math.linear;

import java.util.Arrays;

/**
 * An integer vector. Very basic operations.
 */
public class Vector {
	private int[] x;

	public Vector(int... x) {
		if (x.length == 0) throw new IllegalArgumentException("Vector need to have a size > 0");
		this.x = Arrays.copyOf(x, x.length);
	}

	// fast construction internal use
	private Vector(int[] x, int length) {
		this.x = x;
	}

	public static Vector quickCreate(int... x) {
		return new Vector(x, 0);
	}

	public int get(int i) {
		return x[i];
	}

	public int size() {
		return x.length;
	}
}
