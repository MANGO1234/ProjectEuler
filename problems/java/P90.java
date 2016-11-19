import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;
import mango123.math.LongRational;

import java.io.*;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.*;
import java.util.regex.*;

public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();
		// 90% of time is spend on this single line lol
		// generating the sets manually would reduce the running time from 70ms to 6ms
		int[] sets = IntStream.range(0, 1 << 10).filter(i -> Integer.bitCount(i) == 6).toArray();

		int[][] constraints = new int[][] {
				new int[]{(1 << 10) + (1 << 1)},
				new int[]{(1 << 10) + (1 << 4)},
				new int[]{(1 << 10) + (1 << 9),(1 << 10) + (1 << 6)},
				new int[]{(1 << 11) + (1 << 6),(1 << 11) + (1 << 9)},
				new int[]{(1 << 12) + (1 << 5)},
				new int[]{(1 << 13) + (1 << 6),(1 << 13) + (1 << 9)},
				new int[]{(1 << 14) + (1 << 9),(1 << 14) + (1 << 6)},
				new int[]{(1 << 16) + (1 << 4),(1 << 19) + (1 << 4)},
				new int[]{(1 << 18) + (1 << 1)}
		};

		int count = 0;
		for (int i = 0; i < sets.length; i++) {
			for (int j = i; j < sets.length; j++) {
				// go through each constraint and check whether the two set passes the constraint
				// count only ++ when all constrains pass
				boolean valid = false;
				for (int k = 0; k < constraints.length; k++) {
					for (int l = 0; l < constraints[k].length; l++) {
						valid = (((sets[i] << 10) + sets[j]) & constraints[k][l]) == constraints[k][l];
						if (valid) break;
						valid = (((sets[j] << 10) + sets[i]) & constraints[k][l]) == constraints[k][l];
						if (valid) break;
					}
					if (!valid) break;
				}
				if (valid) count++;
			}
		}
		System.out.println(count);
	}
}
