import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;
import mango123.math.PrimesUtil;
import mango123.miscellaneous.Digit;
import mango123.miscellaneous.InPlacePermGen;
import mango123.miscellaneous.IntegerDigit;
import mango123.miscellaneous.LexicographicalPermGen;
import mango123.util.ArraysUtils;

import java.io.*;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.*;

public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		int[] primes = new EratosthenesSieve(1000000).toArray();

		long count = 0;
		for (int i = 1; i < primes.length; i++) {
			long ps = (long) primes[i] * primes[i] + 13;
			long n = ps % 6 / 2;
			long  m = ps / 6;
			if (n == 1) {
				n += 3;
				m--;
			}
			count += 2 * (1 + (m - n) / 4);
		}
		System.out.println(count);

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}
