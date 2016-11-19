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
		long NUM = 71328803586048l;
		long[] factors = EulerMath.divisorsOf(NUM);
		long sum = 0;
		for (int i = 0; i < factors.length; i++) {
			long j = NUM / factors[i];
			sum = (sum + P2(factors[i], j).mod(BigInteger.valueOf(100000000)).longValue()) % 100000000;
		}
		System.out.println(sum);

		System.out.println(Arrays.toString(factors));
		System.out.println(factors.length);
		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}

	private static BigInteger P2(long f, long r) {
		if (f == 1) {
			BigInteger rn = BigInteger.valueOf(r);
			return rn.multiply(rn.add(BigInteger.ONE)).divide(BigInteger.valueOf(2));
		}

		if (r == 1) {
			BigInteger rn = BigInteger.valueOf(f);
			return rn.multiply(rn).divide(BigInteger.valueOf(2));
		}

		if (r == 2) {
			long fn = f % 2 == 0 ? f + 1 : f;
			BigInteger rn = BigInteger.valueOf(fn);
			return rn.multiply(rn).subtract(P2(f, 1));
		}

		long parity, k, a;
		if (r % 2 == 1) {
			k = (r + 1) / 2;
			parity = 1;
		}
		else {
			k = r / 2;
			parity = 2;
		}
		if (f % 2 == 1) {
			if (r % 2 == 1) {
				a = 2 * f + 1;
			}
			else {
				a = 2 * f + 3;
			}
		}
		else {
			if (r % 2 == 1) {
				a = 2 * f + 3;
			}
			else {
				a = 2 * f + 5;
			}
		}
		BigInteger kn = BigInteger.valueOf(k - 1);
		return P2(f, parity)
				.add(BigInteger.valueOf(a).multiply(kn))
				.add(BigInteger.valueOf(2).multiply(kn).multiply(kn.subtract(BigInteger.ONE)));
	}
}
