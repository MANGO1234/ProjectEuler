import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;

import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;
import mango123.math.BigRational;
import mango123.math.linear.Vector;
import org.jscience.mathematics.number.LargeInteger;

public class Euler {
	public static void main(String[] args) {
		long START = System.currentTimeMillis();
		BigInteger[] p = Arrays.stream(new EratosthenesSieve(190).toArray())
				.mapToObj(BigInteger::valueOf)
				.toArray(i -> new BigInteger[i]);
		LargeInteger sqt = Arrays.stream(p)
				.map(LargeInteger::valueOf)
				.reduce(LargeInteger.ONE, LargeInteger::times)
				.sqrt(); // pretty much for this one function...
		BigInteger sq = new BigInteger(sqt.toString());

		BigInteger[] p1 = Arrays.copyOfRange(p, 0, 21);
		BigInteger[] p2 = Arrays.copyOfRange(p, 21, 42);

		// Incorporate the sorting during generation of subset products (via merging the two sroted list generated at step)
		// to get 2^(N/2) running time, 6sec -> 2sec for this step
		// it takes more work though =.=
		BigInteger[] s1 = new BigInteger[2];
		BigInteger[] s2 = new BigInteger[2];
		int l = 0;
		s1[0] = s2[0] = BigInteger.ONE;
		while (l < p1.length) {
			int k = 1 << l;
			int k2 = 1 << (l + 1);
			for (int i = 0; i < k; i++) {
				s1[k + i] = s1[i].multiply(p1[l]);
				s2[k + i] = s2[i].multiply(p2[l]);
			}

			BigInteger[] temp = new BigInteger[1 << ((l == p1.length - 1) ? l + 1 : l + 2)];
			int i = 0, j = k, m = 0;
			while (m < k2) {
				if (j >= k2 || (i < k && s1[i].compareTo(s1[j]) < 0)) {
					temp[m] = s1[i];
					i++;
				}
				else {
					temp[m] = s1[j];
					j++;
				}
				m++;
			}
			s1 = temp;

			temp = new BigInteger[1 << ((l == p1.length - 1) ? l + 1 : l + 2)];
			i = 0; j = k; m = 0;
			while (m < k2) {
				if (j >= k2 || (i < k && s2[i].compareTo(s2[j]) < 0)) {
					temp[m] = s2[i];
					i++;
				}
				else {
					temp[m] = s2[j];
					j++;
				}
				m++;
			}
			s2 = temp;

			l++;
		}

		BigInteger max = BigInteger.ZERO;
		int i = 0, j = s2.length - 1;
		while (i < s1.length) {
			BigInteger temp = s1[i].multiply(s2[j]);
			if (temp.compareTo(sq) < 0) {
				if (temp.compareTo(max) > 0) max = temp;
				i++;
			}
			else {
				j--;
			}
		}
		System.out.println(max.mod(BigInteger.TEN.pow(16)));//*/

		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}
}
