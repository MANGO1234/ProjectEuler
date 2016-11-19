import mango123.math.*;
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

		String st = "PPPPNNPPPNPPNPN";

		BigRational[] primeC = new BigRational[502];
		BigRational[] nprimeC = new BigRational[502];
		BigRational e13 = new BigRational(1,3);
		BigRational e23 = new BigRational(2,3);
		EratosthenesSieve s = new EratosthenesSieve(100);
		for (int i = 1; i < 501; i++) {
			if (s.isPrime(i)) {
				primeC[i] = e23;
				nprimeC[i] = e13;
			}
			else {
				primeC[i] = e13;
				nprimeC[i] = e23;
			}
		}

		BigRational[] frog = new BigRational[502];
		Arrays.fill(frog, new BigRational(1, 500));
		frog[0] = new BigRational(0, 1);
		frog[501] = new BigRational(0, 1);
		BigRational[] r = st.charAt(0) == 'P'? primeC : nprimeC;
		for (int i = 1; i < 501; i++) {
			frog[i] = frog[i].mul(r[i]);
		}
		BigRational TWO = new BigRational(2, 1);

		for (int i = 1; i < st.length(); i++) {
			BigRational[] newFrog = new BigRational[502];
			BigRational[] call = st.charAt(i) == 'P' ? primeC : nprimeC;
			for (int j = 1; j < 501; j++) {
				newFrog[j] = frog[j - 1].add(frog[j + 1]).div(TWO).mul(call[j]);
			}
			newFrog[2] = newFrog[2].add(frog[1].div(TWO).mul(call[2]));
			newFrog[499] = newFrog[499].add(frog[500].div(TWO).mul(call[499]));
			newFrog[0] = frog[0];
			newFrog[501] = frog[501];
			frog = newFrog;
		}
		BigRational a = new BigRational(0, 1);
		for (int i = 1; i < 501; i++) {
			a = a.add(frog[i]);
		}
		System.out.println(a);
		System.out.println(Arrays.toString(frog));

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}
