import mango123.math.EratosthenesSieve;
import mango123.math.PrimesUtil;
import mango123.miscellaneous.Digit;
import mango123.util.ArraysUtils;

import java.io.*;
import java.util.*;
import java.util.stream.*;

public class Euler {
public static void main(String[] args) {
	final long START = System.currentTimeMillis();

	EratosthenesSieve primes = new EratosthenesSieve(100_000_000);
	final int LIMIT = 9000;
	int[] p = new EratosthenesSieve(LIMIT).toArray();

	Collection<int[]> m1 = new ArrayList<>();
	Collection<int[]> m2 = new ArrayList<>();
	m1.add(new int[]{3,0,0,0,0});
	m2.add(new int[]{3,0,0,0,0});

	for (int i = 3; i < p.length; i++) {
		Collection<int[]> m = p[i]%3==1 ? m1 : m2;

		Map<Integer, Boolean> visited = new HashMap<>();
		Set<int[]> newSets = new HashSet<>();
		boolean solutionFound = false;
		for (int[] sets : m) {
			int j = 0;
			Boolean concatToPrime = Boolean.FALSE;
			while (j < sets.length && sets[j] != 0) {
				 concatToPrime = visited.get(sets[j]);
				if (concatToPrime == null) {
					// a simple primalit check vs EratosthenesSieve has ~ same performance
					// almost no difference (~20 ms)
					concatToPrime = primes.isPrime(combine(sets[j], p[i])) && primes.isPrime(combine(p[i], sets[j]));
					visited.put(sets[j], concatToPrime);
				}
				j++;
				if (!concatToPrime) break;
			}

			if (concatToPrime) { // true only when every integer in set can concat to prime with given prime
				if (j == 4) { // solution found, well maybe, it just happens this works
					System.out.println("Sum: " + (Arrays.stream(sets).sum() + p[i]));
					solutionFound = true;
					break;
				}
				int[] newSet = Arrays.copyOf(sets, sets.length);
				newSet[j] = p[i];
				newSets.add(newSet);
			}
		}
		if (solutionFound) break;
		m.addAll(newSets);
		m.add(new int[]{p[i],0,0,0,0});
	}

	long END = System.currentTimeMillis();
	System.out.println("Time: "+ (END - START) +"ms");
}


private static final int[] INT_POW_10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};
private static int combine(int a, int b) {
	return a * INT_POW_10[Digit.numOfDigits(b)] + b;
}

/*	public static BigInteger sum = BigInteger.ZERO;
	public static int[] p = new EratosthenesSieve(100).toArray();
	public static final long UPTO = 10_000_000_000_000_000l;

	public static void all(int i, long a, int num) {
		//all(0, 1, 0);
		//System.out.println(sum);
		/*int[] p = new EratosthenesSieve(35).toArray();
		LongRational r = new LongRational(1, 1);
		LongRational LIMIT = new LongRational(4, 10);
		long k = 1;
		for (int i = 0; i < 20; i++) {
			r = r.mul(new LongRational(p[i] - 1, p[i]));
			k *= p[i];
			//106696590
			boolean found = false;
				if (r.mul(new LongRational(k, k - 1)).compareTo(LIMIT) < 0) break;
				for (int j = 0; j <= i; j++) {
					LongRational temp = r;
					r = r.add(r.mul(new LongRational(1, p[j] * k - 1)));
					if (r.compareTo(LIMIT) < 0) {
						found = true;
						k *= p[j];
						break;
					}
					r = temp;
				}
			if (found) break;
		}
		System.out.println(r);
		System.out.println(k);


		if (num >= 4) {
			if (num % 2 == 0) sum = sum.add(BigInteger.valueOf(UPTO / a));
			else             sum = sum.subtract(BigInteger.valueOf(UPTO / a).multiply(BigInteger.valueOf(EulerMath.nCr(num, num-4))));
		}
		for (int j = i; j < p.length; j++) {
			long s = a * p[j];
			if (s > UPTO) break;
			all(j + 1, s, num + 1);
		}
	}//*/
}
