import java.util.*;

import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;
import mango123.math.Rational;
import mango123.math.linear.Vector;

public class Euler {
	public static void main(String[] args) {
		long START = System.currentTimeMillis();
		final int SIZE = 1000000;
		EratosthenesSieve prime = new EratosthenesSieve(SIZE);
		int num = 0;
		int sum = 0;

		for (int n = 2; n <= SIZE; n++) {
			if (!prime.isPrime(n) && EulerMath.GCF(n, 10) == 1) {
				int r = 1;
				int k = 1;
				for (int i = 1; i < n; i++) {
					r = (r * 10 + 1) % n;
					k++;
					if (r == 0) break;
				}

				if ((n - 1) % k == 0) {
					num++;
					sum += n;
					System.out.println(n);
				}
				if (num == 25) break;
			}
		}

		System.out.println(sum);


		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}
}
