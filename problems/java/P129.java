import java.util.*;
import mango123.math.Rational;
import mango123.math.linear.Vector;

public class Euler {
	public static void main(String[] args) {
		long START = System.currentTimeMillis();
		final int UPPER = 1000000;
		for (int n = UPPER + ((UPPER & 1) == 0 ? 1 : 0); true; n += 2) {
			if (n % 5 == 0) continue;
			int r = 1;
			int k = 1;
			for (int i = 1; i < n; i++) {
				r = (r * 10 + 1) % n;
				k++;
				if (r == 0) break;
			}

			if (k > UPPER) {
				System.out.println(n);
				break;
			}
		}


		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}
}
