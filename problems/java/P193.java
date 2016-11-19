import mango123.math.BigRational;
import mango123.math.EratosthenesSieve;

public class Euler {
	public static void main(String[] args) {
		long START = System.currentTimeMillis();

		all(0, 1, false);
		System.out.println(sum);

		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}

	public static long sum = 0;
	public static int[] p = new EratosthenesSieve(1 << 25).toArray();

	public static void all(int i, long a, boolean isEven) {
		if (!isEven) sum += (1l << 50) / (a * a);
		else         sum -= (1l << 50) / (a * a);
		for (int j = i; j < p.length; j++) {
			long s = a * p[j];
			if (s > (1 << 25)) break;
			all(j + 1, s, !isEven);
		}
	}
}
