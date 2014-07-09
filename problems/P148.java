import java.util.*;


public class Euler {
	public static long[] p7 = new long[12];
	public static long[] fp7 = new long[12];

	public static void main(String[] args) {
		p7[1] = 7;
		fp7[1] = 0;
		for (int i = 2; i < p7.length; i++) {
			p7[i] = p7[i - 1] * 7;
			fp7[i] = fp7[i - 1] * 28 + (p7[i - 1] - 1) / 2 * p7[i - 1] * 21;
		}

		long START = System.currentTimeMillis();
		long a = 1000000000l;
		System.out.println(a * (a + 1) / 2 - f(a));
		long END = System.currentTimeMillis();
		System.out.println(END - START);
	}

	public static long f(long r) {
		if (r <= 7) return 0;
		int i = 0;
		while (p7[i + 1] < r) i++;
		long rem = r % p7[i];
		long quot = r / p7[i];

		System.out.println(quot + "*" + p7[i] + "+" + rem);

		long tri = (p7[i] - 1) / 2 * p7[i];
		long botTri = p7[i] - rem - 1;
		botTri = botTri * (botTri + 1) / 2;
		long a = fp7[i] * (quot + 1) * quot / 2 + quot * (quot - 1) / 2 * tri;
		a += (quot + 1) * f(rem) - quot * botTri + quot * tri;
		return a;
	}
}
