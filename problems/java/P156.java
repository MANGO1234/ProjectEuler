import mango123.util.ArraysUtils;
import java.util.*;

public class Euler {
	public static void main(String[] args) {
			final long START = System.currentTimeMillis();

		long sum = 0;
		for (int i = 1; i <= 9; i++) {
			long k = s(i);
			System.out.println(i+": "+k);
			sum += k;
		}
		System.out.println("sum: " + sum);

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}

	private static final long[] LONG_POW_10 = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
			10000000000l, 100000000000l, 1000000000000l, 10000000000000l,
			100000000000000l, 1000000000000000l, 10000000000000000l,
			100000000000000000l, 1000000000000000000l};
	static long[][] accR = new long[100][10];
	static long[][] maxR = new long[100][10];
	static long[][] minR = new long[100][10];
	public static long s(int d) {
		// p <= 10 guarantees, all solution < 10^10, need this for maxL[0] and minL[0] for getAll
		final int MAX_POWER = 10;
		for (int p = 0; p <= MAX_POWER; p++) {
			long[] accL = accR[p] = new long[10];
			long[] maxL = maxR[p] = new long[10];
			long[] minL = minR[p] = new long[10];
			accL[0] = p == 0 ? -1 : accR[p-1][9];
			maxL[0] = p == 0 ? -1 : ArraysUtils.max(maxR[p-1]);
			minL[0] = p == 0 ? -1 : ArraysUtils.min(minR[p-1]);

			for (int i = 1; i < 10; i++) {
				if (i == d) {
					accL[i] = accL[i-1]+accL[0]+LONG_POW_10[p];
					maxL[i] = accL[i];
					minL[i] = accL[i-1];
				}
				else {
					accL[i] = accL[i-1]+accL[0];
					maxL[i] = accL[i-1]+maxL[0];
					minL[i] = accL[i-1]+minL[0];
				}
			}
		}

		// all solutions are below 10^10, fyi
		long sum = 0;
		List<Long> a = getAll(MAX_POWER, 0, d, 0, 0);
		for (Long x : a) sum += x;
		//System.out.println(a);
		return sum;
	}

	public static List<Long> getAll(int p, int n, int d, long acc, int dCount) {
		List<Long> l = new ArrayList<>();
		if (p == -1) {
			l.add((long) n);
			return l;
		}

		long[] accL = accR[p];
		long[] maxL = maxR[p];
		long[] minL = minR[p];
		long accT, maxT, minT;

		for (int i = 0; i < 10; i++) {
			if (i == d) {
				accT = acc+accL[0]+(dCount + 1)*LONG_POW_10[p];
				maxT = acc+accL[0]+(dCount + 1)*LONG_POW_10[p];
				minT = p == 0 ? maxT : acc;
			}
			else if (dCount > 0) {
				accT = acc+accL[0]+dCount*LONG_POW_10[p];
				maxT = acc+accL[0]+dCount*LONG_POW_10[p];
				minT = p == 0 ? maxT : acc;
			}
			else {
				// we can use the cache accR[p], but this makes code look more beautiful
				accT = acc+accL[0];
				maxT = acc+maxL[0];
				minT = acc+minL[0];
			}

			if (maxT >= -1 && minT <= -1) {
				List<Long> ls = getAll(p - 1, i, d, acc, i == d ? dCount + 1 : dCount);
				for (Long x : ls) {
					l.add(i * (p == 0 ? 0 : LONG_POW_10[p]) + x);
				}
			}
			acc = accT;
		}
		return l;
	}
}