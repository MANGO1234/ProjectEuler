import mango123.math.EratosthenesSieve;

import java.util.PriorityQueue;

public class Euler {
	public static class P implements Comparable<P> {
		public long n;
		public long np;

		public P(int n) {
			this.n = n;
			np = n;
		}

		@Override
		public int compareTo(P o) {
			return this.np < o.np ? -1 : 1;
		}
	}

	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		long MOD = 500500507;
		EratosthenesSieve sieve = new EratosthenesSieve(10000000); // 90% time spend
		int[] primes = sieve.toArray();
		PriorityQueue<P> ps = new PriorityQueue<P>();
		for (int prime : primes) {
			ps.add(new P(prime));
		}
		long answer = 1;
		for (int i = 0; i < 500500; i++) {
			P p = ps.poll();
			answer = answer * p.np % MOD;
			p.np *= p.np;
			ps.add(p);
		}
		System.out.println(answer);

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}