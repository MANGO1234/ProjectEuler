public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		final long MOD = 100000007;
		long[] fact = new long[800000];
		long[] pow2 = new long[1000000/3+3];
		long[] inv = new long[1000000/2+2];
		long[] invFac = new long[1000000/2+2];
		fact[0] = 1;
		pow2[0] = 1;
		inv[1] = 1;
		inv[0] = 1;
		invFac[0] = 1;
		invFac[1] = 1;
		for (int i = 1; i < fact.length; i++) {
			fact[i] = (fact[i - 1] * i) % MOD;
		}
		for (int i = 1; i < pow2.length; i++) {
			pow2[i] = (pow2[i - 1] * 2) % MOD;
		}
		for (int i = 2; i < inv.length; i++) {
			inv[i] = quickPow(i, (int) (MOD - 2), MOD);
			invFac[i] = (invFac[i-1] * inv[i]) % MOD;
		}

		long sum = 0;

		int extra = 0;
		int a = (1000000-4) / 2;
		int b = 1;
		while (a >= 0) {
			long ways = 1;
			ways = (ways * fact[a+b+1]) % MOD;
			ways = (ways * fact[b+extra]) % MOD;
			ways = (ways * fact[a+b]) % MOD;
			ways = (ways * pow2[b]) % MOD;
			ways = (ways * fact[a+b]) % MOD;
			ways = (ways * invFac[a]) % MOD;
			ways = (ways * invFac[b]) % MOD;
			sum = (sum + ways) % MOD;
			a -= 3;
			b += 2;
		}

		extra = 1;
		a = (1000000 - 2) / 2;
		b = 0;
		while (a >= 0) {
			long ways = 1;
			ways = (ways * fact[a+b]) % MOD;
			ways = (ways * fact[a+b+1]) % MOD;
			ways = (ways * fact[b+extra]) % MOD;
			ways = (ways * fact[a+b]) % MOD;
			ways = (ways * pow2[b]) % MOD;
			ways = (ways * invFac[a]) % MOD;
			ways = (ways * invFac[b]) % MOD;
			sum = (sum + ways) % MOD;
			sum = (sum + ways) % MOD;
			a -= 3;
			b += 2;
		}

		extra = 2;
		a = (1000000 - 6) / 2;
		b = 1;
		while (a >= 0) {
			long ways = 1;
			ways = (ways * fact[a+b]) % MOD;
			ways = (ways * fact[a+b+1]) % MOD;
			ways = (ways * fact[b+extra]) % MOD;
			ways = (ways * fact[a+b]) % MOD;
			ways = (ways * pow2[b]) % MOD;
			ways = (ways * invFac[a]) % MOD;
			ways = (ways * invFac[b]) % MOD;
			sum = (sum + ways) % MOD;
			a -= 3;
			b += 2;
		}

		System.out.println(sum);
		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}

	public static long quickPow(long r, int pow, long primeMod) {
		long temp = 1;
		while (pow != 0) {
			if ((pow & 1) == 1) {
				temp = (temp * r) % primeMod;
			}
			r = (r * r) % primeMod;
			pow >>= 1;
		}
		return temp;
	}
}