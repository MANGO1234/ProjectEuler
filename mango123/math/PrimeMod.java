package mango123.math;

/**
 * Created by mango123 on 2015-01-24.
 */
public final class PrimeMod {
	private long prime;
	private long rem = 0;

	public int getValue() {
		return (int) rem;
	}

	public PrimeMod(int prime, int rem) {
		this.prime = prime;
		this.rem = rem;
	}

	public void add(int n) {
		rem = (rem + n) % prime;
	}

	public void sub(int n) {
		rem = (rem - n) % prime;
	}

	public void mul(int n) {
		rem = (rem * n) % prime;
	}

	// divide by n (equivalent to multiplying by n^(p-2)
	private long[] c = new long[1000000];
	public void div(int n) {
		long l;
		if (c[n] == 0) {
			l = quickPow(n, (int) prime - 2);
			c[n] = l;
		}
		else {
			l = c[n];
		}
		rem = (rem * l) % prime;
	}

	public void pow(int n) {
		rem = quickPow(rem, n);
	}

	// calculate rem * (n^pow)
	public void mulPow(int n, int pow) {
		long l = quickPow(n, pow);
		rem = (rem * l) % prime;
	}

	public long quickPow(long r, int pow) {
		long temp = 1;
		while (pow != 0) {
			if ((pow & 1) == 1) {
				temp = (temp * r) % prime;
			}
			r = (r * r) % prime;
			pow >>= 1;
		}
		return temp;
	}
}
