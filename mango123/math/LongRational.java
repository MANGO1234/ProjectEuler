package mango123.math;

/**
 * Immutable class representing the rationals. Support very basic arithmetic.
 * The rational will always be in lowest reduced form.
 */
public final class LongRational implements Comparable<LongRational> {
	final public long num;
	final public long den;

	/**
	 * Constructs a rational number with the given numerator and denominator.
	 * @param numerator
	 * @param denominator
	 */
	public LongRational(long numerator, long denominator) {
		if (denominator == 0) throw new ArithmeticException("LongRational: denominator cannot be 0");
		if (denominator < 0) {
			denominator = -denominator;
			numerator = -denominator;
		}
		long a = Math.abs(EulerMath.GCD(numerator, denominator));
		den = denominator / a;
		num = numerator / a;
	}

	public LongRational inverse() {
		return new LongRational(den, num);
	}

	public LongRational negate() {
		return new LongRational(-num, den);
	}

	public LongRational add(LongRational rat) {
		return new LongRational(this.num*rat.den + rat.num*this.den, this.den * rat.den);
	}

	public LongRational sub(LongRational rat) {
		return new LongRational(this.num*rat.den - rat.num*this.den, this.den * rat.den);
	}

	public LongRational mul(LongRational rat) {
		return new LongRational(rat.num * this.num, rat.den * this.den);
	}

	public LongRational div(LongRational rat) {
		return new LongRational(this.num * rat.den, this.den * rat.num);
	}


	@Override
	public int compareTo(LongRational o) {
		if (num == o.num) {
			if (den > o.den)       return -1;
			else if (den < o.den) return 1;
			return 0;
		}
		if (den == o.den) {
			return num > o.num ? 1 : -1;
		}
		return num * o.den > o.num * den ? 1 : -1;
	}



	/**
	 * Return a copy of the current rational number.
	 * @return a copy of current rational
	 */
	public LongRational copy() {
		return new LongRational(num, den);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		LongRational rational = (LongRational) o;

		if (den != rational.den) return false;
		if (num != rational.num) return false;

		return true;
	}

	@Override
	public int hashCode() {
		long result = num;
		result = 31 * result + den;
		return (int) result;
	}

	@Override
	public String toString() {
		return "LongRational{" +
				"num=" + num +
				", den=" + den +
				'}';
	}
}
