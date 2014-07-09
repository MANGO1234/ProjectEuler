package mango123.math;

/**
 * Immutable class representing the rationals. Backed by ints. Support very basic arithmetic.
 * The rational will always be in lowest reduced form.
 */
public final class Rational implements Comparable<Rational> {
	final public int num;
	final public int den;

	/**
	 * Constructs a rational number with the given numerator and denominator.
	 * @param numerator
	 * @param denominator
	 */
	public Rational(int numerator, int denominator) {
		if (denominator == 0) throw new ArithmeticException("Rational: denominator cannot be 0");
		if (denominator < 0) {
			denominator = -denominator;
			numerator = -denominator;
		}
		int a = Math.abs(EulerMath.GCD(numerator, denominator));
		den = denominator / a;
		num = numerator / a;
	}
	
	public Rational inverse() {
		return new Rational(den, num);
	}
	
	public Rational negate() {
		return new Rational(-num, den);
	}

	public Rational add(Rational rat) {
		return new Rational(this.num*rat.den + rat.num*this.den, this.den * rat.den);
	}

	public Rational sub(Rational rat) {
		return new Rational(this.num*rat.den - rat.num*this.den, this.den * rat.den);
	}

	public Rational mul(Rational rat) {
		return new Rational(rat.num * this.num, rat.den * this.den);
	}

	public Rational div(Rational rat) {
		return new Rational(this.num * rat.den, this.den * rat.num);
	}


	@Override
	public int compareTo(Rational o) {
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
	public Rational copy() {
		return new Rational(num, den);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		Rational rational = (Rational) o;

		if (den != rational.den) return false;
		if (num != rational.num) return false;

		return true;
	}

	@Override
	public int hashCode() {
		int result = num;
		result = 31 * result + den;
		return result;
	}

	@Override
	public String toString() {
		return "Rational{" +
				"num=" + num +
				", den=" + den +
				'}';
	}
}