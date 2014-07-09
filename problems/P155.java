import java.util.*;
import mango123.math.Rational;

public class Euler {
	public static void main(String[] args) {
		long START = System.currentTimeMillis();
		// brute force, welp given it is so easy to code, it's acceptable
		// no real elegant way beside some tricks exploiting symmetry, using booleans instead of sets
		// rad forum if want more
		final int N = 16;
		List<Set<Rational>> c = new ArrayList<Set<Rational>>(N+1);
		for (int i = 0; i <= N; i++) {
			c.add(i, new HashSet<Rational>());
		}

		c.get(1).add(new Rational(1, 1));
		for (int i = 2; i <= N; i++) {
			Set<Rational> temp = c.get(i);
			for (int j = 1; j <= i/2; j++) {
				for (Rational a : c.get(j)) {
					for (Rational b : c.get(i-j)) {
						temp.add(a.add(b));
						temp.add((a.inverse().add(b.inverse())).inverse());
					}
				}
			}
			System.out.println("Done "+i);
		}

		Set<Rational> r = c.get(0);
		for (int i = 1; i <= N; i++) {
			r.addAll(c.get(i));
			c.set(i, null);
			System.out.println(i + ": " + r.size());
		}

		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}
}
