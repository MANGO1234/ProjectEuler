package mango123.math.combinatorialGame;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Nim3GameExample {
	public static class Nim3 implements Position<Nim3> {
		// the iterator should hopefully be correct
		private class Nim3Iterator implements Iterator<Nim3> {
			private final int a;
			private final int b;
			private final int c;
			private int co = 0;
			private int vari = 0;

			public Nim3Iterator(int a, int b, int c) {
				this.a = a;
				this.b = b;
				this.c = c;
				if (a == 0) {
					vari = 1;
				}
				if (vari == 1 && b == 0) {
					vari = 2;
				}
				if (vari == 2 && c == 0) {
					vari = 3;
				}
			}

			@Override
			public boolean hasNext() {
				return vari < 3;
			}

			@Override
			public Nim3 next() {
				Nim3 n = null;
				switch (vari) {
					case 0:
						n = new Nim3(co, b, c);
						break;
					case 1:
						n = new Nim3(a, co, c);
						break;
					case 2:
						n = new Nim3(a, b, co);
						break;
				}
				co++;
				if (vari == 0 && co >= a) {
					co = 0;
					vari++;
				}
				if (vari == 1 && co >= b) {
					co = 0;
					vari++;
				}
				if (vari == 2 && co >= c) {
					co = 0;
					vari++;
				}
				// no 'else if' consider (2,0,0), we need the fall through after
				return n;
			}
		}

		public final int a;
		public final int b;
		public final int c;

		public Nim3(int a, int b, int c) {
			this.a = a;
			this.b = b;
			this.c = c;
		}

		@Override
		public List<Nim3> nextPositions() {
			List<Nim3> ps = new ArrayList<Nim3>(a * b * c);
			for (int i = 0; i < a; i++) {
				ps.add(new Nim3(i, b, c));
			}
			for (int i = 0; i < b; i++) {
				ps.add(new Nim3(a, i, c));
			}
			for (int i = 0; i < c; i++) {
				ps.add(new Nim3(a, b, i));
			}
			return ps;
		}

		@Override
		public Iterator<Nim3> nextPositionsIter() {
			return new Nim3Iterator(a, b, c);
		}

		// the following are of course generated automatically
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (o == null || getClass() != o.getClass()) return false;

			Nim3 nim3 = (Nim3) o;

			if (a != nim3.a) return false;
			if (b != nim3.b) return false;
			if (c != nim3.c) return false;

			return true;
		}

		@Override
		public String toString() {
			final StringBuilder sb = new StringBuilder("Nim3{");
			sb.append("a=").append(a);
			sb.append(", b=").append(b);
			sb.append(", c=").append(c);
			sb.append('}');
			return sb.toString();
		}

		@Override
		public int hashCode() {
			int result = a;
			result = 31 * result + b;
			result = 31 * result + c;
			return result;
		}
	}

	public static void runExample() {
		Solver<Nim3> solver = new Solver<>();
		solver.addPosition(new Nim3(0, 0, 0), PosType.P);

		// empirically, rules are slower or equal (on large size) than just brute forcing so they have absolutely no use
		// maybe I should just remove it
		// the cost of calling rules for every unknown position are much higher than the costs it save when it succeeds
		/*solver.addRule(x -> {
			int a = x.a; int b = x.b; int c = x.c;
			if (b==0) {
				b = a; a = 0;
			}
			else if (c == 0) {
				c = a; a = 0;
			}
			if (a != 0) return PosType.UNKNOWN;
			if (b != c) return PosType.N;
			else return PosType.P;
		});//*/

		final long START = System.currentTimeMillis();
		//System.out.println(solver.solveType(new Nim3(32, 64, 96)));      // slow
		System.out.println(solver.solveTypeIter(new Nim3(32, 64, 96)));  // fastest
		//System.out.println(solver.solveType2(new Nim3(32, 64, 96)));     // slowest
		//System.out.println(solver.solveTypeIter2(new Nim3(32, 64, 96))); // slight slower than second fastest
		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}
