public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		final int SIZE = 1000000;
		final int UPTO = ((int) Math.sqrt(SIZE)) / 2;
		long s=0;
		for (int i = 1; i <= UPTO; i++) {
			int m = (SIZE + 4 * i * i) / 4 / i;
			s += m - 2 * i;
		}
		System.out.println(s);

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}