public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		final int SIZE = 1000000;
		int[] l = new int[SIZE+1];
		for (int i = 1; i < 250000; i++) {
			int j = i + 2;
			int k = j * j - i * i;
			while (k <= 1000000) {
				l[k]++;
				j+=2;
				k = j * j - i * i;
			}
		}
		int s = 0;
		for (int i = 0; i < l.length; i++) {
			if (1<=l[i]&&l[i]<=10) s++;
		}
		System.out.println(s);

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}