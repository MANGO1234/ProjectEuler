package problems.java;

public class P188 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        long[] m = new long[1250000];
        m[0] = 1;
        for (int i = 1; i < m.length; i++) {
            m[i] = (m[i - 1] * 1777) % 100000000;
        }

        int i = 1;
        for (int j = 0; j < 1855; j++) {
            i = (int) m[i % m.length];
        }
        System.out.println(i);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
