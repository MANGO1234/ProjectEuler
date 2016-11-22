package problems.java;

public class P323 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        double[] ps = new double[60];
        for (int i = 0; i < ps.length; i++) {
            ps[i] = Math.pow(1 - Math.pow(0.5, i), 32);
        }
        for (int i = ps.length - 1; i > 0; i--) {
            ps[i] = ps[i] - ps[i - 1];
        }
        double k = 0;
        for (int i = 0; i < ps.length; i++) {
            k += i * ps[i];
        }
        System.out.println(k);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
