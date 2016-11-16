public class EulerMain {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

    private static final long[] LONG_POW_10 = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000,
            10000000000l, 100000000000l, 1000000000000l, 10000000000000l,
            100000000000000l, 1000000000000000l, 10000000000000000l,
            100000000000000000l, 1000000000000000000l };
}