import java.util.Arrays;

public class EulerMain {
    public static void f(long[] t, long m, int i, int start) {
        int count = 1;
        boolean done = false;
        for (int j = start; j < t.length; j += start) {
            if (count == i) {
                count = 1;
                if (!done) {
                    done = true;
                    f(t, m * i * i + 1, i, j);
                }
            } else {
                t[j] *= m;
                count++;
            }
        }
    }

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        long[] t = new long[64000000];
        Arrays.fill(t, 1);
        for (int i = 2; i < t.length; i++) {
            if (t[i] == 1) {
                f(t, ((long) i) * i + 1, i, i);
            }
        }
        long sum = 0;
        for (int i = 0; i < t.length; i++) {
            long sqrt = (long) Math.sqrt(t[i]);
            if (sqrt * sqrt == t[i]) {
                sum += i;
            }
        }
        System.out.println(sum);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}