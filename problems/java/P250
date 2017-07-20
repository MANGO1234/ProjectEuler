import mango123.math.EulerMath;

import java.util.Arrays;

public class EulerMain {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        long[] t = new long[250];
        t[0] = 1;
        for (int i = 1; i <= 250250; i++) {
            int m = EulerMath.powMod(i, i, 250);
            long[] newt = Arrays.copyOf(t, t.length);
            for (int j = 0; j < 250; j++) {
                int idx = (j + m) % 250;
                newt[idx] = (newt[idx] + t[j]) % 10000000000000000l;
            }
            t = newt;
        }
        System.out.println(t[0] - 1);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}