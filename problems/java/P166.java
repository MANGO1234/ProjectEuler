import static java.lang.Math.max;
import static java.lang.Math.min;

public class EulerMain {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // ~1.4s, original solution in python ported to java, gets 40x speed up
        int[] len = new int[37];
        int[][][] t = new int[37][670][];

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                for (int k = 0; k < 10; k++) {
                    for (int l = 0; l < 10; l++) {
                        int sum = i + j + k + l;
                        t[sum][len[sum]++] = new int[] { i, j, k, l };
                    }
                }
            }
        }

        int total = 0;
        for (int i = 0; i < 18; i++) {
            total += f(i, t[i], len[i]) * 2;
        }
        total += f(18, t[18], len[18]);
        System.out.println(total);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

    private static int f(int n, int[][] xs, int len) {
        int tn = 0;
        for (int xi = 0; xi < len; xi++) {
            for (int yi = 0; yi < len; yi++) {
                int[] x = xs[xi];
                int[] y = xs[yi];
                int l1 = min(max(n - x[0] - y[0] - 9, 0), 9);
                int u1 = max(min(n - x[0] - y[0] + 1, 10), 0);
                for (int b31 = l1; b31 < u1; b31++) {
                    int b41 = n - x[0] - y[0] - b31;
                    int b32 = n - x[3] - y[2] - b41;
                    if (b32 < 0 || b32 > 9) {
                        continue;
                    }
                    int b42 = n - x[1] - y[1] - b32;
                    if (b42 < 0 || b42 > 9) {
                        continue;
                    }
                    int l2 = min(max(n - x[3] - y[3] - 9, 0), 9);
                    int u2 = max(min(n - x[3] - y[3] + 1, 10), 0);
                    for (int b34 = l2; b34 < u2; b34++) {
                        int b44 = n - x[3] - y[3] - b34;
                        int b33 = n - x[0] - y[1] - b44;
                        if (b33 < 0 || b33 > 9) {
                            continue;
                        }
                        if (b31 + b32 + b33 + b34 != n) {
                            continue;
                        }
                        int b43 = n - x[2] - y[2] - b33;
                        if (b43 < 0 || b43 > 9) {
                            continue;
                        }
                        if (b41 + b42 + b43 + b44 != n) {
                            continue;
                        }
                        tn += 1;
                    }
                }
            }
        }
        return tn;
    }
}