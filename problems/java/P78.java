package problems.java;

public class P78 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();
        int[] pen = new int[1000];
        for (int i = 0; i < pen.length / 2; i++) {
            int j = i + 1;
            pen[i * 2] = j * (3 * j - 1) / 2;
            j = -1 * j;
            pen[i * 2 + 1] = j * (3 * j - 1) / 2;
        }

        int[] p = new int[100000];
        p[0] = 1;
        for (int i = 1; i < p.length; i++) {
            int k = 0;
            int t = 0;
            for (int j = 0; i >= pen[j]; j++) {
                if (t < 2) {
                    k = (k + p[i - pen[j]]) % 1000000;
                } else {
                    k = (k - p[i - pen[j]]) % 1000000;
                }
                t++;
                if (t == 4) {
                    t = 0;
                }
            }
            p[i] = k;
            if (k == 0) {
                System.out.println(i);
                break;
            }
        }

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
