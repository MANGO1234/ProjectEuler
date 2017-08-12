public class EulerMain {

    static final int N = 100;
    static double[][] f1 = new double[N][N];
    static double[] pow_2_f = new double[9];
    static int[] pow_2_i = new int[9];

    public static void main(String[] args) {
        long START = System.currentTimeMillis();

        for (int i = 0; i < pow_2_f.length; i++) {
            pow_2_f[i] = Math.pow(2, i);
            pow_2_i[i] = (int) Math.pow(2, i);
        }

        for (int i = 0; i < f1.length; i++) {
            for (int j = 0; j < f1[i].length; j++) {
                f1[i][j] = -1;
            }
        }
        for (int i = N - 1; i >= 0; i--) {
            for (int j = N - 1; j >= 0; j--) {
                f(i, j);
            }
        }

        System.out.println(f1[0][0] / 2 + f1[1][0] / 2);
        System.out.println(f1[N - 1][N - 3]);
        System.out.println(f1[N - 1][N - 2]);
        System.out.println(f1[N - 1][N - 1]);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

    private static double f(int p1, int p2) {
        if (p2 >= N) {
            // p2 needs to checked first!
            return 1;
        }
        if (p1 >= N) {
            return 0;
        }
        if (f1[p1][p2] >= 0) {
            return f1[p1][p2];
        }
        double p = 0;
        for (int i = 1; i < pow_2_i.length; i++) {
            double sum = f(p1, p2 + pow_2_i[i - 1]) + f(p1 + 1, p2 + pow_2_i[i - 1]) + (pow_2_f[i] - 1) * f(p1 + 1, p2);
            p = Math.max(p, sum / (pow_2_f[i] + 1));
            if (p2 + pow_2_i[i - 1] >= N) {
                break;
            }
        }
        f1[p1][p2] = p;
        return p;
    }
}