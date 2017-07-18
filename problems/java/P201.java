public class EulerMain {
    static final int CARD = 50;
    static final int N = 101;
    static final int MAX = 295426;

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        int t[][] = new int[N][MAX];
        t[0][0] = 1;
        for (int i = 0; i < CARD; i++) {
            int newt[][] = new int[N][MAX];
            for (int j = 0; j < N; j++) {
                for (int k = 0; k < MAX; k++) {
                    int m = t[j][k];
                    if (m != 0) {
                        for (int l = j + 1; l < 101; l++) {
                            newt[l][k + l * l] += m;
                        }
                    }
                }
            }
            t = newt;
            System.out.println(i);
        }

        long sum = 0;
        for (int k = 0; k < MAX; k++) {
            int count = 0;
            for (int j = 0; j < N; j++) {
                count += t[j][k];
            }
            if (count == 1) {
                sum += k;
            }
        }
        System.out.println(sum);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}