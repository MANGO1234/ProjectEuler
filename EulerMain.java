public class EulerMain {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        final int LIMIT = 100000;
        int[] t = new int[LIMIT + 1];
        for (int a = 1; a < t.length; a++) {
            for (int b = 1; b < Math.min(LIMIT / a, a) + 1; b++) {
                for (int c = 1; c < Math.min((LIMIT - 2 * a * b) / (a + b), b) + 1; c++) {
                    int cur = 2 * (a * b + b * c + a * c);
                    int i = 0;
                    while (cur <= LIMIT) {
                        t[cur] += 1;
                        cur = cur + (a + b + c) * 4 + i;
                        i += 8;
                    }
                }
            }
        }

        for (int i = 0; i < t.length; i++) {
            if (t[i] == 1000) {
                System.out.println(i);
                break;
            }
        }

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}