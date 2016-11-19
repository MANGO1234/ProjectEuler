package problems.java;

public class P164 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // forum: a dp algorithm is better, idk why this immediately come to my mind instead of
        // a dp algorithm
        boolean[][] v = new boolean[100][100];
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                v[i][j] = valid(i, j);
            }
        }

        long[][] t4 = new long[100][100];
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                if (v[i][j]) {
                    t4[i][j] = 1;
                }
            }
        }
        long[][] t8 = new long[100][100];
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                for (int k = 0; k < 100; k++) {
                    for (int l = 0; l < 100; l++) {
                        if (v[j][k]) {
                            t8[i][l] += t4[i][j] * t4[k][l];
                        }
                    }
                }
            }
        }
        long[][] t16 = new long[100][100];
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                for (int k = 0; k < 100; k++) {
                    for (int l = 0; l < 100; l++) {
                        if (v[j][k]) {
                            t16[i][l] += t8[i][j] * t8[k][l];
                        }
                    }
                }
            }
        }
        long[][] t20 = new long[100][100];
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                for (int k = 0; k < 100; k++) {
                    for (int l = 0; l < 100; l++) {
                        if (v[j][k]) {
                            t20[i][l] += t16[i][j] * t4[k][l];
                        }
                    }
                }
            }
        }
        long k = 0;
        for (int i = 10; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                k += t20[i][j];
            }
        }
        System.out.println(k);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

    public static boolean valid(int m1, int m2) {
        // swapped / and % and debug foe half an hour yay
        int a = m1 / 10;
        int b = m1 % 10;
        int c = m2 / 10;
        int d = m2 % 10;
        return a + b + c <= 9 && b + c + d <= 9;
    }

}
