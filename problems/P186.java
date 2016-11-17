package problems;

import mango123.util.DisjointSet;

public class P186 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();
        int[] n = new int[5000000 + 1];
        for (int i = 0; i < n.length; i++) {
            long x = i;
            if (i < 56) {
                long k = 100003;
                k = (k - 200003 * x) % 1000000;
                k = k < 0 ? k + 1000000 : k;
                long t = (x * x) % 1000000;
                t = (t * x) % 1000000;
                k = (k + 300007 * t) % 1000000;
                n[i] = (int) k;
            } else {
                n[i] = (n[i - 24] + n[i - 55]) % 1000000;
            }
        }

        DisjointSet<Integer> s = new DisjointSet<>();
        for (int i = 0; i < 1000000; i++) {
            s.add(i);
        }
        int t = 0;
        for (int i = 1; i <= n.length / 2; i++) {
            s.union(n[i * 2 - 1], n[i * 2]);
            if (n[i * 2 - 1] != n[i * 2]) {
                t++;
            }
            if (s.find(524287).getSize() >= 990000) {
                System.out.println(t);
                break;
            }
        }
        System.out.println("no");
        System.out.println(s.find(524287).getSize());

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}

