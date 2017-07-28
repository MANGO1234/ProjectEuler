public class EulerMain {
    public static class Tuple {
        public final long a;
        public final long b;

        public Tuple(long a, long b) {
            this.a = a;
            this.b = b;
        }

        @Override public String toString() {
            return "Tuple{" +
                    "a=" + a +
                    ", b=" + b +
                    '}';
        }
    }

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // using ArrayList and Tuple: 8s, using long array: 400ms

        int[] ps = new int[] { 5, 13, 17, 29, 37, 41, 53, 61, 73, 89, 97, 101, 109, 113, 137, 149 };

        long[] sols = new long[60000000];
        int sols_len = 0;
        for (int p : ps) {
            int a, b = 0;
            boolean keep_going = true;
            for (a = 0; a < 13; a++) {
                for (b = a; b < 13; b++) {
                    if (a * a + b * b == p) {
                        keep_going = false;
                        break;
                    }
                }
                if (!keep_going) {
                    break;
                }
            }

            int size_before = sols_len;
            for (int i = 0; i < size_before; i += 2) {
                long a2 = sols[i];
                long b2 = sols[i + 1];
                sols[sols_len] = Math.abs(a * b2 - b * a2);
                sols[sols_len + 1] = a * a2 + b * b2;
                sols_len += 2;
                sols[sols_len] = Math.abs(b * b2 - a * a2);
                sols[sols_len + 1] = b * a2 + a * b2;
                sols_len += 2;
            }
            sols[sols_len] = a;
            sols[sols_len + 1] = b;
            sols_len += 2;
            System.out.println(sols_len);
        }
        long sum = 0;
        for (int i = 0; i < sols_len; i += 2) {
            sum += Math.min(sols[i], sols[i + 1]);
        }
        System.out.println(sum);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}