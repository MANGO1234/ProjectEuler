public class EulerMain {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        //        final long N = 1000000000000000L;
        //        long sq = (long) Math.sqrt(N);
        //        long answer = 0;
        //        long upto = N / sq;
        //        for (long i = 1; i <= upto; i++) {
        //            answer = (answer + N / i * i % 1000000000 * i) % 1000000000;
        //        }
        //        BigInteger BIG_1E9 = BigInteger.valueOf(1000000000);
        //        BigInteger BIG_6 = BigInteger.valueOf(6);
        //        BigInteger ls = BigInteger.valueOf(upto).multiply(BigInteger.valueOf(upto + 1)).multiply(BigInteger.valueOf(2 * upto + 1));
        //        for (long i = sq - 1; i > 0; i--) {
        //            long upper = N / i;
        //            BigInteger us = BigInteger.valueOf(upper).multiply(BigInteger.valueOf(upper + 1)).multiply(BigInteger.valueOf(2 * upper + 1));
        //            BigInteger t = us.subtract(ls).divide(BIG_6).mod(BIG_1E9);
        //            answer = (answer + t.longValue() * i) % 1000000000;
        //            ls = us;
        //        }
        //        System.out.println(answer);

        final long N = 1000000000000000L;
        final long M = 1000000000;
        final long M2 = 2000000000;
        long sq = (long) Math.sqrt(N);
        long answer = 0;
        long upto = N / sq;
        for (long i = 1; i <= upto; i++) {
            answer = (answer + N / i * i % M * i) % M;
        }
        long ls = ((upto % M2) * ((upto + 1) % M2) % M2) * ((2 * upto + 1) % M2) / 2;
        for (long i = sq - 1; i > 0; i--) {
            long upper = N / i;
            // learned this in forum to skip BigInteger
            long us = ((upper % M2) * ((upper + 1) % M2) % M2) * ((2 * upper + 1) % M2) / 2;
            long t = (us % M - ls % M + M) * 666666667 % M;
            answer = (answer + t * i) % M;
            if (i == sq - 1) {
                System.out.println(answer);
            }
            ls = us;
        }
        System.out.println(answer);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}