package problems.java;

import mango123.math.BigRational;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class P66 {

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // just implemented sqrt's continued fraction's as described on wikipedia
        BigInteger max = BigInteger.ZERO;
        int answer = 0;
        for (int i = 2; i <= 1000; i++) {
            double sqrt = Math.sqrt(i);
            if (Math.floor(sqrt) != sqrt) {
                BigInteger m0 = BigInteger.ZERO;
                BigInteger d0 = BigInteger.ONE;
                BigInteger a0 = BigInteger.valueOf((int) Math.floor(sqrt));
                BigInteger S = BigInteger.valueOf(i);
                BigInteger sqrtS = a0;
                List<BigRational> as = new ArrayList<>();
                as.add(BigRational.valueOf(a0));
                while (true) {
                    BigInteger m1 = d0.multiply(a0).subtract(m0);
                    BigInteger d1 = S.subtract(m1.multiply(m1)).divide(d0);
                    BigInteger a1 = sqrtS.add(m1).divide(d1);
                    BigRational r = BigRational.valueOf(a1);
                    for (int j = as.size() - 1; j >= 0; j--) {
                        r = BigRational.ONE.divide(r).add(as.get(j));
                    }
                    if (r.getNumerator().pow(2).subtract(r.getDenominator().pow(2).multiply(S)).equals(BigInteger.ONE)) {
                        if (max.compareTo(r.getNumerator()) < 0) {
                            max = r.getNumerator();
                            answer = i;
                        }
                        break;
                    }
                    as.add(BigRational.valueOf(a1));
                    a0 = a1;
                    m0 = m1;
                    d0 = d1;
                }
            }
        }
        System.out.println(max);
        System.out.println(answer);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

}
