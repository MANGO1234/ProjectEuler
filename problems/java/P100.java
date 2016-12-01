package problems.java;

import mango123.math.BigRational;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class P100 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // just implemented sqrt's continued fraction's as described on wikipedia
        // took from P66
        // if you do manipulation you get a(a-1)/b(b-1)=1/2 to (2b-1)^2-2(2a-1)^2=-1 and just
        // solve for Pell's
        BigInteger TEST = BigInteger.valueOf(-1);
        BigInteger TWO = BigInteger.valueOf(2);
        double sqrt = Math.sqrt(2);
        if (Math.floor(sqrt) != sqrt) {
            BigInteger m0 = BigInteger.ZERO;
            BigInteger d0 = BigInteger.ONE;
            BigInteger a0 = BigInteger.valueOf((int) Math.floor(sqrt));
            BigInteger S = TWO;
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
                if (r.getNumerator().pow(2).subtract(r.getDenominator().pow(2).multiply(S)).equals(TEST)) {
                    BigInteger temp = r.getNumerator().add(BigInteger.ONE);
                    if (temp.divide(TWO).compareTo(BigInteger.valueOf(1000000000000l)) > 0) {
                        System.out.println(r.getDenominator().add(BigInteger.ONE).divide(TWO));
                        break;
                    }
                }
                as.add(BigRational.valueOf(a1));
                a0 = a1;
                m0 = m1;
                d0 = d1;
            }
        }

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
