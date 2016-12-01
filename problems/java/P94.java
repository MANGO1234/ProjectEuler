package problems.java;

import static mango123.math.EulerMath.isSquare;

public class P94 {
    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        // this is different than the forum
        //        Let's say the triangle is b,b+1,b+1.
        //        If you use Heron's formula on it you get $A=b\sqrt{(3b+2)(b+2)}/4$. The term inside the square root needs to be a square for the whole expression to be an integer. If you look at $(3b+2)(b+2) \mod 4$, since $n^2=0 (\mod 4)$ or $n^2=1 (\mod 4)$, b has to be even.
        //        Let b=2a, so the triangle is of the form 2a, 2a+1, 2a+1. A triangle has area $A=bh/2=2ah/2=ah$. Use 2a as the base and 2a+1 as the hypotenuse, draw the height h of the triangle. Note that a,h,2a+1 forms a right triangle, so $h=\sqrt{3a^2+4a+1}$. The term inside the square root needs to be a square so that $A=ah$ is an integer.
        //                Remember that a,h,2a+1 forms a right triangle, so the hypotenuse need to be $m^2+n^2$ for some m, n and the other two sides are 2mn and $m^2-n^2$ (learned this in past problems dealing with pythagoras triplet). The difference between the hypotenuse (length 2a+1) and one of the side (length a) is equal to $2a+1-a=a+1$, and it also needs to equal to either $m^2+n^2-(m^2-n^2)=2n^2$ or $m^2+n^2-2mn=(m-n)^2=c^2$ for some c. In other words, $a$ can only be of the form $2c^2-1$ (n was arbitrary, so just call it c) or $c^2-1$.
        //                The perimeter 2a+2a+1+2a+1=6a+2<1000000000 means a<(1000000000-2)/6, so we don't have to search through a lot of values for c.
        //        If you do this for triangles of b,b-1,b-1, it's pretty much the same thing: a can only be of the form $2c^2+1$ or $c^2+1$ and a<(1000000000+2)/6.
        //
        //        Then just look through a of the form $2c^2-1$, $c^2-1$, $2c^2+1$, $c^2+1$ and check whether $h=\sqrt{3a^2+4a+1}$ is square or not. This took less than 10ms on an i5-6200u.

        long sum = 0;
        final long A_MAX = (1000000000l - 2) / 6;
        long i = 2; // i=2 removes edge case when i = 1
        while (true) {
            long a = i * i - 1;
            if (a > A_MAX) {
                break;
            }
            if (isSquare(3 * a * a + 4 * a + 1)) {
                sum += 6 * a + 2;
            }
            i++;
        }
        System.out.println(sum);
        i = 1;
        while (true) {
            long a = 2 * i * i - 1;
            if (a > A_MAX) {
                break;
            }
            if (isSquare(3 * a * a + 4 * a + 1)) {
                sum += 6 * a + 2;
            }
            i++;
        }
        System.out.println(sum);

        final long B_MAX = (1000000000l + 2) / 6;
        i = 1;
        while (true) {
            long a = i * i + 1;
            if (a > B_MAX) {
                break;
            }
            if (isSquare(3 * a * a - 4 * a + 1)) {
                sum += 6 * a - 2;
            }
            i++;
        }
        System.out.println(sum);
        i = 1;
        while (true) {
            long a = 2 * i * i + 1;
            if (a > B_MAX) {
                break;
            }
            if (isSquare(3 * a * a - 4 * a + 1)) {
                sum += 6 * a - 2;
            }
            i++;
        }
        System.out.println(sum);

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }
}
