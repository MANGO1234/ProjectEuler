import mango123.math.EulerMath;

public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		System.out.println(calc(0, 0, 0, 0));

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}

	private static long calc(int a, int b, int c, int n) {
		if (a+b+c>10) return 0;
		if (a+2*b+3*c>18) return 0;
		if (a+2*b+3*c==18) {
			long t = EulerMath.factorial(18);
			t/=EulerMath.pow(2,b);
			t /= EulerMath.pow(6,c);
			t/=EulerMath.factorial(a);
			t/=EulerMath.factorial(b);
			t/=EulerMath.factorial(c);
			t*=9;
			t*=EulerMath.factorial(9)/EulerMath.factorial(10-a-b-c);
			return t;
		}
		if (n == 0) return calc(a+1,b,c,0) + calc(a,b+1,c,1) + calc(a,b,c+1,2);
		if (n == 1) return calc(a,b+1,c,1) + calc(a,b,c+1,2);
		else        return calc(a, b, c + 1, 2);
	}
}