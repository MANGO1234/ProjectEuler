public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		// quick dynamic programming, thought inclusion/exclusion would be a little complicated
		// but it's actually not ==
		long[][] ways = new long[17][8];
		ways[0][0] = 1;
		for (int i = 1; i < ways.length; i++) {
			ways[i][0]=ways[i-1][0]*13;
			ways[i][1]=ways[i-1][0]+ways[i-1][1]*14;
			ways[i][2]=ways[i-1][0]+ways[i-1][2]*14;
			ways[i][3]=ways[i-1][0]+ways[i-1][3]*14;
			ways[i][4]=ways[i-1][1]+ways[i-1][2]+ways[i-1][4]*15;
			ways[i][5]=ways[i-1][1]+ways[i-1][3]+ways[i-1][5]*15;
			ways[i][6]=ways[i-1][2]+ways[i-1][3]+ways[i-1][6]*15;
			ways[i][7]=ways[i-1][4]+ways[i-1][5]+ways[i-1][6]+ways[i-1][7]*16;//*/
		}
		long[][] invalid= new long[16][4];
		invalid[0][0] = 1;
		for (int i = 1; i < invalid.length; i++) {
			invalid[i][0]=invalid[i-1][0]*13;
			invalid[i][1]=invalid[i-1][0]+invalid[i-1][1]*14;
			invalid[i][2]=invalid[i-1][0]+invalid[i-1][2]*14;
			invalid[i][3]=invalid[i-1][1]+invalid[i-1][2]+invalid[i-1][3]*15;
		}
		long ans = ways[16][7];
		for (int i = 0; i < invalid.length; i++) {
			ans -= invalid[i][3];
		}
		System.out.println(Long.toHexString(ans).toUpperCase());

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}