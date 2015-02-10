public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		// newton interpolation
		long[][] fx = new long[11][11];
		for (int i = 0; i < fx.length; i++) {
			long x=i+1;
			fx[0][i]=1-x*(1-x*(1-x*(1-x*(1-x*(1-x*(1-x*(1-x*(1-x*(1-x)))))))));
		}
		for (int i = 1; i < fx.length; i++) {
			for (int j = i; j < fx.length; j++) {
				fx[i][j]=(fx[i-1][j]-fx[i-1][j-1])/i;
			}
		}
		long sum=fx[0][0];
		for (int i = 1; i < fx.length-1; i++) {
			long xx=i+2;
			long y=fx[i-1][i-1]+fx[i][i]*(xx-i);
			for (int j = i-1; j > 0; j--) {
				y=fx[j-1][j-1]+y*(xx-j);
			}
			sum+=y;
		}
		System.out.println(sum);
		
		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START) + "ms");
	}
}