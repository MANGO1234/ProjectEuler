import java.util.*;


public class Euler {
	public static int MAX_PERIMETER = 1000000000;

	public static void main(String[] args) {
		long START = System.currentTimeMillis();
		long[][] n = new long[5][51];
		n[2][0]=1;
		n[2][1]=1;
		for (int i = 2; i < 51; i++) {
			n[2][i] = n[2][i-1] + n[2][i-2];
		}

		n[3][0]=1;
		n[3][1]=1;
		n[3][2]=1;
		for (int i = 3; i < 51; i++) {
			n[3][i] = n[3][i-1] + n[3][i-3];
		}

		n[4][0]=1;
		n[4][1]=1;
		n[4][2]=1;
		n[4][3]=1;
		for (int i = 4; i < 51; i++) {
			n[4][i] = n[4][i-1] + n[4][i-4];
		}

		final int x = 50;
		long sum = n[2][x] + n[3][x] + n[4][x];
		for (int i = 2; i < 5; i++) {
			for (int j = 0; j < 51; j++) {
				System.out.print(j+":"+n[i][j]+',');
			}
			System.out.println(n[i][x]);
		}
		System.out.println(sum-3); // -3 takes out the 3 empty tiling
		long END = System.currentTimeMillis();
		System.out.println(END - START);
	}
}
