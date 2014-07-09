import java.util.*;


public class Euler {
	public static int MAX_PERIMETER = 1000000000;

	public static void main(String[] args) {
		long START = System.currentTimeMillis();
		// t[m][n] is the number of different rectangles that can fit EXACTLY inside a grid of size m * n
		int[][] t = new int[48][48];
		for (int i = 1; i < 48; i++) {
			for (int j = 1; j < 48; j++) {
				t[i][j]++; // 1 for normal rectngles
			}
		}

		for (int i = 1; i < 94; i++) {
			for (int j = 1; j < 94; j++) { // wasteful can bound 94 tighter
				// formula from observation, for a crosshatchet rectangle of size i and j find the corresponding
				// m * n grid it fit exactly into (there will be 2 cases for each crosshatchet rectangle)
				// can exploit symmetry when i and j are not both even
				int m = i / 2 + j / 2 + 1;
				int n = (i + j) / 2 + 1;
				if (m < 48 && n < 48) t[m][n]++;

				m = (i + 1) / 2 + (j + 1) / 2;
				n = (i + j - 1) / 2 + 1;
				if (m < 48 && n < 48) t[m][n]++;
			}
		}

		// f[m][m] is the total number of rectangles that can fit inside m*n grid
		long[][] f = new long[48][48];
		f[1][1] = 1;
		for (int i = 1; i < 48; i++) {
			for (int j = 1; j < 48; j++) {
				if (f[i][j] != 0) continue; // skip entries we exploited symmetry, also simplify j=1
				long sum = 2 * f[i][j - 1] - f[i][j - 2];
				for (int k = 1; k <= i; k++) {
					sum += k * t[i + 1 - k][j];
				}
				f[i][j] = sum;
				f[j][i] = sum; // symmetry, make handling j = 1 easy
			}
		}

		long answer = 0;
		for (int i = 0; i < 44; i++) {
			for (int j = 0; j < 48; j++) {
				answer += f[i][j];
			}
		}
		System.out.println(answer);

		long END = System.currentTimeMillis();
		System.out.println(END - START);
	}
}
