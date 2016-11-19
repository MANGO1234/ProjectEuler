import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;
import mango123.math.LongRational;

import java.io.*;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.*;
import java.util.regex.*;

public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();
		int[][] n = new int[6][];
		n[0] = new int[141 - 45];
		for (int i = 0; i < n[0].length; i++) {
			int x = i + 45;
			n[0][i] = x * (x + 1) / 2;
		}
		n[1] = new int[100 - 32];
		for (int i = 0; i < n[1].length; i++) {
			int x = i + 32;
			n[1][i] = x * x;
		}
		n[2] = new int[82 - 26];
		for (int i = 0; i < n[2].length; i++) {
			int x = i + 26;
			n[2][i] = x * (3 * x - 1) / 2;
		}
		n[3] = new int[71 - 23];
		for (int i = 0; i < n[3].length; i++) {
			int x = i + 23;
			n[3][i] = x * (2 * x - 1);
		}
		n[4] = new int[64 - 21];
		for (int i = 0; i < n[4].length; i++) {
			int x = i + 21;
			n[4][i] = x * (5 * x - 3) / 2;
		}
		n[5] = new int[59 - 19];
		for (int i = 0; i < n[5].length; i++) {
			int x = i + 19;
			n[5][i] = x * (3 * x - 2);
		}

		boolean[] checked = new boolean[6];
		checked[5] = true;
		for (int i = 0; i < n[5].length; i++) {
			int sol = find(checked, n[5][i], n[5][i], n[5][i], 4, n);
			if (sol != 0) {
				System.out.println(n[5][i] + ":" + sol);
				break;
			}
		}

		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}

	public static int find(boolean[] checked, int sum, int num, int finalNum, int count, int[][] n) {
		if (count == 0) {
			int needToExist = num / 100 + (finalNum % 100) * 100;
			for (int i = 0; i < checked.length; i++) {
				if (!checked[i] && Arrays.binarySearch(n[i], needToExist) >= 0) {
					System.out.println(needToExist + ":" + (sum + needToExist));
					return sum + needToExist;
				}
			}
		}
		else {
			num /= 100;
			for (int i = checked.length - 1; i >= 0; i--) {
				if (checked[i]) continue;
				checked[i] = true;
				for (int j = 0; j < n[i].length; j++) {
					if (n[i][j] % 100 == num) {
						int sol = find(checked, sum + n[i][j], n[i][j], finalNum, count - 1, n);
						if (sol != 0) {
							System.out.println(n[i][j] + ":" + sol);
							return sol;
						}
					}
				}
				checked[i] = false;
			}
		}
		return 0;
	}
}
