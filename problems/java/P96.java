import mango123.util.DisjointSet;
import mango123.util.Pair;

import java.io.BufferedReader;
import java.io.FileReader;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();
		try {
			BufferedReader r = new BufferedReader(new FileReader("sudoku.txt"));
			String s = r.readLine();
			int sum = 0;
			while (s != null) {
				int[][] b = new int[9][9];
				for (int i = 0; i < 9; i++) {
					s = r.readLine();
					for (int j = 0; j < 9; j++) {
						b[i][j]=s.charAt(j)-48;
					}
				}
				solve(b);
				sum += b[0][0]*100+b[0][1]*10+b[0][2];
				s = r.readLine();
			}
			System.out.println(sum);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		long END = System.currentTimeMillis();
		System.out.println("Time:"+ (END - START));
	}

	private static boolean solve(int[][] b) {
		Pair<Integer, Integer> c = findFirstEmpty(b);
		if (c == null) return true; // we filled everything
		Set<Integer> k = getPossible(b, c);
		for (Integer integer : k) {
			b[c.a][c.b] = integer;
			if (solve(b)) return true;
		}
		b[c.a][c.b] = 0;
		return false;
	}

	private static Set<Integer> getPossible(int[][] b, Pair<Integer, Integer> c) {
		Set<Integer> s = new HashSet<>();
		for (int i = 0; i < 9; i++) s.add(i+1);
		for (int i = 0; i < 9; i++) {
			s.remove(b[c.a][i]);
			s.remove(b[i][c.b]);
		}
		int x = c.a / 3 * 3;
		int y = c.b / 3 * 3;
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 3; j++) {
				s.remove(b[x + i][y + j]);
			}
		}
		return s;
	}

	private static Pair<Integer, Integer> findFirstEmpty(int[][] b) {
		for (int i = 0; i < 9; i++) {
			for (int j = 0; j < 9; j++) {
				if (b[i][j] == 0) return new Pair<>(i, j);
			}
		}
		return null;
	}
}
