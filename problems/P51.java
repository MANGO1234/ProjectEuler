import mango123.miscellaneous.Digit;
import mango123.util.ArraysUtils;

import java.io.*;
import java.util.*;
import java.util.stream.*;

public class Euler {
	public static void main(String[] args) {
		final long START = System.currentTimeMillis();

		try {
			BufferedReader r = new BufferedReader(new FileReader("words.txt"));
			Map<String, List<String>> tempMap = Arrays.asList(r.readLine().split(",")).stream()
					.map(st -> st.substring(1, st.length() - 1))
					.collect(Collectors.groupingBy(i -> {
						char[] arr = i.toCharArray();
						Arrays.sort(arr);
						return new String(arr);
					}));
			Map<Integer, List<List<String>>> wordsMap = tempMap.values().stream()
					.filter(i -> i.size() > 1)
					.collect(Collectors.groupingBy(i -> i.get(0).length()));

			int sq = 1;
			List<Integer> squares = new ArrayList<>();
			for (int i = 3; sq < 1_000_000_000; i += 2) {
				sq += i;
				squares.add(sq);
			}
			Map<String, List<int[]>> tempMap2 = squares.stream()
					.map(Digit::digitsOf)
					.collect(Collectors.groupingBy(i -> {
						int[] digits = Arrays.copyOf(i, i.length);
						Arrays.sort(digits);
						return toKey(digits);
					}));
			Map<Integer, List<List<int[]>>> squaresMap = tempMap2.values().stream()
					.filter(i -> i.size() > 1)
					.collect(Collectors.groupingBy(i -> i.get(0).length));

			int max = 0;
			for (int i = 9; i > 3; i--) {
				if (i==7) continue;
				System.out.println(i + ":" + wordsMap.get(i));
				for (List<String> wordsAna : wordsMap.get(i)) {
					for (List<int[]> squareAna : squaresMap.get(i)) {
						String word = wordsAna.get(0);
						for (int l = 0; l < squareAna.size(); l++) {
							int[] square = squareAna.get(l);
							char[] map = new char[10];
							boolean success = true;
							for (int j = 0; j < square.length; j++) {
								if (map[square[j]] != 0 && word.charAt(j) != map[square[j]]) {
									success = false;
									break;
								}
								map[square[j]] = word.charAt(j);
							}
							if (!success) break;
							for (int j = l; j < squareAna.size(); j++) {
								StringBuilder wordB = new StringBuilder();
								int[] num = squareAna.get(j);
								for (int k = 0; k < num.length; k++) {
									wordB.append(map[num[k]]);
								}
								String wordTest = wordB.toString();
								if (!wordTest.equals(word) && wordsAna.contains(wordTest)) {
									System.out.println(Arrays.toString(squareAna.get(j)) +"." + Arrays.toString(squareAna.get(l))+"."+wordB);
								}
							}
						}
					}
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
		}

		long END = System.currentTimeMillis();
		System.out.println("Time: "+ (END - START) +"ms");
	}

	private static String toKey(int[] digits) {
		StringBuilder key = new StringBuilder();
		for (int i = 0; i < digits.length; i++) {
			key.append(digits[i]);
		}
		return key.toString();
	}
}
