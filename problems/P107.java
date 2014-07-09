import mango123.math.EratosthenesSieve;
import mango123.math.EulerMath;
import mango123.math.LongRational;

import java.io.*;
import java.math.BigInteger;
import java.util.*;
import java.util.stream.*;
import java.util.regex.*;

public class Euler {
	public static class Edge implements Comparable<Edge> {
		int start;
		int end;
		int weight;

		public Edge(int start, int end, int weight) {
			this.start = start;
			this.end = end;
			this.weight = weight;
		}

		@Override
		public String toString() {
			return "Edge{" +
					"start=" + start +
					", end=" + end +
					", weight=" + weight +
					'}';
		}

		@Override
		public int compareTo(Edge o) {
			return Integer.compare(weight, o.weight);
		}
	}

	public static void main(String[] args) {
		final long START = System.currentTimeMillis();
		try {
			BufferedReader r = new BufferedReader(new FileReader("network.txt"));
			PriorityQueue<Edge> q = new PriorityQueue<>();
			for (int i = 0; i < 40; i++) {
				String[] st = r.readLine().split(",");
				for (int j = 0; j < st.length; j++) {
					if (!st[j].equals("-")) q.add(new Edge(i, j, Integer.valueOf(st[j])));
				}
			}
			int totalCost = q.stream().mapToInt(i -> i.weight).sum() / 2;

			// can use disjoint set structure but require more coding ==
			ArrayList<Long> sets = new ArrayList<>();
			for (int i = 0; i < 40; i++) sets.add(1l << i);
			ArrayList<Edge> edges = new ArrayList<>();
			while (!q.isEmpty()) {
				Edge e = q.poll();
				long index = (1l << e.start) + (1l << e.end);
				int start=-1, end=-1;
				for (int i = 0; i < sets.size(); i++) {
					if ((sets.get(i) & index) == index) break;
					if ((sets.get(i) & index) != 0) {
						if (start == -1) start = i;
						else end = i;
					}
				}

				if (start != -1) {
					long newSet = sets.get(start) | sets.get(end);
					sets.remove(Math.max(start, end));
					sets.remove(Math.min(start, end));
					sets.add(newSet);
					edges.add(e);
				}
			}
			System.out.println(totalCost - edges.stream().mapToLong(i -> i.weight).sum());

		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
