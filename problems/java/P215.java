import java.util.*;

public class Euler {
	public static final int WIDTH = 32;
	public static final int HEIGHT = 10;

	public static class Node {
		int sum = 0;
		Node parent;
		int id;

		public Node add(int k) {
			if (sum + k >= WIDTH - 1) return null;
			Node n = new Node();
			n.sum = sum + k;
			n.parent = this;
			return n;
		}
	}

	public static void main(String[] args) {
		long START = System.currentTimeMillis();

		LinkedList<Node> s = new LinkedList<Node>();
		s.add(new Node());
		List<Node> c = new ArrayList<Node>();
		while (s.size() != 0) {
			Node p = s.removeFirst();
			if (p.sum == WIDTH - 2 || p.sum == WIDTH - 3) {
				c.add(p);
				continue;
			}
			Node temp = p.add(2);
			s.addFirst(temp);
			temp = p.add(3);
			if (temp != null) {
				s.addFirst(temp);
			}
		}
		final int C_SIZE = c.size();

		List<List<Node>> col = new ArrayList<List<Node>>(WIDTH);
		for (int i = 0; i < WIDTH; i++) {
			col.add(new ArrayList<Node>());
		}
		for (Node n : c) {
			Node temp = n;
			while (n.sum != 0) {
				col.get(n.sum).add(temp);
				n = n.parent;
			}
		}

		for (int i = 0; i < C_SIZE; i++) {
			c.get(i).id = i;
		}

		boolean[][] edges = new boolean[C_SIZE][C_SIZE];
		for (int i = 0; i < WIDTH; i++) {
			List<Node> collisions = col.get(i);
			int jl = collisions.size();
			for (int j = 0; j < jl; j++) {
				int nid = collisions.get(j).id;
				for (int k = 0; k < jl; k++) {
					edges[nid][collisions.get(k).id] = true;
				}
			}
		}

		long[][] path = new long[HEIGHT][C_SIZE];
		for (int i = 0; i < C_SIZE; i++) {
			long sum = 0;
			for (int j = 0; j < C_SIZE; j++) {
				if (edges[i][j] == false) {
					sum++;
				}
			}
			path[1][i] = sum;
		}

		for (int i = 2; i < HEIGHT; i++) {
			for (int j = 0; j < C_SIZE; j++) {
				long sum = 0;
				for (int k = 0; k < C_SIZE; k++) {
					if (edges[j][k] == false) {
						sum += path[i - 1][k];
					}
				}
				path[i][j] = sum;
			}
		}

		long sumo = 0;
		for (int i = 0; i < C_SIZE; i++) {
			sumo += path[HEIGHT - 1][i];
		}
		System.out.println(sumo);

		long END = System.currentTimeMillis();
		System.out.println("Time: " + (END - START));
	}
}
