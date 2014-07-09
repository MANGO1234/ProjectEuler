package mango123.util;

import java.util.*;

/**
 * A basic implementation of a disjoint set in Java. Optimizations involve using rank and path compression.
 * T should have its .equals() and .hashCode() override if not comparing by identity.
 * There should be no null in the set.
 *
 * @author De Li
 */
public class DisjointSet<T> {
	// Node of disjoint set
	public static class Node<T> {
		private Node<T> parent;
		private T value;
		private int rank;

		private Node(T value) {
			this.value = value;
			this.parent = this;
			rank = 0;
		}

		public T getValue() {
			return value;
		}

		@Override
		public String toString() {
			return "Node{" +
					"value=" + value +
					'}';
		}
	}

	// sets and total number of elements
	private Set<Node<T>> sets = new HashSet<>();
	private Map<T, Node<T>> map = new HashMap<>();

	/**
	 * Create an empty disjoint-set.
	 */
	public DisjointSet() {}

	/**
	 * Create a disjoint-set containing singleton sets for each element in the given arguments.
	 */
	public DisjointSet(T... elements) {
		for (int i = 0; i < elements.length; i++) {
			addInternal(elements[i]);
		}
	}

	/**
	 * Create a disjoint-set structure containing singleton sets for each element in the collection.
	 */
	public DisjointSet(Collection<T> elements) {
		for (T e : elements) {
			addInternal(e);
		}
	}

	/**
	 * Merge the two sets x and y belongs to (if they are both in the disjoint-set)
	 * @param x
	 * @param y
	 */
	public void union(T x, T y) {
		// get the set for the two given elements return if either is null
		Node<T> px = find(x);
		if (px == null) return;
		Node<T> py = find(y);
		if (py == null) return;
		if (px == py) return;

		// merge, point every element in set y to sex
		if (px.rank < py.rank) {
			px.parent = py;
			sets.remove(px);
		}
		else if (py.rank < px.rank) {
			py.parent = px;
			sets.remove(py);
		}
		else {
			py.parent = px;
			sets.remove(py);
			px.rank++;
		}
	}

	/**
	 * Find the set x belongs to, or null if x is not in the disjoint-set.
	 * @param x
	 * @return the set x belongs to (a Node)
	 */
	public Node<T> find(T x) {
		Node<T> n = map.get(x);
		if (n == null) return n;
		return findInternal(n);
	}

	/**
	 * Get a list containing all the elements in the disjoint-set .
	 * @return a list of all elements
	 */
	public List<T> getElements() {
		return new ArrayList<T>(map.keySet());
	}

	/**
	 * Fill a collection with all the elements in the disjoint-set.
	 * @return a set of all elements
	 */
	public void fillCollection(Collection<? super T> collection) {
		collection.addAll(map.keySet());
	}

	/**
	 * Get a list containing all the sets in the disjoint-set structure. Each set contain the elements, unboxed.
	 * @return a list of all sets
	 */
	public List<Set<T>> getSets() {
		List<Set<T>> list = new ArrayList<>();
		HashMap<Node<T>, Set<T>> setsMap = new HashMap<>();
		for (Node<T> set : sets) {
			Set<T> setr = new HashSet<>();
			setsMap.put(set, setr);
			list.add(setr);
		}
		for (Node<T> element : map.values()) {
			setsMap.get(element.parent).add(element.value);
		}
		return list;
	}

	//*****************************************************
	// Utility methods
	//*****************************************************

	/**
	 * Return number of elements in the disjoint-set.
	 * @return number of elements
	 */
	public int size() {
		return map.size();
	}

	/**
	 * Return the number of sets in disjoint-set.
	 * @return the number of sets
	 */
	public int numOfSets() {
		return sets.size();
	}

	/**
	 * Determine if two elements are in the same set. Returns false if x or y is not in the disjoint-set.
	 * @param x
	 * @param y
	 * @return if two elements are in the same set
	 */
	public boolean inSameSet(T x, T y) {
		Node<T> n = find(x);
		if (n == null) return false;
		Node<T> n2 = find(y);
		return n2 == null || n2.parent == n.parent;
	}

	/**
	 * For each element in the argument, add a singleton set containing it to the disjoint-set structure
	 * @param elements elements to be added
	 */
	public void add(T... elements) {
		for (int i = 0; i < elements.length; i++) {
			addInternal(elements[i]);
		}
	}

	/**
	 * For each element in the collection, add a singleton set containing it to the disjoint-set structure
	 * @param elements elements to be added
	 */
	public void addAll(Collection<T> elements) {
		for (T e : elements) {
			addInternal(e);
		}
	}

	public boolean contains(T element) {
		return map.containsKey(element);
	}

	@Override
	public String toString() {
		return getSets().toString();
	}

	//*****************************************************
	// Private methods
	//*****************************************************
	// add an element in a singleton set to the disjoint-set structure
	private void addInternal(T element) {
		if (element != null && !map.containsKey(element)) {
			Node<T> node = new Node<>(element);
			sets.add(node);
			map.put(element, node);
		}
	}

	private Node<T> findInternal(Node<T> n) {
		if (n.parent != n) {
			n.parent = findInternal(n.parent);
		}
		return n.parent;
	}
}