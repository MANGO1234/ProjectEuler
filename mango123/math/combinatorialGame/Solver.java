package mango123.math.combinatorialGame;

import java.util.*;
import java.util.function.Function;
import static mango123.math.combinatorialGame.PosType.*;

public class Solver<T extends Position<T>> {
	Map<T, PosType> cache;
	List<Function<T, PosType>> rules;

	public Solver() {
		cache = new HashMap<>();
		rules = new ArrayList<>();
	}

	public void addRule(Function<T, PosType> rule) {
		rules.add(rule);
	}

	public void addPosition(T position, PosType type) {
		cache.put(position, type);
	}

	//**********************************************************
	// SOLVE
	//**********************************************************
	// from nim game, solveTypeIter (slight faster)~ solveTypeIter2 > solveType > solveType2

	// however, from Position:
	// "in addition, try to make it so nextPositions rreturn positions closer to the base case first
	// nim(16,32,48) should generate positions nim(0,32,48), nim(1, 32, 48)... for instance"

	// if nim(16, 32, 48) generate positions backwards nim(15,32,48), nim(14,32,48)
	// solveType2 will actually run faster than solveType
	// anyway, just make is so nextPositions generate positions closer to the base case first

	public PosType solveType(T position) {
		PosType type = quickID(position);
		if (type != UNKNOWN) return type;
		return solveTypeH(position);
	}

	public PosType solveTypeIter(T position) {
		PosType type = quickID(position);
		if (type != UNKNOWN) return type;
		return solveTypeIterH(position);
	}


	public PosType solveType2(T position) {
		PosType type = quickID(position);
		if (type != UNKNOWN) return type;
		return solveType2H(position);
	}

	public PosType solveTypeIter2(T position) {
		PosType type = quickID(position);
		if (type != UNKNOWN) return type;
		return solveTypeIter2H(position);
	}

	private PosType solveTypeH(T position) {
		List<T> ps = position.nextPositions();
		int len = ps.size();
		List<T> nps = new ArrayList<>(len / 2);
		for (int i = 0; i < len; i++) {
			PosType type = quickID(ps.get(i));
			if (type == P) {
				cache.put(position, N);
				return N;
			}
			else if (type == UNKNOWN) {
				nps.add(ps.get(i));
			}
		}

		len = nps.size();
		for (int i = 0; i < len; i++) {
			PosType type = solveTypeH(nps.get(i));
			if (type == P) {
				cache.put(position, N);
				return N;
			}
		}

		// then every position is N, so this must be P
		cache.put(position, P);
		return P;
	}

	private PosType solveTypeIterH(T position) {
		Iterator<T> ps = position.nextPositionsIter();
		List<T> nps = new ArrayList<>();
		while (ps.hasNext()) {
			T np = ps.next();
			PosType type = quickID(np);
			if (type == P) {
				cache.put(position, N);
				return N;
			}
			else if (type == UNKNOWN) {
				nps.add(np);
			}
		}

		int len = nps.size();
		for (int i = 0; i < len; i++) {
			PosType type = solveTypeIterH(nps.get(i));
			if (type == P) {
				cache.put(position, N);
				return N;
			}
		}

		// then every position is N, so this must be P
		cache.put(position, P);
		return P;
	}

	public PosType solveType2H(T position) {
		List<T> ps = position.nextPositions();
		int length = ps.size();
		for (int i = 0; i < length; i++) {
			PosType type = quickID(ps.get(i));
			if (type == P) {
				cache.put(position, N);
				return N;
			}
			if (type == N) continue;
			type = solveType2H(ps.get(i));
			if (type == P) {
				cache.put(position, N);
				return N;
			}
		}

		// then every position is N, so this must be P
		cache.put(position, P);
		return P;
	}

	public PosType solveTypeIter2H(T position) {
		Iterator<T> ps = position.nextPositionsIter();
		while (ps.hasNext()) {
			T np = ps.next();
			PosType type = quickID(np);
			if (type == P) {
				cache.put(position, N);
				return N;
			}
			if (type == N) continue;
			type = solveTypeIter2H(np);
			if (type == P) {
				cache.put(position, N);
				return N;
			}
		}

		// then every position is N, so this must be P
		cache.put(position, P);
		return P;
	}

	private PosType quickID(T position) {
		PosType type = cache.get(position);
		if (type == null) {
			for (Function<T, PosType> rule : rules) {
				type = rule.apply(position);
				if (type != UNKNOWN) {
					cache.put(position, type);
					return type;
				}
			}
			type = PosType.UNKNOWN;
		}
		return type;
	}
}