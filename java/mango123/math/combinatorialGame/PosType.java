package mango123.math.combinatorialGame;

public enum PosType {
	// P means Previous player wins; in other words, second player wins
	// N means Next player wins; in other words, first player wins
	// UNKNOWN is used internally in Solver
	P, N, UNKNOWN;
}