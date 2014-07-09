package mango123.math;

public interface PrimeSearcher {
	//general
	int nthPrime(int n);
	boolean isPrime(int n);
	void expandSearchUpTo(int n);
	int numberOfPrimesFound();
	int size();
	int[] toArray();
	
	//iteration
	int nextPrime();
	int previousPrime();
	int indexOfCurrentPrime();
	int currentPrime();
	void resetIteration();
}
