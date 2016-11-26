package problems.java;

import mango123.math.EratosthenesSieve;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class P118 {
    static EratosthenesSieve sieve = new EratosthenesSieve(100000000);
    static Set<String> set = new HashSet<>();

    public static void main(String[] args) {
        final long START = System.currentTimeMillis();

        count(0, 0, new ArrayList<>());
        System.out.println(set.contains("2,5,47,89,631"));
        System.out.println(set.size());

        long END = System.currentTimeMillis();
        System.out.println("Time: " + (END - START) + "ms");
    }

    public static void count(int a, int n, List<Integer> xs) {
        if (a == 0b111111111) {
            if (n == 0) {
                String s = String.join(",", xs.stream().sorted().map(Object::toString).collect(Collectors.toList()));
                set.add(s);
            }
            return;
        }
        for (int j = 0; j < 9; j++) {
            if ((a & (1 << j)) == 0) {
                int newn = n * 10 + j + 1;
                if (sieve.isPrime(newn)) {
                    xs.add(newn);
                    count(a | (1 << j), 0, xs);
                    xs.remove(xs.size() - 1);
                }
                count(a | (1 << j), newn, xs);
            }
        }
    }
}
