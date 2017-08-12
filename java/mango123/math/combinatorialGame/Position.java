package mango123.math.combinatorialGame;

import java.util.Iterator;
import java.util.List;

// java type system please, the lack of self reference...
public interface Position<T extends Position<T>> {
	// need to at least implement one method and use corresponding solver method to solve in Solver
	// trade offs:
	//  - List are likely much easier to write and less verbose than iterators
	//  - Lists are memory intensive compare to Iterator. Lazy evaluation also make Iterator the faster method.
	//  - I would recommend List and only moving to Iterator if needed.

	// in addition, try to make it so nextPositions reach the base case faster
	// nim(16,32,48) should generate positions nim(0,32,48), nim(1, 32, 48)... for instance
	// will solve much faster that way
	List<T> nextPositions();
	Iterator<T> nextPositionsIter();
}