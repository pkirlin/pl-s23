
public class Rational {

	/* Fields (variables that each instance of the class has a copy of).
	 Typically these are private so that users can't muck with them
	 except by calling our public methods. 
	 */
	private int numerator, denominator;

	// Constructor.
	// Public so that anyone can use it.
	public Rational(int n, int d) {
		numerator = n;
		denominator = d;
	}

	// Every class has a toString() method.  It is called automatically
	// when an object is printed to the screen.  You can also call it
	// yourself whenever you want to.
	public String toString() {
		// Integer is a class that encapsulates the primitive "int" data type.
		// Here we call the Integer.toString() static method. 
		// A static method is a method that does not require an instance of 
		// the class to be called.
		// In C++ this would be Integer::toString(numerator).
		String answer = Integer.toString(numerator);

		if (denominator != 1) {
			answer += "/";
			answer += Integer.toString(denominator);
			// notice how += is overloaded for strings
			// + is as well.  Otherwise no operator overloading.
		}

		return answer;
	}

	public void add(Rational other) {
		int a = other.numerator;
		int b = other.denominator;
		int c = numerator;
		int d = denominator;
		numerator = (a * d) + (b * c);
		denominator = b * d;
		reduce();
	}

	// A private method that only we can call internally.  Outside users
	// of this class can't call it.
	private int gcd(int x, int y) {
		if (x == y) {
			return x;
		} else if (x < y) {
			return gcd(x, y - x);
		} else {
			return gcd(y, x);
		}
	}

	// Another private method.  This one reduces the fraction if possible.
	private void reduce() {
		if (numerator == 0) {
			denominator = 1;
		} else {
			int d = gcd(Math.abs(numerator), denominator);
			numerator = numerator / d;
			denominator = denominator / d;
		}
	}
}
