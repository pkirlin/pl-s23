public class RationalTest {

	public static void main(String args[])
	{
		Rational rat = new Rational(3, 4);
		Rational rat2 = new Rational(1, 2);
		
		System.out.println("First rational is " + rat);
		System.out.println("Second rational is " + rat2);
		rat.add(rat2);
		System.out.println("Sum is " + rat);
	}	
}