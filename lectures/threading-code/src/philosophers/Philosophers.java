
package philosophers;

class Fork {}

class Philosopher extends Thread
{
	private Fork left, right;
	
	public Philosopher(Fork l, Fork r)
	{
		left = l; right = r;
	}
	
	public void run()
	{
		synchronized (left)
		{
			System.out.println(Thread.currentThread().getName() + " takes left fork.");
			try { Thread.sleep(1000);  } catch (InterruptedException e) {}
			synchronized (right)
			{
				System.out.println(Thread.currentThread().getName() + " takes right fork and eats.");
				try { Thread.sleep(1000);  } catch (InterruptedException e) {}
			}
		}
	}
}


public class Philosophers {
	public static void main(String args[])
	{
		Fork f1 = new Fork(), f2 = new Fork(), f3 = new Fork(), f4 = new Fork(), f5 = new Fork();
		Philosopher p1 = new Philosopher(f1, f2);
		Philosopher p2 = new Philosopher(f2, f3);
		Philosopher p3 = new Philosopher(f3, f4);
		Philosopher p4 = new Philosopher(f4, f5);
		Philosopher p5 = new Philosopher(f5, f1);
		
		p1.start();
		p2.start();
		p3.start();
		p4.start();
		p5.start();
		
	}
}
