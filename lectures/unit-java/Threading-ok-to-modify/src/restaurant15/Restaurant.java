// Resaurant 1.5:
// busy waits -- deadlock because one thread hogs
// the scheduler and never switches to the other thread.

package restaurant15;

class PickupArea
{
	// note: bad data hiding!
	// orderNumbers are positive, or 0 for no order waiting
	public int orderNumber;
}

class Chef extends Thread {
	private final PickupArea pickupArea;
	
	public Chef(PickupArea a) { this.pickupArea = a; }

	public void run() {
		for (int orderNum = 1; orderNum <= 10; orderNum++) {
			System.out.println("Chef is about to to try to cook " + orderNum);
			try { // simulate time to cook
				Thread.sleep((int) (Math.random() * 1000));
			}
			catch (InterruptedException e) {}
			
			// wait until the pickup area is available (has no order in it)
			while (pickupArea.orderNumber > 0) { }
			System.out.println("Chef: about to synchronize");
			
			synchronized (pickupArea) {
				System.out.println("Cooked order #" + orderNum);
				pickupArea.orderNumber = orderNum;
			}
		}
	}
}

class Waiter extends Thread {
	private final PickupArea pickupArea;
	
	public Waiter(PickupArea a) { this.pickupArea = a; }

	public void run() {
		for (int orderNum = 1; orderNum <= 10; orderNum++) {
			System.out.println("Waiter is about to to try to serve" + orderNum);
			
			// wait until there is food in the pickup area
			while (pickupArea.orderNumber == 0) { }
			
			System.out.println("Waiter: food is available!");
			
			synchronized (pickupArea) {
				// retrieve an order
				int order = pickupArea.orderNumber;
				// reset the pickup area
				pickupArea.orderNumber = 0;
				
				// serve food
				System.out.println("Served order #" + order);
			}
			
			try { // simulate time to serve
				Thread.sleep((int) (Math.random() * 1000));
			}
			catch (InterruptedException e) {}
		}
	}
}

public class Restaurant {
	public static void main(String args[])
	{
		PickupArea area = new PickupArea();
		Chef chef = new Chef(area);
		chef.start();
		Waiter waiter = new Waiter(area);
		waiter.start();
	}
}
