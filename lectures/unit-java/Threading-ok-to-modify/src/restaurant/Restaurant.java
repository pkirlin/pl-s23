// Restaurant 1.0:
// Chef and waiter operate concurrently without
// any communication.

package restaurant;

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
			try { // simulate time to cook
				Thread.sleep((int) (Math.random() * 1000));
			}
			catch (InterruptedException e) {}
			System.out.println("Cooked order #" + orderNum);
			pickupArea.orderNumber = orderNum;
		}
	}
}

class Waiter extends Thread {
	private final PickupArea pickupArea;
	
	public Waiter(PickupArea a) { this.pickupArea = a; }

	public void run() {
		for (int orderNum = 1; orderNum <= 10; orderNum++) {
			// retrieve an order
			int order = pickupArea.orderNumber;
			// reset the pickup area
			pickupArea.orderNumber = 0;
			// serve food
			System.out.println("Served order #" + order);
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
