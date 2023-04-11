// Restaurant 2: One chef, one waiter, correctly synched.

package restaurant2;

class PickupArea
{
	// note: bad data hiding!
	// orderNumbers are positive, or 0 for no order waiting
	public int orderNumber = 0;
}

class Chef extends Thread {
	private final PickupArea pickupArea;
	
	public Chef(PickupArea a) { this.pickupArea = a; }

	public void run() {
		for (int orderNum = 1; orderNum <= 10; orderNum++) {
			try { 
				// simulate time to cook
				Thread.sleep((int) (Math.random() * 1000));

				synchronized (pickupArea) {
					// wait until the pickup area is free
					while (pickupArea.orderNumber > 0) {
						System.out.println("Chef: is waiting");
						pickupArea.wait();
						System.out.println("Chef: woke up");
					}
					// we are now guaranteed that the pickup area is empty.
					// since we own the pickup area's lock, nobody could have
					// changed it between the end of the wait() above and here.
					
					// put the food in the pickup area.
					pickupArea.orderNumber = orderNum;
					System.out.println("Chef: Sent out order #" + orderNum);
					
					// signal the waiter to come get it
					pickupArea.notifyAll();
					System.out.println("Chef: Waiter notified of order #" + orderNum);
				}
			} catch (InterruptedException e) {
			}
		}
	}
}

class Waiter extends Thread {
	private final PickupArea pickupArea;
	
	public Waiter(PickupArea a) { this.pickupArea = a; }

	public void run() {
		for (int orderNum = 1; orderNum <= 10; orderNum++) {
			try {
				int order;
				synchronized (pickupArea) {
					// wait until the pickup area has food
					while (pickupArea.orderNumber == 0) {
						System.out.println("Waiter: is waiting");
						pickupArea.wait();
						System.out.println("Waiter: woke up");
					}

					// we are now guaranteed that the pickup area has food.
					// since we own the pickup area's lock, nobody could have
					// changed it between the end of the wait() above and here.

					// get the food in the pickup area and clear the area.
					order = pickupArea.orderNumber;
					pickupArea.orderNumber = 0;
					System.out.println("Waiter: Picked up order #" + order);

					// signal the chef that the pickup area is free.
					pickupArea.notifyAll();
					System.out.println("Waiter: Notified chef of open pickup area.");
				}
				// simulate time to serve the food
				Thread.sleep((int) (Math.random() * 1000));
			} catch (InterruptedException e) {}
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
