package barber;

import java.util.ArrayDeque;
import java.util.Queue;

class CheckoutLine {

	final public int totalSeats = 3;
	final public Queue<Customer> q = new ArrayDeque<Customer>();
}

class Cashier extends Thread {

	final private CheckoutLine line;

	public Cashier(CheckoutLine shop) {
		this.line = shop;
	}

	public void run() {
		while (true) {
			synchronized (line) {
				while (line.q.isEmpty()) {
					try {
						line.wait();
					} catch (InterruptedException e) {
					}
				}

				//while (!shop.q.isEmpty()) {
				Customer c = line.q.poll();
				line.notifyAll();
				System.out.println("Cutting hair: " + c);

			}
			try {
				Thread.sleep(100);
				//}
			} catch (InterruptedException ex) {
			}
		}
	}
}

class Customer extends Thread {

	final private CheckoutLine line;

	public Customer(CheckoutLine shop) {
		this.line = shop;
	}

	public void run() {
		synchronized (line) {
			while (line.q.size() >= line.totalSeats) {
				try {
					line.wait();
				} catch (InterruptedException e) {
				}
			}
			System.out.println("Customer sitting down: " + line.q.size() + this);
			line.q.add(this);
			line.notifyAll();

		}
		try {
			Thread.sleep(100);
			//}
		} catch (InterruptedException ex) {
		}
	}
}

public class BarberShopTest {

	public static void main(String args[]) {
		CheckoutLine shop = new CheckoutLine();
		Customer c1 = new Customer(shop);
		Customer c2 = new Customer(shop);
		Customer c3 = new Customer(shop);
		Customer c4 = new Customer(shop);
		Cashier b = new Cashier(shop);


		for (int x = 0; x < 20; x++) {
			Customer c = new Customer(shop);
			c.start();
		}

		c1.start();
		c2.start();
		c3.start();
		c4.start();
		b.start();
	}
}
