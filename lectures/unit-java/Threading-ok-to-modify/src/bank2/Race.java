package bank2;

class BankAccount {

	private int balance = 0;

	public synchronized void deposit(int x) {
		balance += x;
	}

	public synchronized void withdraw(int x) {
		if (balance >= x) {
			balance -= x;
		}
	}

	public int getBalance() {
		return balance;
	}
}

class DepositorThread extends Thread {

	private BankAccount acc;

	public DepositorThread(BankAccount acc) {
		this.acc = acc;
	}

	public void run() {
		for (int x = 0; x < 1000; x++) {
			acc.deposit(1);
		}
		System.out.println("done");
	}
}

public class Race {

	public static void main(String args[]) throws InterruptedException {

		BankAccount acc = new BankAccount();

		for (int x = 0; x < 5; x++) {
			DepositorThread i = new DepositorThread(acc);
			i.start();
		}
		
		Thread.sleep(1000); // hack: wait for all the threads to finish
		System.out.println("Account has " + acc.getBalance()); // should have $5000
	}
}