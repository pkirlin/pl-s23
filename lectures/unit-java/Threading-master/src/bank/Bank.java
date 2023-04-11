package bank;

class BankAccount
{
	private int balance = 0;
	public void deposit(int x) { balance += x; }
	public void withdraw(int x) { 
		if (balance >= x) { balance -= x; }
	}
	public int getBalance() { return balance; }
}

class Transaction extends Thread
{
	private BankAccount acc;
	private int whatToDo;

	public Transaction(BankAccount acc, int x) {
		this.acc = acc;
		this.whatToDo = x;
	}

	public void run()
	{
		if (whatToDo > 0)
			acc.deposit(whatToDo);
		else if (whatToDo < 0)
			acc.withdraw(-whatToDo);
		int bal = acc.getBalance();
		if (bal < 0) {
			System.out.println("balance went negative: " + bal);
			System.exit(1);
		}
	}
}

public class Bank
{
	public static void main(String args[])
	{
		BankAccount acc = new BankAccount();

		while (true)   // attempt forever to get negative balance
		{
			Transaction t1 = new Transaction(acc, 1);
			Transaction t2 = new Transaction(acc, -1);
			Transaction t3 = new Transaction(acc, -1);
			t1.start();
			t2.start();
			t3.start();
		}
	}
}