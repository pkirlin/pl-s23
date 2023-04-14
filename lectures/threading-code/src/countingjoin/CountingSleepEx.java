package countingjoin;

public class CountingSleepEx {

	public static void main(String args[]) {
		CountingSleepThread t1 = new CountingSleepThread(100, 10);
		CountingSleepThread t2 = new CountingSleepThread(100, 10);
		t1.start();
		t2.start();
		System.out.println("Both threads have started.");
		
		// make this line appear after t1 and t2 are done.
		/*try {
			t1.join();
			t2.join();
		} catch (InterruptedException e) {}*/
		System.out.println("Both threads have ended.");
	}
}

class CountingSleepThread extends Thread {

	private int max;
	private int sleepDur;

	public CountingSleepThread(int m, int s) {
		this.max = m;
		this.sleepDur = s;
	}

	public void run() {
		for (int x = 0; x <= max; x++) {
			System.out.println(currentThread().getName() + " " + x);
			try { Thread.sleep(sleepDur); }
			catch (InterruptedException e) {}
		}
	}
}
