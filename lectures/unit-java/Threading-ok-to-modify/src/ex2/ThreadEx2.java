package ex2;

public class ThreadEx2 {

	public static void main(String args[]) {
		SimpleThread t1 = new SimpleThread();
		SimpleThread t2 = new SimpleThread();
		t1.start();
		t2.start();
	}
}

class SimpleThread extends Thread {

	public void run() {
		System.out.println("I am a thread!");
	}
}
