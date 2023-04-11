package ex1;

public class ThreadEx1 {

	public static void main(String args[]) {
		SimpleThread t1 = new SimpleThread();
		t1.start();
	}
}

class SimpleThread extends Thread {

	public void run() {
		System.out.println("I am a thread!");
	}
}
