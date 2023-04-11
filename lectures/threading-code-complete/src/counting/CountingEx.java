package counting;

public class CountingEx {

    public static void main(String args[]) {
        CountingThread t1 = new CountingThread(100);
        CountingThread t2 = new CountingThread(100);
        t1.start();
        t2.start();
        System.out.println("Both threads have started.");
    }
}

class CountingThread extends Thread {

    private int max;

    public CountingThread(int m) {
        this.max = m;
    }

    public void run() {
        for (int x = 0; x <= max; x++) {
            System.out.println(x);
        }
    }
}
