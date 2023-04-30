package master;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Timer;

public class TimerExample {

	public static void main(String args[])
	{
		Listener listener = new Listener();
		
		// Make a new timer.  It will fire every 250 ms, and will call
		// the actionPerformed method on "listener."
		Timer timer = new Timer(250, listener);
		
		// Actually start the timer.
		timer.start();
		
		// This sleep line below is not usually used in practice.  It's only here
		// to give our timer some time to fire off some events.
		try { Thread.sleep(2000); } catch (InterruptedException e) {}
		
		// Stop the timer from firing events every 250 ms.
		timer.stop();
	}
}

class Listener implements ActionListener
{
	public void actionPerformed(ActionEvent event)
	{
		System.out.println("Event triggered!");
	}
}
