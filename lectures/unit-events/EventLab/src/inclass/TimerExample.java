package inclass;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Timer;

public class TimerExample {

	public static void main(String args[])
	{
		Listener listener = new Listener();
		Timer timer = new Timer(250, listener);
		timer.start();
		try { Thread.sleep(1000); } catch (InterruptedException e) {}
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
