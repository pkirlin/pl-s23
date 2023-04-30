package inclass;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;

public class EventExample {

	public static void main(String args[]) {
		JFrame frame = new JFrame();
		JButton button = new JButton("Press me!");
		frame.add(button);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		HelloWorldListener listener = new HelloWorldListener();
		button.addActionListener(listener);
		
		frame.pack();
		frame.setVisible(true);
	}
}

/**
 * An ActionListener, when attached to an object that triggers
 * events, will automatically call actionPerformed whenever the event happens.
 */
class HelloWorldListener implements ActionListener {

	public void actionPerformed(ActionEvent event) {
		System.out.println("Hello world!");
	}
}