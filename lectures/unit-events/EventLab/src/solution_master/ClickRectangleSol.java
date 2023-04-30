package solution_master;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.Timer;

public class ClickRectangleSol {

	public static void main(String args[]) {
		GameFrame frame = new GameFrame();
		frame.pack();
		frame.setVisible(true);
	}
}

class GameFrame extends JFrame {

	private GamePanel gameArea;
	private JButton startButton;

	public GameFrame() {
		setTitle("Click the Rectangles!");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		gameArea = new GamePanel();
		startButton = new JButton("Start");

		add(startButton, BorderLayout.NORTH);
		add(gameArea, BorderLayout.CENTER);

		StartButtonActionListener buttonListener = new StartButtonActionListener();
		startButton.addActionListener(buttonListener);
		
		GameMouseClickListener clickListener = new GameMouseClickListener();
		gameArea.addMouseListener(clickListener);
	}

	class StartButtonActionListener implements ActionListener {

		public void actionPerformed(ActionEvent event) {
			MoveShapesActionListener listener = new MoveShapesActionListener();
			Timer timer = new Timer(10, listener);
			timer.start();
		}
	}

	class GameMouseClickListener extends MouseAdapter {

		public void mouseReleased(MouseEvent event) {
			gameArea.handleMouseClick(event.getX(), event.getY());
			repaint();
		}
	}

	class MoveShapesActionListener implements ActionListener {

		public void actionPerformed(ActionEvent event) {
			gameArea.moveShapesToLeft();
			repaint();
		}
	}
}

class GamePanel extends JPanel {

	public int AREA_WIDTH = 500;
	public int AREA_HEIGHT = 500;
	private int score;
	private List<Rectangle> shapes = new ArrayList<Rectangle>();

	public GamePanel() {
		setPreferredSize(new Dimension(AREA_WIDTH, AREA_HEIGHT));
		// add some random rectangles
		for (int c = 0; c < 10; c++) {
			shapes.add(makeRandomRectangle());
		}
		
	}

	public void handleMouseClick(int x, int y) {
		for (Rectangle r : shapes) {
			if (r.contains(x, y)) {
				score += 100000 / (r.width * r.width + r.height * r.height);
				System.out.println(100000 / (r.width * r.width + r.height * r.height));
				Rectangle r2 = makeRandomRectangle();
				r.setRect(AREA_WIDTH, r2.y, r2.width, r2.height);
			}
		}
	}

	public void moveShapesToLeft() {
		for (Rectangle r : shapes) {
			r.translate(-1, 0);
			if (r.x <= 0) {
				Rectangle r2 = makeRandomRectangle();
				r.setRect(AREA_WIDTH, r2.y, r2.width, r2.height);
			}
		}
	}

	private Rectangle makeRandomRectangle() {
		int MAX_HEIGHT = 100;
		int MAX_WIDTH = 100;

		int x = (int) (Math.random() * AREA_WIDTH);
		int y = (int) (Math.random() * AREA_HEIGHT);

		int w = (int) (Math.random() * (MAX_WIDTH - 10) + 10);
		int h = (int) (Math.random() * (MAX_HEIGHT - 10) + 10);

		return new Rectangle(x, y, w, h);
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		g.setColor(Color.black);
		g.drawString("Score: " + score, 20, 20);
		for (Rectangle r : shapes) {
			g.drawRect(r.x, r.y, r.width, r.height);
		}
	}
}