import java.util.Scanner;

class Main {
  public static void main(String[] args) {
    System.out.println("Welcome to the game!");
    int numRandom = (int)Math.random() * 100 + 1;  
    int userGuess = 0;
    Scanner scanner = new Scanner(System.in);
    System.out.println("The random number generated was: " + numRandom);

    while (true) {
      System.out.print("Enter a number: ");
      userGuess = scanner.nextInt();
      System.out.println("Your guess was " + userGuess);

      if (userGuess < numRandom) {
        System.out.println("Your guess is too small!");
      }
      else if (userGuess > numRandom) {
        System.out.println("Your guess is too big!");
      }
      else {
        System.out.println("You guessed the number!");
        break;
      }
    }
  }
}
