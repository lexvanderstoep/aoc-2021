using System;
using System.Drawing;

class Program {
    static int computeOptimalYVelocity(Rectangle targetArea) {
        return -targetArea.Y - 1;
    }

    static int computeYPosition(int time, int initialYPosition, int initialYVelocity, int gravity) {
        return initialYPosition + initialYVelocity * time - gravity * time * (time - 1) / 2;
    }

    static int computeHighestYPosition(int initialYPosition, int initialYVelocity, int gravity) {
        int inflectionTime = initialYVelocity;
        return computeYPosition(inflectionTime, initialYPosition, initialYVelocity, gravity);
    }

    static void Main(string[] args) {
        Rectangle targetArea = new Rectangle(277, -92, 41, 39);
        int initialYVelocity = computeOptimalYVelocity(targetArea);
        int highestYPosition = computeHighestYPosition(0, initialYVelocity, 1);
        Console.WriteLine("Answer: " + highestYPosition);
    }
}