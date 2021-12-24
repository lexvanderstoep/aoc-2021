using System;
using System.Collections.Generic;
using System.Drawing;
using System.Numerics;

class Program {
    static int computeOptimalYVelocity(Rectangle targetArea) {
        return -targetArea.Y - 1;
    }

    static int computeXPosition(int time, int initialXPosition, int initialXVelocity) {
        if (time >= initialXVelocity) {
            return initialXPosition + initialXVelocity * (initialXVelocity + 1) / 2;
        } else {
            return initialXPosition + initialXVelocity * time - time * (time - 1) / 2;
        }
    }

    static int computeYPosition(int time, int initialYPosition, int initialYVelocity) {
        return initialYPosition + initialYVelocity * time - time * (time - 1) / 2;
    }

    static Vector2 computePosition(int time, Vector2 initialPosition, Vector2 initialVelocity) {
        return new Vector2(
            computeXPosition(time, (int) initialPosition.X, (int) initialVelocity.X),
            computeYPosition(time, (int) initialPosition.Y, (int) initialVelocity.Y));
    }

    static int computeMinXVelocity(Vector2 initialPosition, Rectangle targetArea) {
        return (int) Math.Ceiling((-1 + Math.Sqrt(1 + 8*(targetArea.X - initialPosition.X))) / 2);
    }

    static int computeMaxXVelocity(Vector2 initialPosition, Rectangle targetArea) {
        return targetArea.X + targetArea.Width - (int) initialPosition.X;
    }

    static int computeMinYVelocity(Vector2 initialPosition, Rectangle targetArea) {
        return targetArea.Y + (int) initialPosition.Y;
    }

    static int computeMaxYVelocity(Vector2 initialPosition, Rectangle targetArea) {
        return computeOptimalYVelocity(new Rectangle(targetArea.X - (int) initialPosition.X, targetArea.Y - (int) initialPosition.Y, targetArea.Width, targetArea.Height));
    }

    static List<Vector2> computeTrajectory(Vector2 initialPosition, Vector2 initialVelocity, Rectangle targetArea) {
        int time = 0;
        Vector2 position = computePosition(time, initialPosition, initialVelocity);
        List<Vector2> trajectory = new List<Vector2>();
        while (position.X <= targetArea.X + targetArea.Width & position.Y >= targetArea.Y) {
            trajectory.Add(position);
            time++;
            position = computePosition(time, initialPosition, initialVelocity);
        }
        return trajectory;
    }

    static bool hitsTargetArea(List<Vector2> trajectory, Rectangle targetArea) {
        foreach (var position in trajectory)
        {
            if (position.X >= targetArea.X & position.X <= targetArea.X + targetArea.Width & position.Y >= targetArea.Y & position.Y <= targetArea.Y + targetArea.Height) {
                    return true;
            }
        }
        return false;
    }

    static void Main(string[] args) {
        Rectangle targetArea = new Rectangle(277, -92, 41, 39);
        Vector2 initialPosition = new Vector2(0, 0);
        List<Vector2> hitVelocities = new List<Vector2>();
        int minXVelocity = computeMinXVelocity(initialPosition, targetArea);
        int maxXVelocity = computeMaxXVelocity(initialPosition, targetArea);
        int minYVelocity = computeMinYVelocity(initialPosition, targetArea);
        int maxYVelocity = computeMaxYVelocity(initialPosition, targetArea);
        for (int xVelocity = minXVelocity; xVelocity <= maxXVelocity; xVelocity++) {
            for (int yVelocity = minYVelocity; yVelocity <= maxYVelocity; yVelocity++) {
                Vector2 initialVelocity = new Vector2(xVelocity, yVelocity);
                var trajectory = computeTrajectory(initialPosition, initialVelocity, targetArea);
                if (hitsTargetArea(trajectory, targetArea)) {
                    hitVelocities.Add(initialVelocity);
                }
            }
        }
        Console.WriteLine("#Velocities: " + hitVelocities.Count);
    }
}