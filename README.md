# Masyu Solver - Scala Project

## Project Overview

This project implements a solution to the logic puzzle Masyu (also known as Pearl) using Scala. The puzzle involves drawing a single continuous line through a board of black and white tiles, following specific rules. The solution uses a combination of functional programming principles and search algorithms to efficiently solve puzzles where the sum of height and width is 40 or less, with a few edge cases.

## Authors

- **Jon Ingvar Skånøy**
- **Isak Killingrød**

This project was completed as part of the IKT212 course on 10th August 2024.

## Abstract

The Masyu solver employs a two-step algorithm:
1. **Forced Moves**: Apply all moves that are mandatory based on the current board state.
2. **Search Algorithm**: Test potential moves when no forced moves are available, applying all obvious moves following the searched move.

This approach is both efficient and correct for most boards, with edge cases noted for particularly large or complex puzzles.

## Solution Details

### Design Patterns

The project largely adheres to functional programming principles:
- **Immutability**: Each board state generates a new board rather than modifying the existing one.
- **Recursion**: Recursion is used in place of iterative loops wherever possible, although some iterative patterns were necessary for simplicity.

### Data Structures

- **Enumeration**: Utilized case classes for clear and bug-resistant encoding of board states (`Line` and `TileType`).
- **Tiles**: Each tile is represented by a `Tile` class, which includes its state, coordinates, and utility functions for checking properties like line placement.
- **Boards**: The `Puzzle` class represents the entire board, containing a 2D array of tiles and utility functions to assist the solver.

### PuzzleSolver

The `PuzzleSolver` class is the core of the program. It handles:
- **Input/Output**: Managed by the `PuzzleReaderWriter` class.
- **Solving**: Through recursive application of forced moves and search functions to find and apply legal moves.

### Correctness and Efficiency

- The program is correct by the rules of Masyu, ensuring that all moves and the final solution follow the game's constraints.
- Some edge cases exist, particularly with larger, more complex puzzles, but these are exceptions rather than the rule.

### Empirical Evidence

Extensive testing was conducted using both local puzzles and puzzles from online sources such as [Puzzle Masyu](https://www.puzzle-masyu.com/) and [Kakuro Online](https://kakuro-online.com/masyu/). The program solves all 126 puzzles on Bamboo correctly and handles most large puzzles, with a few exceptions.

## Project Timeline

The project was developed over five weeks:
- **Week 1**: Setup, basic Scala learning, and initial enumeration.
- **Week 2**: Implementation of basic rules and puzzle solving for smaller boards.
- **Week 3**: Completion of specific rules and utility functions.
- **Week 4**: Implementation of search functions for more complex puzzles.
- **Week 5**: Refactoring for functional programming, optimization, and final testing.

## Reflection

While the project was largely successful, future improvements could include:
- **Earlier Focus on Functional Programming**: Starting with a fully functional approach from the beginning could have saved time later in refactoring.
- **Better Planning**: A more detailed plan might have improved efficiency, particularly in writing and refining the report.

## How to Run the Project

1. Clone this repository.
2. Compile the Scala project using your preferred IDE or command line.
3. Provide input puzzle files as specified.
4. Run the `PuzzleSolver` class to solve the puzzles.
5. Check the output directory for the solutions.

## Summary

The project demonstrates a robust and efficient approach to solving Masyu puzzles using Scala. The solution leverages functional programming principles, although with some necessary compromises for the sake of simplicity and speed. Extensive testing ensures that the program is both correct and performant across a wide range of puzzles.

