# robots

An approximate clone of the [classic BSD robots game](https://en.wikipedia.org/wiki/Robots_(computer_game)). I wrote this mainly as an exercise to get some practice with Clojure. I left out scoring, but added undo/redo and safe teleport.

## Usage

You need Clojure and leiningen.

Just run it with leiningen:

    $ lein run

The only command line argument recognized is an optional number indicating the level to start at (defaults to 1).

    $ lein run 10

## Playing the game

Robots is a turn-based game. You take a turn (move one space or wait in place or teleport) then the robots take their turn and move one space closer to you. When robots enter the same space, they crash into each other and form a debris pile. If you enter the same space as a robot or pile, you die. If a robot enters your space, you die. If all of the robots crash, you advance to the next level. Each level starts with a number of robots equal to 10 times the level number. Eventually there will be so many robots that you will die.

Here's how a level might start (in this case, level 5 with 50 robots).

    +-----------------------------------------------------------+
    |             +                +   + +                      | Level 5
    |                                                           |
    |                                     +        +            | Moves 0
    |                                         +                 |
    |+   +                                      +   +           | Robots 50/50
    |     +                          +      +                   |
    |       +                   +                               | Piles 0
    |                                                           |
    |      +                               +                    | Alive :)
    |                                                           |
    |                              +               +            |
    |                +    +                +                    |
    |         +                          +                      |
    |              +                   +   +              +     |
    |                             +                             |
    |           +                +                              |
    |     +                              +                      |
    |   +    +              +            +  @                  +|
    |       +                          +            +           |
    |                                        +  +              +|
    |                   +                                       |
    |                              ++              + +          |
    +-----------------------------------------------------------+
    Move HJKLYUBN or numpad [T]teleport [space]wait [Z]undo [X]redo

The player is represented by an `@`. Each robot is a `+`. A debris pile is a `*`.

If the player moves north on their turn, here's how the board will look after the robots move.

    +-----------------------------------------------------------+
    |                                                           | Level 5
    |              +                +   + +                     |
    |                                                           | Moves 1
    |                                      +      +             |
    |                                        +                  | Robots 48/50
    | +   +                                    +   +            |
    |      +                          +     +                   | Piles 1
    |        +                   +                              |
    |                                                           | Alive :)
    |       +                               +                   |
    |                                                           |
    |                               +             +             |
    |                 +    +                +                   |
    |          +                          +                     |
    |               +                   +   +            +      |
    |                              +                            |
    |    + +  +  +           +    +       * @                 + |
    |        +                          +          +            |
    |                                       +  +              + |
    |                    +                                      |
    |                               ++            + +           |
    |                                                           |
    +-----------------------------------------------------------+
    Move HJKLYUBN or numpad [T]teleport [space]wait [Z]undo [X]redo

Note that 2 robots have crashed and formed a pile.

### Keys

Move with either the numpad (where `5` is wait, `8` is north, etc) or with classic Vi-style keys (`H` for east, `J` for south, `K` for north, `L` for west, plus `Y` for northwest, `U` for northeast, `B` for southwest and `N` for southeast).

Wait with `space` or `.`.

`T` to teleport to a random location (possibly on a robot or pile, resulting in death).

`W` to wait for until either the robots kill themselves or you.

#### Cheat Keys

`Z` will undo the last turn. `X` will redo the last undo. (Undo and redo apply to the current level only; you can't undo back to a previous level.)

`S` teleports to a safe location. Note that this makes the game too easy; you can just safe teleport until the robots kill themselves... fun to watch, but where's the challenge? If you get to the later levels, you may find yourself on a board with so many robots that there is no safe space to teleport to!

## License

Copyright © 2016 Sam Brauer

Distributed under the MIT License.
