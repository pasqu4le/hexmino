# Hexmino
Hexmino is a small game where you have to put domino-like hexagonal tiles into a grid in as little time as possible.

![screenshot](screenshot.png)

This game was written in haskell mostly as an experiment in the use of the [gloss library](https://hackage.haskell.org/package/gloss).

## Installation
> Note: You may need to install [freeglut](https://www.archlinux.org/packages/extra/x86_64/freeglut/) or [glfw](https://www.archlinux.org/packages/community/x86_64/glfw/) for hexmino to work, for more info see the [gloss website](http://gloss.ouroborus.net/).

For ArchLinux the binary from [the latest github release](https://github.com/pasqu4le/hexmino/releases/latest) should work.
For other Linux distro the binary may work as well, or you can build from source.

You can build from source using [cabal-install](http://hackage.haskell.org/package/cabal-install).
Since hexmino is on Hackage you can just use:

```
$ cabal install hexmino
```
or install from the cloned repository:
```
$ git clone https://github.com/pasqu4le/hexmino.git
$ cd hexmino
$ cabal install
```

## How to play
Once you selected a difficulty level and started the game you can drag and drop tiles from the queue on the right onto the grid.
You can rotate the tile you are dragging by pressing the spacebar.

The game ends when (and if) the grid is full and every tile on the grid matches with it's neighbours with each of it's faces.

The better your time, the better your position in the leaderboard.

## Command line arguments
You can only select the frames per second used in the game using `--fps` or `-f`, for example: `hexmino --fps 120`. If you don't specify it defaults to 60 fps.

## TODOs
- better text rendering (both in quality and performance), maybe using a font
- background music
- widgets animations