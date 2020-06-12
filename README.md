# Star Empire

A very simple game of planetary conquest.

## The game

The maps is a graph of planets, each one connected to a few other planets. Ships
can travel along these connections to take over enemy planets.

Planets produce ships at a steady rate; they can be conquered by flying more
ships there than are currently on the planet. A slight wrinkle is that planets
can have 'shields', each of which must be defeated by a ship first.

Planets can also be upgraded to produce ships faster (economic) or be better
defended (defensive).

## The code

I'm currently working on `2d.rkt`, which contains an as-yet-unfinished
implementation of the game mechanics in two dimensions.

I played around with racket's `pict3d` library in `3d.rkt`, but put that on hold
in favour of getting the game mechanics right in the 2D version first.
