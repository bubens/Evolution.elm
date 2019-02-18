The Evolution-Game
==================

Simulation of a game I like and that is loosely based on evolution.

Install
-------
Clone the repo and run `make.sh` in the folder. `elm` and `uglifyjs` should be installed.

Rules
-----
All creatures begin the game in the same initial state "rank 0" and float around on the screen. If two creatures of the same rank meet they fight for progressing in the evolution. The winner gets a promotion to the next rank, the looser gets demoted to the rank before. Creatures that reach rank 5 retire and stay seated.
