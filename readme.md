# Description

A Monte Carlo numerical simulation program is containd, which outputs
- equilibrium snapshots of two-dimensional Ising models evolving in time by the Wolff algorithm,
- non-equilibrium snapshots by the single-spin-flip algorithm,
- several (equilibrium and non-equilibrium) observables as time series.

We adopted an extended version, which include external static (integer-valued) field, as the Wolff algorithm.
Please refer [the link][1].

# Directory organization

The entity of the program is organized by the following rules
- Running binary is located in the directory ``./bin`` and may be modified by the command ``make``,
- Source codes for the main program and subprocedures are located in the directory ``./src`` and ``./lib``, respectively,
- (Fortram 90-specified) module files (with the extension ``.mod``) are in the root. Do not modify them independently.

The output data are organized by the following rules
- Each snapshots are saved in the outside directory ``../dat/snap``, with a parameter-named subdirectory,
- Each time series are saved in the outside directory ``../dat/stream``,
- Each simulation-parameter metadata are saved in the outside directory ``../dat/param``.

# Usage

in construction...

Roughly speaking, just put the parameter strings which you want to simulate by.

# Extension

This program is intended to connect with other programs, such as the converter from time-series to (averaged) physical value.

This program, therefore, has no room for improvements except for fundamental algorithmic feature or something fundamentals.

# Future development directions

- Dividing this program into two specific calculations, namely the equilibrium part and the non-equilibrium part.

[1]: http://latt.if.usp.br/technical-pages/twawesab/Text.html/Text.html