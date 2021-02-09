# NN\_MOD: A Fortran 2008 Module for neural network regressors

This module contains Fortran 2008 classes for efficiently training and
evaluating dense neural networks, without support for many advanced features
such as batch training, dropout, convolutions, and distributed parallelism.
However, as a prototype library for training and evaluating neural networks
regressors, this library is currently in working order.

To take this code to the next level, the following modifications are needed.
 - Support for other optimizers: I have provided several possible optimization
   subroutines in `OPT\_MOD` but have not added the code to support changing
   SGD for any of the other options.
 - Add L-BFGS to the list in `OPT\_MOD`.
 - Add support for training and evaluating with dropout (in the form of a
   dropout layer).
 - Add a transformer layer.
 - Add a recurrent layer.
 - Add support for two- and three-tensor inputs, including two- and three-
   dimensional convolutional filters.
 - Add support for shared-memory and distributed memory parallelism.
 - Add support to train on a randomized subset of the available data, and
   for streaming said data from a file so that it is never all held in
   primary memory.
 - Add a checkpointing feature for fault tolerance.
 - Add support for classification problems (some alternative activation
   functions and output layers are needed).
 - Add support for convolutional layers and recurrent network structures.

## Contents

The `src` subdirectory contains the following source files:
 - `neuralnet.f90` contains the `NEURALNET\_MOD` module, which defines the
   `NEURALNET` derived data type/Fortran 2008 class.
   Documentation contained therein.
 - `optimizers.f90` contains `OPT\_MOD`, which defines several common
   first-order convex optimization algorithms.
 - `main.f90` contains a driver that tests `NEURALNET\_MOD` by training to
   solve a linear least squares problem using both QR factorization (LAPACK)
   and a neural network with linear activations.
   The outputs should be approximately equal if the `NEURALNET` object is
   working properly.
 - A GNU `Makefile` builds the project and executes the driver code.

The `test` subdirectory contains a more detailed test suite (in progress).
The included GNU `Makefile` builds and runs the tests.

## Installation

### Prerequisites

`NN\_MOD` requires both `BLAS` and `LAPACK` for efficient linear algebra.

### Building

To install, pull this repo and run
``
make -B
``

## Author

* ** Tyler Chang ** - *Primary author*

