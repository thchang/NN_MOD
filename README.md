# NN\_MOD: A Fortran 2008 Module for neural network regressors

This module contains Fortran 2008 classes for efficiently training and
evaluating dense neural networks, without support for many advanced features
such as batch training, dropout, convolutions, and distributed parallelism.
However, as a prototype library for training and evaluating neural networks
regressors, this library is currently in working order.

To take this code to the next level, the following modifications are needed.
 - Support for other optimizers: I have provided several possible optimization
   subroutines in OPT\_MOD but have not addded the code to support changing
   SGD for any of the other options.
 - Add support for training and evaluating with dropout: This is trivial to
   implement, but to do it correctly, I need to add a library for sparse
   linear algebra.
 - Add support for shared-memory (trivial) and distributed memory (difficult)
   parallelism.
 - Add support to train on a randomized subset of the available data, and
   for streaming said data from a file so that it is never all held in
   primary memory.
 - Add a checkpointing feature for fault tollerance.
 - Add L-BFGS to the list in OPT\_MOD
 - Add support for classification problems (some alternative activation
   functions and output layers are needed).
 - Add support for convolutional layers and recurrent network structures.
 - Add support for two- and three-tensor inputs, including two- and three-
   dimensional convolutional filters.

## Contents

This repo contains the following files:
 - neuralnet.f08 contains the NEURALNET\_MOD module, which defines the
   NEURALNET derived data type/Fortran 2008 class.
   Documentation contained therein.
 - optimizers.f08 contains OPT\_MOD, which defines several common first-order
   convex optimization algorithms.
 - main.f08 contains a driver that tests NEURALNET\_MOD by training to
   solve a linear least squares problem using both QR factorization (LAPACK)
   and a neural network with linear activations.
   The outputs should be approximately equal if the NEURALNET object is
   working properly.
 - Makefile builds the project and executes the driver code.

## Installation

### Prerequisites

NN\_MOD requires both BLAS and LAPACK for efficient linear algebra.

### Building

To install, pull this repo and run
``
make -B
``

## Author

* ** Tyler Chang ** - *Primary author*

