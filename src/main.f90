PROGRAM TEST
! Test the existing functionalities of the neural network module.
USE NEURALNET_MOD
USE REAL_PRECISION
IMPLICIT NONE

TYPE(LAYER) :: MYLAYER
TYPE(NEURALNET) :: MYNET
REAL(KIND=R8) :: X(4), B(20)
REAL(KIND=R8) :: DATA(5,20), WEIGHTS(20), BIAS(5), A(20,4)
REAL(KIND=R8) :: WORK(50)
INTEGER :: IERR, LWORK, I

REAL(KIND=R8), EXTERNAL :: DDOT
CALL RANDOM_NUMBER(DATA(:,:))

A(:,:) = TRANSPOSE(DATA(1:4,:))
B(:) = DATA(5,:)
LWORK=50
CALL DGELS('N', 20, 4, 1, A, 20, B, 20, WORK, LWORK, IERR)
IF(IERR .NE. 0) PRINT *, IERR

MYNET = NEURALNET()
MYLAYER = LAYER(4, IERR)
IF(IERR .NE. 0) PRINT *, IERR
CALL MYNET%ADD(MYLAYER, IERR)
IF(IERR .NE. 0) PRINT *, IERR
MYLAYER = LAYER(4, IERR)
IF(IERR .NE. 0) PRINT *, IERR
CALL MYNET%ADD(MYLAYER, IERR)
IF(IERR .NE. 0) PRINT *, IERR
MYLAYER = LAYER(1, IERR)
IF(IERR .NE. 0) PRINT *, IERR
CALL MYNET%ADD(MYLAYER, IERR)
IF(IERR .NE. 0) PRINT *, IERR

CALL MYNET%TRAIN(DATA, IERR, LAMBDA=0.0_R8)
IF(IERR .NE. 0) PRINT *, IERR

DO I = 1, 4
   X(:) = 0.0_R8
   X(I) = 1.0_R8
   A(1,1) = DDOT(4, B, 1, X, 1)
   PRINT *, 'Optimal value: ', A(1,1)
   PRINT *, 'Predicted value: ', MYNET%EVALUATE(X,IERR)
END DO

END PROGRAM TEST
