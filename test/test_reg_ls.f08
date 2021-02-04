PROGRAM TEST_REG_LS
! Test the existing functionalities of the neural network module.
USE NEURALNET_MOD
USE REAL_PRECISION
IMPLICIT NONE

! Define local variables.
INTEGER :: IERR, LWORK, I
LOGICAL :: ERR
! Define work and data arrays.
REAL(KIND=R8) :: X(4), B(20)
REAL(KIND=R8) :: DATA(5,20), WEIGHTS(20), BIAS(5), A(20,4)
REAL(KIND=R8) :: WORK(50)
! Define the neural network handles.
TYPE(LAYER) :: MYLAYER
TYPE(NEURALNET) :: MYNET
! External BLAS function DDOT.
REAL(KIND=R8), EXTERNAL :: DDOT

! Randomly generate the data.
CALL RANDOM_NUMBER(DATA(:,:))

! Set up a LOGICAL value to track whether any errors have occurred.
ERR = .FALSE.

! Solve a linear system to check against the results.
A(:,:) = TRANSPOSE(DATA(1:4,:))
B(:) = DATA(5,:)
LWORK=50
CALL DGELS('N', 20, 4, 1, A, 20, B, 20, WORK, LWORK, IERR)
IF(IERR .NE. 0) ERR = .TRUE.

! Create a network with 3 layers using all linear activation functions.
MYNET = NEURALNET()
MYLAYER = LAYER(4, IERR) ! Input layer.
IF(IERR .NE. 0) ERR = .TRUE.
CALL MYNET%ADD(MYLAYER, IERR)
IF(IERR .NE. 0) ERR = .TRUE.
MYLAYER = LAYER(4, IERR) ! Hidden layer.
IF(IERR .NE. 0) ERR = .TRUE.
CALL MYNET%ADD(MYLAYER, IERR)
IF(IERR .NE. 0) ERR = .TRUE.
MYLAYER = LAYER(1, IERR) ! Output layer.
IF(IERR .NE. 0) ERR = .TRUE.
CALL MYNET%ADD(MYLAYER, IERR)
IF(IERR .NE. 0) ERR = .TRUE.

! Train the neural network using gradient descent and backpropogation.
CALL MYNET%TRAIN(DATA, IERR, LAMBDA=0.0_R8)
IF(IERR .NE. 0) ERR = .TRUE.

! If no errors occurred, check that the results match a linear regression.
IF (.NOT. ERR) THEN
   DO I = 1, 4
      X(:) = 0.0_R8
      X(I) = 1.0_R8
      A(1,1) = DDOT(4, B, 1, X, 1)
      IF (ABS(A(1,1) - MYNET%EVALUATE(X, IERR)) > 0.01_R8) THEN
         ERR = .TRUE.
         EXIT
      END IF
      IF (IERR .NE. 0) THEN
         ERR = .TRUE.
         EXIT
      END IF
   END DO
END IF

! Report the results.
IF (ERR) THEN
   WRITE (*,*) "Error: Least Squares Regression Test FAILED"
ELSE
   WRITE (*,*) "PASSED: Least Squares Regression Test"
END IF

END PROGRAM TEST_REG_LS
