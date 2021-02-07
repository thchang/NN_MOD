PROGRAM TEST_ACTIV
! Test the existing functionalities of the neural network module.
USE NEURALNET_MOD
USE REAL_PRECISION
IMPLICIT NONE

! Define local variables.
INTEGER :: IERR

! Set up a LOGICAL value to track whether any errors have occurred.
IERR = 0

! Test the LINEAR function.
IF (0.0_R8 .NE. LINEAR(0.0_R8, FW = .TRUE.)) THEN
   IERR = 1
ELSE IF (1.0_R8 .NE. LINEAR(1.0_R8, FW = .TRUE.)) THEN
   IERR = 1
ELSE IF (-1.0_R8 .NE. LINEAR(-1.0_R8, FW = .TRUE.)) THEN
   IERR = 1
ELSE IF (0.0_R8 .NE. LINEAR(0.0_R8, FW = .FALSE.)) THEN
   IERR = 1
ELSE IF (1.0_R8 .NE. LINEAR(1.0_R8, FW = .FALSE.)) THEN
   IERR = 1
ELSE IF (-1.0_R8 .NE. LINEAR(-1.0_R8, FW = .FALSE.)) THEN
   IERR = 1
! Test the RELU function.
ELSE IF (0.0_R8 .NE. RELU(0.0_R8, FW = .TRUE.)) THEN
   IERR = 2
ELSE IF (1.0_R8 .NE. RELU(1.0_R8, FW = .TRUE.)) THEN
   IERR = 2
ELSE IF (0.0_R8 .NE. RELU(-1.0_R8, FW = .TRUE.)) THEN
   IERR = 2
ELSE IF (0.0_R8 .NE. RELU(0.0_R8, FW = .FALSE.)) THEN
   IERR = 2
ELSE IF (1.0_R8 .NE. RELU(1.0_R8, FW = .FALSE.)) THEN
   IERR = 2
ELSE IF (0.0_R8 .NE. RELU(-1.0_R8, FW = .FALSE.)) THEN
   IERR = 2
! Test the SIGMOID function.
ELSE IF (0.5_R8 .NE. SIGMOID(0.0_R8, FW = .TRUE.)) THEN
   IERR = 3
ELSE IF (0.5_R8 .GE. SIGMOID(1.0_R8, FW = .TRUE.)) THEN
   IERR = 3
ELSE IF (0.5_R8 .LE. SIGMOID(-1.0_R8, FW = .TRUE.)) THEN
   IERR = 3
ELSE IF (0.0_R8 .NE. SIGMOID(0.0_R8, FW = .FALSE.)) THEN
   IERR = 3
ELSE IF (1.0_R8 .LE. SIGMOID(1.0_R8, FW = .FALSE.)) THEN
   IERR = 3
ELSE IF (-1.0_R8 .GE. SIGMOID(-1.0_R8, FW = .FALSE.)) THEN
   IERR = 3
! Test the HYPERTAN function.
ELSE IF (0.0_R8 .NE. HYPERTAN(0.0_R8, FW = .TRUE.)) THEN
   IERR = 4
ELSE IF (0.0_R8 .GE. HYPERTAN(1.0_R8, FW = .TRUE.)) THEN
   IERR = 4
ELSE IF (0.0_R8 .LE. HYPERTAN(-1.0_R8, FW = .TRUE.)) THEN
   IERR = 4
ELSE IF (0.0_R8 .NE. HYPERTAN(0.0_R8, FW = .FALSE.)) THEN
   IERR = 4
ELSE IF (1.0_R8 .LE. HYPERTAN(1.0_R8, FW = .FALSE.)) THEN
   IERR = 4
ELSE IF (-1.0_R8 .GE. HYPERTAN(-1.0_R8, FW = .FALSE.)) THEN
   IERR = 4
END IF

! Report the results.
IF (IERR .EQ. 0) THEN
   WRITE (*,*) "PASSED: Activation Function Tests"
ELSE IF (IERR .EQ. 1) THEN
   WRITE (*,*) "Error: LINEAR Function Test FAILED"
ELSE IF (IERR .EQ. 2) THEN
   WRITE (*,*) "Error: RELU Function Test FAILED"
ELSE IF (IERR .EQ. 3) THEN
   WRITE (*,*) "Error: SIGMOID Function Test FAILED"
ELSE
   WRITE (*,*) "Error: HYPERTAN Function Test FAILED"
END IF

END PROGRAM TEST_ACTIV
