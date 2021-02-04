
MODULE NEURALNET_MOD
! Hompack real precision module for approximately 64-bit arithmetic on all
! known machines.
USE REAL_PRECISION
! Default to private, but make LAYER and NEURALNET objects public.
PRIVATE
PUBLIC :: LAYER, NEURALNET

! Derived data type for constructing a single layer of a neural network.
TYPE LAYER
   INTEGER :: NNODES
   REAL(KIND=R8), ALLOCATABLE :: INPUTS(:), OUTPUTS(:)
   LOGICAL :: BIAS
   PROCEDURE(ACTIVATION), NOPASS, POINTER :: F
CONTAINS
   PROCEDURE, PASS :: FWPROP => FWPROP
   PROCEDURE, PASS :: BWPROP => BWPROP
END TYPE LAYER

! Derived data type for a neural network object.
TYPE NEURALNET
   INTEGER :: NLAYERS
   INTEGER, ALLOCATABLE :: NODESPERLAYER(:)
   REAL(KIND=R8), ALLOCATABLE :: WEIGHTS(:)
   REAL(KIND=R8), ALLOCATABLE :: BIAS(:)
   TYPE(LAYER), ALLOCATABLE :: LAYERS(:)
CONTAINS
   PROCEDURE, PASS :: ADD => ADDLAYER
   PROCEDURE, PASS :: EVALUATE => EVALUATE
   PROCEDURE, PASS :: BACKPROP => BACKPROP
   PROCEDURE, PASS :: TRAIN => TRAIN
END TYPE NEURALNET

! Interface for layer activation functions.
INTERFACE
   FUNCTION ACTIVATION(INPUT, FW)
      USE REAL_PRECISION
      IMPORT LAYER
      REAL(KIND=R8), INTENT(IN) :: INPUT
      LOGICAL, INTENT(IN) :: FW
      REAL(KIND=R8) :: ACTIVATION
   END FUNCTION ACTIVATION
END INTERFACE

! Interface for LAYER constructor.
INTERFACE LAYER
   MODULE PROCEDURE NEWLAYER
END INTERFACE

! Interface for NEURALNET constructor.
INTERFACE NEURALNET
   MODULE PROCEDURE NEWNEURALNET
END INTERFACE

CONTAINS

! --- Procedures for the LAYER class --- !

FUNCTION NEWLAYER(SIZE, IERR, BIAS, ACTIVATION)
! Constructor for LAYER objects.
   IMPLICIT NONE
   ! Parameter list.
   INTEGER, INTENT(IN) :: SIZE
   INTEGER, INTENT(OUT) :: IERR
   TYPE(LAYER) :: NEWLAYER
   LOGICAL, OPTIONAL, INTENT(IN) :: BIAS
   OPTIONAL :: ACTIVATION
   ! Interface for ACTIVATION function parameter.
   INTERFACE
      FUNCTION ACTIVATION(INPUT, FW)
         USE REAL_PRECISION
         REAL(KIND=R8), INTENT(IN) :: INPUT
         LOGICAL, INTENT(IN) :: FW
         REAL(KIND=R8) :: ACTIVATION
      END FUNCTION ACTIVATION
   END INTERFACE
   ! Check whether SIZE is a legal value.
   IF (SIZE < 1) THEN
      IERR = -1; RETURN; END IF
   ! Initialize the values of the new layer object.
   NEWLAYER%NNODES = SIZE
   NEWLAYER%BIAS = .FALSE.
   NEWLAYER%F => LINEAR
   ! Allocate the INPUTS and OUTPUTS arrays.
   ALLOCATE(NEWLAYER%INPUTS(SIZE), NEWLAYER%OUTPUTS(SIZE), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! Set the activation function.
   IF (PRESENT(BIAS)) NEWLAYER%BIAS = BIAS
   IF (PRESENT(ACTIVATION)) NEWLAYER%F => ACTIVATION
   RETURN
END FUNCTION NEWLAYER

SUBROUTINE FWPROP(THIS)
! Propogate function evaluation forward through the activation function.
   IMPLICIT NONE
   ! Calling object.
   CLASS(LAYER), INTENT(INOUT) :: THIS
   ! Loop index.
   INTEGER :: I
   ! Propogate all values.
   DO I = 1, THIS%NNODES
      THIS%OUTPUTS(I) = THIS%F(THIS%INPUTS(I),.TRUE.)
   END DO
   RETURN
END SUBROUTINE FWPROP

SUBROUTINE BWPROP(THIS)
! Propogate gradient evaluation backward through the activation function.
   IMPLICIT NONE
   ! Calling object.
   CLASS(LAYER), INTENT(INOUT) :: THIS
   ! Loop index.
   INTEGER :: I
   ! Propogate all values.
   DO I = 1, THIS%NNODES
      THIS%INPUTS(I) = THIS%F(THIS%OUTPUTS(I),.FALSE.)
   END DO
   RETURN
END SUBROUTINE BWPROP

! --- Procedures for the NEURALNET class --- !

FUNCTION NEWNEURALNET()
! Constructor for NEURALNETWORK objects.
   IMPLICIT NONE
   TYPE(NEURALNET) :: NEWNEURALNET
   NEWNEURALNET%NLAYERS = 0
   RETURN
END FUNCTION NEWNEURALNET

SUBROUTINE ADDLAYER(THIS, NEWLAYER, IERR)
! Add a layer NEWLAYER to the calling NEURALNETWORK object.
   IMPLICIT NONE
   CLASS(NEURALNET), INTENT(INOUT) :: THIS
   CLASS(LAYER), INTENT(IN) :: NEWLAYER
   INTEGER, INTENT(OUT) :: IERR
   ! Local variables.
   INTEGER :: ITMP
   TYPE(LAYER), ALLOCATABLE :: TMP_LAYERS(:)
   INTEGER, ALLOCATABLE :: TMP_NPL(:)
   ! Check for a legal NEWLAYER.
   IF ( .NOT. ALLOCATED(NEWLAYER%INPUTS) .OR. &
        .NOT. ALLOCATED(NEWLAYER%OUTPUTS) ) THEN
      IERR = -2; RETURN; END IF
   ! Save temporary values. We do not save the weights because it is assumed
   ! that the NN has not been trained yet.
   ! If NODESPERLAYER is nonempty, save the current contents.
   IF (ALLOCATED(THIS%NODESPERLAYER)) THEN
      ALLOCATE(TMP_NPL(THIS%NLAYERS), STAT=IERR)
      IF (IERR .NE. 0) RETURN
      TMP_NPL(:) = THIS%NODESPERLAYER(:)
      DEALLOCATE(THIS%NODESPERLAYER, STAT=IERR)
      IF (IERR .NE. 0) RETURN
   END IF
   ! If LAYERS is nonempty, save the current contents.
   IF (ALLOCATED(THIS%LAYERS)) THEN
      ALLOCATE(TMP_LAYERS(THIS%NLAYERS), STAT=IERR)
      IF (IERR .NE. 0) RETURN
      TMP_LAYERS(:) = THIS%LAYERS(:)
      DEALLOCATE(THIS%LAYERS, STAT=IERR)
      IF (IERR .NE. 0) RETURN
   END IF
   ! Increment the number of layers.
   THIS%NLAYERS = THIS%NLAYERS + 1
   ! Reallocate the NODESPERLAYER counter and the LAYERS array.
   ALLOCATE(THIS%NODESPERLAYER(THIS%NLAYERS), THIS%LAYERS(THIS%NLAYERS), &
      & STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! If the NODESPERLAYER and LAYERS arrays were nonempty, recover the tmp
   ! values.
   IF (ALLOCATED(TMP_NPL)) THEN
      THIS%NODESPERLAYER(1:THIS%NLAYERS-1) = TMP_NPL(:)
   END IF
   IF (ALLOCATED(TMP_LAYERS)) THEN
      THIS%LAYERS(1:THIS%NLAYERS-1) = TMP_LAYERS(:)
   END IF
   ! Add the new values.
   THIS%NODESPERLAYER(THIS%NLAYERS) = NEWLAYER%NNODES
   THIS%LAYERS(THIS%NLAYERS) = NEWLAYER
   ! If this is the input layer, there is nothing else to do.
   IF (THIS%NLAYERS .EQ. 1) RETURN
   ! Otherwise, reallocate the flattened WEIGHTS array in accordance with
   ! the new network dimensions.
   IF (ALLOCATED(THIS%WEIGHTS)) DEALLOCATE(THIS%WEIGHTS, STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ITMP = DOT_PRODUCT(THIS%NODESPERLAYER(2:THIS%NLAYERS), &
      & THIS%NODESPERLAYER(1:THIS%NLAYERS-1))
   ALLOCATE(THIS%WEIGHTS(ITMP), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! Check whethr NEWLAYER is biased.
   IF (NEWLAYER%BIAS) THEN
      ! Compute the size of the BIAS array.
      ITMP = NEWLAYER%NNODES
      IF (ALLOCATED(THIS%BIAS)) THEN
         ITMP = ITMP + SIZE(THIS%BIAS,1)
         DEALLOCATE(THIS%BIAS, STAT=IERR)
         IF (IERR .NE. 0) RETURN
      END IF
      ALLOCATE(THIS%BIAS(ITMP), STAT=IERR)
      IF (IERR .NE. 0) RETURN
   END IF
   RETURN
END SUBROUTINE ADDLAYER

FUNCTION EVALUATE(THIS, X, IERR)
! Evaluate a trained NEURALNETWORK object at a REAL vector X(:).
   IMPLICIT NONE
   ! Parameter list.
   CLASS(NEURALNET), INTENT(INOUT) :: THIS
   REAL(KIND=R8), INTENT(IN) :: X(:)
   INTEGER, INTENT(OUT) :: IERR
   REAL(KIND=R8) :: EVALUATE
   ! Local variables.
   INTEGER :: OFFSETB, OFFSETW ! Book keeping variables.
   INTEGER :: I ! Loop indexing variables.
   ! External procedures.
   EXTERNAL :: DGEMV ! Blas subroutine for matrix vector multiplication.
   ! Check for illegal inputs.
   IF (SIZE(X, 1) .NE. THIS%NODESPERLAYER(1)) THEN
      IERR = -2; RETURN; END IF
   IF (THIS%NLAYERS < 2) THEN
      IERR = -1; RETURN; END IF
   IF (THIS%NODESPERLAYER(THIS%NLAYERS) .NE. 1) THEN
      IERR = -1; RETURN; END IF
   ! Initialize the first layer and offsets.
   THIS%LAYERS(1)%INPUTS(:) = X(:)
   CALL THIS%LAYERS(1)%FWPROP()
   OFFSETW = 1; OFFSETB = 1
   ! Propogate values through the hidden layers.
   DO I = 2, THIS%NLAYERS
      ! If this layer is biased, introduce the biases.
      IF (THIS%LAYERS(I)%BIAS) THEN
         THIS%LAYERS(I)%INPUTS(:) = &
            & THIS%BIAS(OFFSETB:OFFSETB+THIS%NODESPERLAYER(I)-1)
         OFFSETB = OFFSETB + THIS%NODESPERLAYER(I)
      ! Otherwise, initialize all the inputs to zero values.
      ELSE
         THIS%LAYERS(I)%INPUTS(:) = 0.0_R8
      END IF
      ! Propogate input from the previous layer through a linear transform.
      CALL DGEMV('N', THIS%NODESPERLAYER(I), THIS%NODESPERLAYER(I-1),   &
               & 1.0_R8, THIS%WEIGHTS(OFFSETW:), THIS%NODESPERLAYER(I), &
               & THIS%LAYERS(I-1)%OUTPUTS, 1, 1.0_R8,                   &
               & THIS%LAYERS(I)%INPUTS, 1)
      ! Propogate through the activation function.
      CALL THIS%LAYERS(I)%FWPROP()
      ! Track the current offset.
      OFFSETW = OFFSETW + (THIS%NODESPERLAYER(I-1) * THIS%NODESPERLAYER(I))
   END DO
   ! Copy outputs from the final layer.
   EVALUATE = THIS%LAYERS(THIS%NLAYERS)%OUTPUTS(1)
   RETURN
END FUNCTION EVALUATE

SUBROUTINE BACKPROP(THIS, WEIGHTS, BIAS, DATA, LOSS, IERR)
! Evaluate the gradient of the LOSS function with respect to WEIGHTS by
! backprojecting through the layers of the calling NEURALNETWORK object.
   IMPLICIT NONE
   ! Parameter list.
   CLASS(NEURALNET), INTENT(INOUT) :: THIS
   REAL(KIND=R8), INTENT(INOUT) :: WEIGHTS(:)
   REAL(KIND=R8), INTENT(INOUT) :: BIAS(:)
   REAL(KIND=R8), INTENT(IN) :: DATA(:,:)
   INTEGER, INTENT(OUT) :: IERR
   ! Interface for LOSS.
   INTERFACE
      FUNCTION LOSS(PRED, ACTUAL)
         USE REAL_PRECISION
         IMPORT LAYER
         REAL(KIND=R8), INTENT(IN) :: PRED, ACTUAL
         REAL(KIND=R8) :: LOSS
      END FUNCTION LOSS
   END INTERFACE
   ! Local variables.
   INTEGER :: D, N, M ! Problem dimensions.
   INTEGER :: I, J ! Loop indexing variables.
   INTEGER :: STARTW, FINISHW ! Book keeping variables for weights.
   INTEGER :: STARTB, FINISHB ! Book keeping variables for biases.
   REAL(KIND=R8) :: RTMP ! Temporary variable for storing loss.
   ! External procedures.
   EXTERNAL :: DGEMV ! Blas subroutine for matrix vector multiplication.
   EXTERNAL :: DGER ! Blas subroutine for applying a rank 1 update.
   ! Get sizes.
   D = SIZE(DATA, 1) - 1
   N = SIZE(WEIGHTS, 1)
   M = SIZE(BIAS, 1)
   ! Check for illegal sizes.
   IF (THIS%NLAYERS < 2) THEN
      IERR = -1; RETURN; END IF
   IF (THIS%NODESPERLAYER(THIS%NLAYERS) .NE. 1) THEN
      IERR = -1; RETURN; END IF
   IF (N .NE. SIZE(THIS%WEIGHTS, 1)) THEN
      IERR = -2; RETURN; END IF
   IF (ALLOCATED(THIS%BIAS) .AND. M .NE. SIZE(THIS%BIAS, 1)) THEN
      IERR = -3; RETURN; END IF
   IF (D .NE. THIS%NODESPERLAYER(1)) THEN
      IERR = -4; RETURN; END IF
   ! Set the weights and biases and initialize the gradients.
   THIS%WEIGHTS(:) = WEIGHTS(:)
   IF (ALLOCATED(THIS%BIAS)) THEN
      THIS%BIAS(:) = BIAS(:)
      BIAS(:) = 0.0_R8
   END IF
   WEIGHTS(:) = 0.0_R8
   ! Loop over all points in the data set.
   DO I = 1, SIZE(DATA, 2)
      ! There is just one node in the output layer. The derivative of the
      ! loss function with respect to its output is the derivative of the
      ! loss function with respect to the output of the current model.
      RTMP = LOSS(THIS%EVALUATE(DATA(1:D,I),IERR), DATA(D+1,I)) &
         & / SIZE(DATA,2)
      IF (IERR .NE. 0) RETURN
      THIS%LAYERS(THIS%NLAYERS)%OUTPUTS(1) = RTMP
      ! Backproject through the final activation function.
      CALL THIS%LAYERS(THIS%NLAYERS)%BWPROP()
      ! Track the memory offsets.
      FINISHW = N
      STARTW = N - THIS%NODESPERLAYER(THIS%NLAYERS-1) + 1
      FINISHB = M
      ! Evaluate the partial derivative with respect to the weights using
      ! the output from the previous layer.
      WEIGHTS(STARTW:FINISHW) = WEIGHTS(STARTW:FINISHW) + &
         & THIS%LAYERS(THIS%NLAYERS-1)%OUTPUTS(:) * &
         & THIS%LAYERS(THIS%NLAYERS)%INPUTS(1)
      IF (THIS%LAYERS(THIS%NLAYERS)%BIAS) THEN
         BIAS(M) = BIAS(M) + THIS%LAYERS(THIS%NLAYERS)%INPUTS(1)
         FINISHB = FINISHB - 1
      END IF
      ! Backproject the gradient through the rest of the layers.
      DO J = THIS%NLAYERS-1, 2, -1
         ! Compute the gradient of the loss function with respect to the
         ! input to the current layer.
         THIS%LAYERS(J)%OUTPUTS(:) = 0.0_R8
         CALL DGEMV('T', THIS%NODESPERLAYER(J+1), THIS%NODESPERLAYER(J), &
               & 1.0_R8, THIS%WEIGHTS(STARTW:FINISHW),                   &
               & THIS%NODESPERLAYER(J+1), THIS%LAYERS(J+1)%INPUTS, 1,    &
               & 1.0_R8, THIS%LAYERS(J)%OUTPUTS, 1)
         CALL THIS%LAYERS(J)%BWPROP()
         ! Update the current offsets.
         FINISHW = STARTW - 1
         STARTW = STARTW - THIS%NODESPERLAYER(J) * THIS%NODESPERLAYER(J-1)
         ! Apply the rank 1 update to the current weights gradient.
         CALL DGER(THIS%NODESPERLAYER(J), THIS%NODESPERLAYER(J-1), 1.0_R8, &
               & THIS%LAYERS(J)%INPUTS(:), 1, THIS%LAYERS(J-1)%OUTPUTS, 1, &
               & WEIGHTS(STARTW:FINISHW), THIS%NODESPERLAYER(J))
         ! Check if this layer is biased.
         IF (THIS%LAYERS(J)%BIAS) THEN
            ! Get the new starting offset.
            STARTB = FINISHB - THIS%NODESPERLAYER(J) + 1
            ! Add directly to the biases.
            BIAS(STARTB:FINISHB) = BIAS(STARTB:FINISHB) &
               & + THIS%LAYERS(J)%INPUTS(:)
            ! Update the new ending offset.
            FINISHB = STARTB - 1
         END IF
      END DO
   END DO
END SUBROUTINE BACKPROP

SUBROUTINE TRAIN(THIS, DATA, IERR, LOSS, REG, LAMBDA)
! Train a neural network object using backpropogation + SGD.
   USE OPT_MOD
   IMPLICIT NONE
   ! Parameter list.
   CLASS(NEURALNET), INTENT(INOUT) :: THIS
   REAL(KIND=R8), INTENT(IN) :: DATA(:,:)
   INTEGER, INTENT(OUT) :: IERR
   REAL(KIND=R8), OPTIONAL, INTENT(IN) :: LAMBDA
   OPTIONAL :: LOSS, REG
   ! Interfaces for optionals LOSS and REG.
   INTERFACE
      FUNCTION LOSS(PRED, ACTUAL)
         USE REAL_PRECISION
         REAL(KIND=R8), INTENT(IN) :: PRED, ACTUAL
         REAL(KIND=R8) :: LOSS
      END FUNCTION LOSS
      FUNCTION REG(WEIGHT)
         USE REAL_PRECISION
         REAL(KIND=R8), INTENT(IN) :: WEIGHT
         REAL(KIND=R8) :: REG
      END FUNCTION REG
   END INTERFACE
   ! Local variables.
   INTEGER :: NUMWEIGHTS, NUMBIAS
   REAL(KIND=R8), ALLOCATABLE :: CURR_WEIGHTS(:)
   REAL(KIND=R8), ALLOCATABLE :: CURR_BIAS(:)
   REAL(KIND=R8), ALLOCATABLE :: CURR_X(:)
   REAL(KIND=R8) :: LAMBDAL
   PROCEDURE(LOSS), POINTER :: LOSSL => MSE
   INTEGER :: I
   ! External procedures.
   REAL(KIND=R8), EXTERNAL :: DNRM2 ! Blas subroutine for Euclidean distance.
   ! Get problem dimensions.
   NUMWEIGHTS = SIZE(THIS%WEIGHTS,1)
   IF (ALLOCATED(THIS%BIAS)) THEN
      NUMBIAS = SIZE(THIS%BIAS,1)
      ALLOCATE(CURR_BIAS(NUMBIAS), STAT=IERR)
      IF (IERR .NE. 0) RETURN
   ELSE
      NUMBIAS = 0
   END IF
   ! Allocate temporary variables for tracking progress.
   ALLOCATE(CURR_WEIGHTS(NUMWEIGHTS), CURR_X(NUMWEIGHTS+NUMBIAS), STAT=IERR)
   IF (IERR .NE. 0) RETURN
   ! Set optional variables and procedures, if present.
   LAMBDAL = 0.5_R8
   IF (PRESENT(LAMBDA)) LAMBDAL = LAMBDA
   IF (PRESENT(LOSS)) LOSSL => LOSS
   ! Randomly initialize weights in [-1, 1].
   CALL RANDOM_NUMBER(CURR_X(:))
   CURR_X(:) = 2.0_R8 * (CURR_X(:) - 0.5_R8)
CURR_X(NUMWEIGHTS+1:NUMWEIGHTS+NUMBIAS) = 0.0_R8
   ! Solve with stochastic gradient descent.
   CALL SGD(NUMWEIGHTS+NUMBIAS, BACKPROP_GRAD, CURR_X, &
          & IBUDGET=10000, TAU=1.0_R8)
   RETURN
CONTAINS
   SUBROUTINE BACKPROP_GRAD(D, X, G)
   ! Internal subroutine for passing the gradient to SGD.
      INTEGER, INTENT(IN) :: D
      REAL(KIND=R8), INTENT(IN) :: X(D)
      REAL(KIND=R8), INTENT(OUT) :: G(D)
      CURR_WEIGHTS(:) = X(1:NUMWEIGHTS)
      IF (ALLOCATED(CURR_BIAS)) THEN
         CURR_BIAS(:) = X(NUMWEIGHTS+1:NUMWEIGHTS+NUMBIAS); END IF
      CALL THIS%BACKPROP(CURR_WEIGHTS, CURR_BIAS, DATA, LOSSL, IERR)
      IF (IERR .NE. 0) THEN; G(:) = 0.0_R8; RETURN; END IF
      G(1:NUMWEIGHTS) = CURR_WEIGHTS(:)
      ! Add regularization terms.
      IF (PRESENT(REG)) THEN
         DO I = 1, NUMWEIGHTS
            G(I) = G(I) + LAMBDAL * REG(X(I))
         END DO
      END IF
      ! Update the biases if present.
      IF (ALLOCATED(CURR_BIAS)) THEN
         G(NUMWEIGHTS+1:NUMWEIGHTS+NUMBIAS) = CURR_BIAS(:); END IF
      !G(:) = G(:) / DNRM2(D, G, 1) ! uncomment for gradient normalization
   END SUBROUTINE BACKPROP_GRAD
END SUBROUTINE TRAIN

! --- Activation functions --- !

FUNCTION LINEAR(INPUT, FW)
! Definition of the linear activation function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: INPUT
   LOGICAL, INTENT(IN) :: FW
   REAL(KIND=R8) :: LINEAR
   ! The parameter FW is not used for the linear function.
   LINEAR = INPUT
   RETURN
END FUNCTION LINEAR

FUNCTION RELU(INPUT, FW)
! Definition of the Rectified Linear Unit activation function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: INPUT
   LOGICAL, INTENT(IN) :: FW
   REAL(KIND=R8) :: RELU
   ! The parameter FW is not used for the RELU function.
   RELU = INPUT
   RELU = MAX(0.0_R8, RELU)
   RETURN
END FUNCTION RELU

FUNCTION SIGMOID(INPUT, FW)
! Definition of the sigmoidal activation function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: INPUT
   LOGICAL, INTENT(IN) :: FW
   REAL(KIND=R8) :: SIGMOID
   ! First compute the sigmoid function.
   SIGMOID = 1.0_R8 / (1.0_R8 + EXP(-INPUT))
   ! If backpropogating, then compute the derivative * INPUT.
   IF (.NOT. FW) THEN
      SIGMOID = INPUT * (SIGMOID * (1.0_R8 - SIGMOID))
   END IF
   RETURN
END FUNCTION SIGMOID

FUNCTION HYPERTAN(INPUT, FW)
! Definition of the hyperpolic tangent activation function.
! Note that TANH is already a Fortran intrinsic.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: INPUT
   LOGICAL, INTENT(IN) :: FW
   REAL(KIND=R8) :: HYPERTAN
   ! First compute the sigmoid function.
   HYPERTAN = TANH(INPUT)
   ! If backpropogating, then compute the derivative * INPUT.
   IF (.NOT. FW) THEN
      HYPERTAN = INPUT * (1.0_R8 - (HYPERTAN ** 2.0_R8))
   END IF
   RETURN
END FUNCTION HYPERTAN

! --- Loss functions --- !

FUNCTION MSE(PRED, ACTUAL)
! Definition of the squared error (L2) loss function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: PRED, ACTUAL
   REAL(KIND=R8) :: MSE
   ! Return the gradient of the loss function.
   MSE = 2.0_R8 * (PRED - ACTUAL)
   RETURN
END FUNCTION MSE

FUNCTION MAE(PRED, ACTUAL)
! Definition of the absolute error (L1) loss function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: PRED, ACTUAL
   REAL(KIND=R8) :: MAE
   MAE = SIGN(1.0_R8, PRED - ACTUAL)
END FUNCTION

! --- Regularization functions --- !

FUNCTION L2(WEIGHT)
! Definition of the squared error (L2) regularization function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: WEIGHT
   REAL(KIND=R8) :: L2
   ! Return the gradient of the regularization function: SUM(W**2)/2.
   L2 = WEIGHT
   RETURN
END FUNCTION L2

FUNCTION L1(WEIGHT)
! Definition of the absolute error (L1) regularization function.
   IMPLICIT NONE
   ! Parameter list.
   REAL(KIND=R8), INTENT(IN) :: WEIGHT
   REAL(KIND=R8) :: L1
   ! Return the gradient of the regularization function: SUM(W**2)/2.
   L1 = SIGN(1.0_R8, WEIGHT)
   RETURN
END FUNCTION L1

END MODULE NEURALNET_MOD
