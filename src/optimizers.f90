MODULE OPT_MOD

  ! Interface for the gradient.
  INTERFACE
     SUBROUTINE GRAD(DIMS, X, G)
       USE REAL_PRECISION
       INTEGER :: DIMS
       REAL(KIND=R8) :: X(DIMS), G(DIMS)
     END SUBROUTINE GRAD
  END INTERFACE

CONTAINS

  SUBROUTINE SGD(D, GRAD, X, IBUDGET, ALPHA, TAU)
    ! Standard SGD optimization algorithm.
    ! INTEGER (IN) D : Dimension of space.
    ! REAL(KIND=R8), FUNCTION (IN) GRAD : Gradient of objective function 
    ! 	g : R^D --> R.
    ! REAL(KIND=R8) (INOUT) X(D) : On input, starting position in R^D; On 
    !	output, computed minima.
    ! INTEGER, OPTIONAL (IN) IBUDGET : Gradient evaluation budget.
    ! REAL(KIND=R8), OPTIONAL (IN) ALPHA : Step size/learning rate.
    ! REAL(KIND=R8), OPTIONAL (IN) TAU : Rate at which to decay step size
    USE REAL_PRECISION
    IMPLICIT NONE
    ! Parameter list.
    INTEGER, INTENT(IN) :: D
    EXTERNAL :: GRAD
    REAL(KIND=R8), INTENT(INOUT) :: X(D)
    ! Optional parameters.
    INTEGER, OPTIONAL, INTENT(IN) :: IBUDGET
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: ALPHA
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: TAU
    ! Local variables.
    REAL(KIND=R8) :: ALPHAL, TAUL
    INTEGER :: IBUDGETL, T
    REAL(KIND=R8) :: G(D)
    ! Set default values for the optional inputs.
    IBUDGETL = 1000
    ALPHAL = 0.01_R8
    TAUL = 0.99_R8
    ! Get the optional inputs.
    IF(PRESENT(IBUDGET)) THEN
       IBUDGETL = IBUDGET
    END IF
    IF(PRESENT(ALPHA)) THEN
       ALPHAL = ALPHA
    END IF
    IF(PRESENT(TAU)) THEN
       TAUL = TAU
    END IF
    ! Main loop over budget.
    DO T=1, IBUDGETL
       ! Set the current gradient.
       CALL GRAD(D,X,G)
       IF (ALL(G .EQ. 0.0_R8)) RETURN
       ! Take the update step.
       X = X - ALPHAL * G
       ! Decay the step size.
       ALPHAL = ALPHAL * TAUL
    END DO
    RETURN
  END SUBROUTINE SGD

  SUBROUTINE ADAM(D, GRAD, X, IBUDGET, ALPHA, BETA1, BETA2, EPS)
    ! ADaM optimization algorithm as proposed by D. P. Kingma and J. L. Ba.
    ! INTEGER (IN) D : Dimension of space.
    ! REAL(KIND=R8), FUNCTION (IN) GRAD : Gradient of objective function 
    ! 	g : R^D --> R.
    ! REAL(KIND=R8) (INOUT) X(D) : On input, starting position in R^D; On 
    !	output, computed minima.
    ! INTEGER, OPTIONAL (IN) IBUDGET : Gradient evaluation budget.
    ! REAL(KIND=R8), OPTIONAL (IN) ALPHA : Step size.
    ! REAL(KIND=R8), OPTIONAL (IN) BETA1 : Decay rate for first moment.
    ! REAL(KIND=R8), OPTIONAL (IN) BETA2 : Decay rate for second moment.
    ! REAL(KIND=R8), OPTIONAL (IN) EPS : Decay rate for second moment.
    USE REAL_PRECISION
    IMPLICIT NONE
    ! Parameter list.
    INTEGER, INTENT(IN) :: D
    EXTERNAL :: GRAD
    REAL(KIND=R8), INTENT(INOUT) :: X(D)
    ! Optional parameters.
    INTEGER, OPTIONAL, INTENT(IN) :: IBUDGET
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: ALPHA
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: BETA1
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: BETA2
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: EPS
    ! Local variables.
    REAL(KIND=R8) :: ALPHAL, BETA1L, BETA2L, EPSL
    INTEGER :: IBUDGETL, T
    REAL(KIND=R8) :: M(D), V(D), M_HAT(D), V_HAT(D), G(D)
    ! Set default values for the optional parameters.
    IBUDGETL = 1000
    ALPHAL = 0.01_R8
    BETA1L = 0.9_R8
    BETA2L = 0.999_R8
    EPSL = SQRT(EPSILON(0.0_R8))
    ! Get optional inputs.
    IF(PRESENT(IBUDGET)) THEN
       IBUDGETL = IBUDGET
    END IF
    IF(PRESENT(ALPHA)) THEN
       ALPHAL = ALPHA
    END IF
    IF(PRESENT(BETA1)) THEN
       BETA1L = BETA1
    END IF
    IF(PRESENT(BETA2)) THEN
       BETA2L = BETA2
    END IF
    IF(PRESENT(EPS)) THEN
       EPSL = EPS
    END IF
    ! Initialize the first and second moments.
    M = 0.0_R8
    V = 0.0_R8
    ! Main loop over budget.
    DO T=1, IBUDGETL
       ! Get the current gradient.
       CALL GRAD(D,X,G)
       IF (ALL(G .EQ. 0.0_R8)) RETURN
       ! Compute the biased first and second moments.
       M = BETA1L * M + (1.0_R8 - BETA1L) * G
       V = BETA2L * V + (1.0_R8 - BETA2L) * G * G
       ! Correct m and v for bias.
       M_HAT = M / (1.0_R8 - BETA1L ** REAL(T,KIND=R8))
       V_HAT = V / (1.0_R8 - BETA2L ** REAL(T,KIND=R8))
       ! Take a step according to nonbiased adaptive gradient.
       X = X - ALPHAL * M_HAT / (SQRT(V_HAT) + EPSL)
    END DO
    RETURN
  END SUBROUTINE ADAM

  SUBROUTINE ADAGRAD(D, GRAD, X, IBUDGET, ALPHA, EPS)
    ! ADAGRAD optimization algorithm as described by Duchi, Hazan, and Singer.
    ! INTEGER (IN) D : Dimension of space.
    ! REAL(KIND=R8), FUNCTION (IN) GRAD : Gradient of objective function 
    ! 	g : R^D --> R.
    ! REAL(KIND=R8) (INOUT) X(D) : On input, starting position in R^D; On 
    !	output, computed minima.
    ! INTEGER, OPTIONAL (IN) IBUDGET : Gradient evaluation budget.
    ! REAL(KIND=R8), OPTIONAL (IN) ALPHA : Step size in the adjusted space 
    !	parameterized by the matrix norm induced by G.
    ! REAL(KIND=R8), OPTIONAL (IN) EPS : "Fudge factor" for adjusting the 
    !	matrix norm G, avoiding pathological cases where the gradient
    !	disappears in one dimension. Should be a VERY small number 
    !	(default: SQRT(EPSILON)).
    USE REAL_PRECISION
    IMPLICIT NONE
    ! Parameter list.
    INTEGER, INTENT(IN) :: D
    EXTERNAL :: GRAD
    REAL(KIND=R8), INTENT(INOUT) :: X(D)
    ! Optional parameters.
    INTEGER, OPTIONAL, INTENT(IN) :: IBUDGET
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: ALPHA
    REAL(KIND=R8), OPTIONAL, INTENT(IN) :: EPS
    ! Local variables.
    REAL(KIND=R8) :: EPSL, ALPHAL 
    INTEGER :: IBUDGETL, T
    REAL(KIND=R8) :: G(D), STEP(D)
    ! Set default values for the optional parameters.
    IBUDGETL = 1000
    ALPHAL = 0.01_R8
    EPSL = SQRT(EPSILON(0.0_R8))
    ! Get optional inputs.
    IF(PRESENT(IBUDGET)) THEN
       IBUDGETL = IBUDGET
    END IF
    IF(PRESENT(EPS)) THEN
       EPSL = EPS
    END IF
    IF(PRESENT(ALPHA)) THEN
       ALPHAL = ALPHA
    END IF
    ! Initialize the matrix norm.
    G = 0.0_R8
    ! Main loop over budget.
    DO T=1, IBUDGETL
       ! Get the current gradient.
       CALL GRAD(D, X, STEP)
       IF (ALL(G .EQ. 0.0_R8)) RETURN
       ! Update the adaptive norm.
       G = G + STEP ** 2.0_R8
       ! Take the adjusted trust region step according to the current norm.
       STEP = STEP / (EPSL + SQRT(G))
       X = X - (ALPHAL * STEP)
    END DO
    RETURN
  END SUBROUTINE ADAGRAD

END MODULE OPT_MOD
