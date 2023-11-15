MODULE filters
IMPLICIT NONE
USE utilities, ONLY : WK => DP
PUBLIC
CONTAINS

! Evaluate convolution of rank 2 array with same extensions (i.e. square
! matrix). User needs to deallocate explicitally the returned matrix.
! Arguments:
!   input
!     Rank 2 real array.
!   weight
!     Rank 2 real array with extensions equal or less than sum of
!     extensions of input and padding.
! Optional arguments:
!   stride
!     Scalar integer, number of elements to skip at each movement of the
!     kernel, default 1.
!   padding
!     Scalar integer, number of elements to add at each border (their
!     value is 0), default 0.
FUNCTION conv2d(input, weight, stride, padding) RESULT(result_)
    ! Dummy arguments declaration.
    REAL(KIND=WK), INTENT(IN) :: input(:, :)
    REAL(KIND=WK), INTENT(IN) :: weight(:, :)
    INTEGER, INTENT(IN), OPTIONAL :: stride
    INTEGER, INTENT(IN), OPTIONAL :: padding
    ! Variables declaration.
    INTEGER :: stride_, padding_, stat_input_, stat_result_
    INTEGER :: shape_input(2), shape_weight(2)
    REAL(KIND=WK), ALLOCATABLE:: input_(:, :), result_(:, :)
    ! Prepare optional and result variables.
    IF (PRESENT(stride)) THEN
        stride_ = stride
    ELSE
        stride_ = 1
    END IF
    IF (PRESENT(padding)) THEN
        padding_ = padding
    ELSE
        padding_ = 0
    END IF
    shape_input = SHAPE(input)
    shape_weight = SHAPE(weight)
    ALLOCATE(input_(shape_input + padding_), STAT=stat_input_)
    ALLOCATE(result_((FLOOR(shape_input + 2 * padding_ - shape_weight) / stride + 1)), STAT=stat_result_)
    IF (stat_input_ .GT. 0) THEN
        WRITE(*, *) 'Error in allocating memory for temporary input matrix'
    ELSE IF (stat_result_ .GT. 0) THEN
        WRITE(*, *) 'Error in allocating memory for output matrix'
    ELSE
        ! MC continue with loop.
        WRITE(*, *) result_ ! MC debug.
    END IF
    ! Deallocate arrays.
    IF (ALLOCATED(input_)) THEN
        DEALLOCATE(input_)
    END IF
END FUNCTION

END MODULE filters
