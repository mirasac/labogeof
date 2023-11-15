MODULE filters
USE utilities, ONLY : WK => DP, mat_write
IMPLICIT NONE
PRIVATE
PUBLIC :: conv2d
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
    INTEGER :: stride_, padding_, stat_input_, stat_result_, i_row, i_col
    INTEGER :: shape_input(2), shape_input_(2), shape_weight(2), shape_result_(2), tmp_lower(2), tmp_upper(2), i(2)
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
    ! Allocate memory for the actual input matrix, considering padding.
    shape_input = SHAPE(input)
    shape_input_ = shape_input + 2 * padding_
    ALLOCATE(input_(shape_input_(1), shape_input_(2)), STAT=stat_input_)
    ! Allocate memory for the output matrix.
    shape_weight = SHAPE(weight)
    shape_result_ = (shape_input + 2 * padding_ - shape_weight) / stride_ + 1
    ALLOCATE(result_(shape_result_(1), shape_result_(2)), STAT=stat_result_)
    ! Evaluate convolution.
    IF (stat_input_ .GT. 0) THEN
        WRITE(*, *) 'Error in allocating memory for temporary input matrix'
    ELSE IF (stat_result_ .GT. 0) THEN
        WRITE(*, *) 'Error in allocating memory for output matrix'
    ELSE
        ! Initialize the actual input matrix.
        input_ = 0.0_WK
        tmp_lower = padding_ + 1
        tmp_upper = padding_ + shape_input
        input_(tmp_lower(1) : tmp_upper(1), tmp_lower(2) : tmp_upper(2)) = input(:, :)
        ! Perform convolution.
        DO i_col = 1, shape_result_(2), 1
            i(2) = i_col
            DO i_row = 1, shape_result_(1), 1
                i(1) = i_row
                tmp_lower = stride_ * (i - 1) + 1
                tmp_upper = tmp_lower + shape_weight - 1
                result_(i_row, i_col) = SUM(input_(tmp_lower(1) : tmp_upper(1), tmp_lower(2) : tmp_upper(2)) * weight)
            END DO
        END DO
    END IF
    ! Deallocate arrays.
    IF (ALLOCATED(input_)) THEN
        DEALLOCATE(input_)
    END IF
END FUNCTION

END MODULE filters
