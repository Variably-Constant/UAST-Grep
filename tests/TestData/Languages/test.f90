! Fortran Test File for UAST-Grep
! Tests: modules, subroutines, functions, arrays, control flow

program uast_grep_test
    use test_module
    use iso_fortran_env
    implicit none

    ! Constants
    integer, parameter :: MAX_ITEMS = 100
    character(len=*), parameter :: DEFAULT_NAME = "UAST-Grep"
    real(real64), parameter :: PI = 3.14159265358979323846_real64

    ! Variable declarations
    integer :: i, n, counter
    real(real64) :: x, y, result
    character(len=50) :: message
    logical :: is_active
    integer, dimension(10) :: int_array
    real(real64), dimension(:), allocatable :: dynamic_array
    type(person) :: sample_person
    type(processor) :: data_processor

    ! Initialize
    print *, "UAST-Grep Fortran Test"
    print *, "========================"

    ! Call subroutines
    call initialize_processor(data_processor, "Main")
    call test_arrays()
    call test_control_flow()
    call test_io()

    ! Function calls
    result = calculate_sum(5.0_real64, 3.0_real64)
    print *, "Sum: ", result

    result = transform(42.0_real64)
    print *, "Transformed: ", result

    ! Derived type usage
    sample_person%name = "Alice"
    sample_person%age = 30
    sample_person%email = "alice@example.com"
    sample_person%active = .true.

    if (is_adult(sample_person)) then
        print *, trim(sample_person%name), " is an adult"
    end if

    print *, "Test complete"

contains

    ! Internal procedure
    subroutine test_arrays()
        integer :: i
        real(real64), dimension(5) :: arr
        real(real64), dimension(3, 3) :: matrix
        real(real64) :: sum_val

        ! Array initialization
        arr = [1.0, 2.0, 3.0, 4.0, 5.0]

        ! Array operations
        arr = arr * 2.0  ! Vectorized operation

        ! Implied do loop
        arr = [(real(i, real64), i = 1, 5)]

        ! Sum
        sum_val = sum(arr)
        print *, "Array sum: ", sum_val

        ! Matrix operations
        matrix = reshape([1,2,3,4,5,6,7,8,9], [3,3])
        print *, "Matrix trace: ", matrix(1,1) + matrix(2,2) + matrix(3,3)

        ! Array slicing
        print *, "First 3 elements: ", arr(1:3)
        print *, "Every other element: ", arr(1:5:2)

        ! Where construct
        where (arr > 3.0)
            arr = arr * 10.0
        elsewhere
            arr = 0.0
        end where

        ! Forall construct
        forall (i = 1:3, j = 1:3)
            matrix(i, j) = real(i + j, real64)
        end forall
    end subroutine test_arrays

    subroutine test_control_flow()
        integer :: i, status
        real(real64) :: value

        ! If-then-else
        value = 42.0
        if (value > 0.0) then
            print *, "Positive"
        else if (value < 0.0) then
            print *, "Negative"
        else
            print *, "Zero"
        end if

        ! Do loop
        do i = 1, 10
            print *, "Iteration: ", i
        end do

        ! Do while
        counter = 0
        do while (counter < 5)
            counter = counter + 1
            print *, "Counter: ", counter
        end do

        ! Do with exit
        counter = 0
        do
            counter = counter + 1
            if (counter >= 10) exit
            if (mod(counter, 2) == 0) cycle
            print *, "Odd: ", counter
        end do

        ! Select case
        status = 200
        select case (status)
            case (200)
                message = "OK"
            case (404)
                message = "Not Found"
            case (500:599)
                message = "Server Error"
            case default
                message = "Unknown"
        end select
        print *, "Status: ", trim(message)

        ! Associate construct
        associate (val => sample_person%age)
            print *, "Age via associate: ", val
        end associate

        ! Block construct
        block
            integer :: local_var
            local_var = 100
            print *, "Block local: ", local_var
        end block
    end subroutine test_control_flow

    subroutine test_io()
        integer :: unit_num, io_status
        character(len=100) :: line
        character(len=20) :: filename

        filename = "test_output.txt"

        ! Write to file
        open(newunit=unit_num, file=filename, status='replace', &
             action='write', iostat=io_status)
        if (io_status /= 0) then
            print *, "Error opening file for writing"
            return
        end if

        write(unit_num, '(A)') "UAST-Grep Test Output"
        write(unit_num, '(A,I0)') "Max items: ", MAX_ITEMS
        write(unit_num, '(A,F10.6)') "Pi: ", PI

        close(unit_num)

        ! Read from file
        open(newunit=unit_num, file=filename, status='old', &
             action='read', iostat=io_status)
        if (io_status == 0) then
            do
                read(unit_num, '(A)', iostat=io_status) line
                if (io_status /= 0) exit
                print *, "Read: ", trim(line)
            end do
            close(unit_num)
        end if
    end subroutine test_io

end program uast_grep_test

! Module definition
module test_module
    use iso_fortran_env
    implicit none

    ! Derived type
    type :: person
        character(len=50) :: name
        integer :: age
        character(len=100) :: email
        logical :: active
    end type person

    ! Abstract type
    type, abstract :: base_processor
        character(len=50) :: name
        integer :: count = 0
    contains
        procedure(process_interface), deferred :: process
        procedure :: log => processor_log
    end type base_processor

    ! Concrete type
    type, extends(base_processor) :: processor
        real(real64), dimension(:), allocatable :: cache
    contains
        procedure :: process => processor_process
    end type processor

    ! Interface for abstract procedure
    abstract interface
        subroutine process_interface(self, items, results)
            import :: base_processor, real64
            class(base_processor), intent(inout) :: self
            real(real64), dimension(:), intent(in) :: items
            real(real64), dimension(:), allocatable, intent(out) :: results
        end subroutine process_interface
    end interface

    ! Generic interface
    interface calculate_sum
        module procedure sum_real, sum_int
    end interface calculate_sum

contains

    ! Initialize processor
    subroutine initialize_processor(proc, name)
        type(processor), intent(out) :: proc
        character(len=*), intent(in) :: name
        proc%name = name
        proc%count = 0
    end subroutine initialize_processor

    ! Process implementation
    subroutine processor_process(self, items, results)
        class(processor), intent(inout) :: self
        real(real64), dimension(:), intent(in) :: items
        real(real64), dimension(:), allocatable, intent(out) :: results
        integer :: i

        allocate(results(size(items)))
        do i = 1, size(items)
            results(i) = transform(items(i))
        end do
        self%count = size(items)
    end subroutine processor_process

    ! Log message
    subroutine processor_log(self, message)
        class(base_processor), intent(in) :: self
        character(len=*), intent(in) :: message
        print *, "[", trim(self%name), "] ", message
    end subroutine processor_log

    ! Transform function
    pure function transform(value) result(res)
        real(real64), intent(in) :: value
        real(real64) :: res
        if (value > 0.0) then
            res = value * 2.0
        else if (value < 0.0) then
            res = -value
        else
            res = 0.0
        end if
    end function transform

    ! Sum functions
    pure function sum_real(a, b) result(res)
        real(real64), intent(in) :: a, b
        real(real64) :: res
        res = a + b
    end function sum_real

    pure function sum_int(a, b) result(res)
        integer, intent(in) :: a, b
        integer :: res
        res = a + b
    end function sum_int

    ! Check if person is adult
    pure function is_adult(p) result(res)
        type(person), intent(in) :: p
        logical :: res
        res = p%age >= 18
    end function is_adult

    ! Elemental function
    elemental function double(x) result(res)
        real(real64), intent(in) :: x
        real(real64) :: res
        res = x * 2.0
    end function double

    ! Recursive function
    recursive function factorial(n) result(res)
        integer, intent(in) :: n
        integer :: res
        if (n <= 1) then
            res = 1
        else
            res = n * factorial(n - 1)
        end if
    end function factorial

end module test_module
