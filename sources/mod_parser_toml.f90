!> A module to read TOML data files and 
!> return a ...
!> copyright novembre 2024 Julien TENAUD

module parser_toml

    use precision

    implicit none
    
    public :: ParserTOML

    type, public :: DataType
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
    end type DataType
    
    contains

    subroutine ParserTOML(file_path)

        !Input
        character(len=*), intent(in) :: file_path

        !Local
        integer :: io, stat, eq_pos, end_of_line
        character(len=512) :: msg
        character(len=256) :: line
        character(len=256) :: key, val
        logical :: in_section

        in_section = .false.

        !Opening dummy argument file_path
        open(newunit=io, file=file_path, status="old", action="read")
        do
            read(io, '(A)', iostat=stat, iomsg=msg) line

            !Checking for existance of the file
            if (stat /= 0) then
                print*, " "
                print *, trim(msg)
                STOP
            end if

            ! Trim spaces
            line = adjustl(trim(line))

            ! Skip blank lines and comments
            if (line == "") cycle
            if (line(1:1) == "#") cycle

            ! Detect sections
            if (line(1:1) == "[" .and. index(line, "]") > 0) then
                print*, " "
                print *, adjustl(line(1:len_trim(line)))
                in_section = .true.
                cycle
            end if

            ! Process key-value pairs
            eq_pos = index(line, "=")
            end_of_line = index(line, ";")
            if (eq_pos > 0) then
                key = adjustl(trim(line(1:eq_pos-1)))
                val = adjustl(trim(line(eq_pos+1:end_of_line-1)))
                print *, adjustl(key(1:len_trim(key))), " := ", adjustl(val(1:len_trim(val)))
            end if
        end do

        !Closing file
        close(io)


    end subroutine ParserTOML
    
end module parser_toml