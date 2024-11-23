program StVenant

    use precision
    use toml_parser
    implicit none

    character(len=256) :: filename, key, value
    integer :: int_value
    real(pr) :: real_value
    logical :: logical_value

    filename = '../input/data.toml'
    key = 'mesh_key'

    call parse_toml(filename, key, value)
    print *, 'String value: ', trim(value)

    call parse_toml(filename, key, int_value)
    print *, 'Integer value', int_value

    call parse_toml(filename, key, real_value)
    print *, 'Real value: ', real_value




    ! call parse_toml(filename, key, real_value, display=.true.)
    ! print *, 'Real value: ', real_value

    ! call parse_toml(filename, key, logical_value, display=.true.)
    ! print *, 'Logical value: ', logical_value


    ! key = 'Lx'

    ! call parse_toml(filename, key, value)
    ! print *, 'String value: ', trim(value)

    ! call parse_toml(filename, key, int_value)
    ! print *, 'Integer value: ', int_value



end program