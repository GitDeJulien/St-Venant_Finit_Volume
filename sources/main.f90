program StVenant

    use precision
    use data_reader
    implicit none

    character(len=256) :: filepath
    type(DataType)     :: df

    filepath = '../input/data.toml'

    call store_data(df, filepath)
    call display_toml_file(filepath)




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