program StVenant

    use precision
    use data_reader
    implicit none

    character(len=256) :: filepath
    type(DataType)     :: df

    filepath = 'input/data.toml'

    call store_data(df, filepath)
    call display_toml_file(filepath)

end program