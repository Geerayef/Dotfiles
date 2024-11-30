function fish_indent_all --argument-names path_dir --description "Fix indentation af all fish files found in the given directory."
    switch "$path_dir"
        case -h --help
            printf "Indent all fish files found at the given path (using `fish_indent -w \$file`).\n\n"
            echo "Usage: fish_indent_all <path>"
            echo "Options:"
            echo "        -h, --help        Print this help message"
            echo "Arguments:"
            echo "        <path>        A valid path"
        case ""
            echo "Unknown path: '$path_dir'. Refer `fish_indent_all --help` for usage." >&2 && return 1
        case \*
            for file in (fd . -H -e fish $path_dir)
                fish_indent -w $file
            end
    end
end
