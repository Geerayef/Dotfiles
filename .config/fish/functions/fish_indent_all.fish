function fish_indent_all --description "Fix indenting in all fish files found in the given directory."
    set -l fish_files_from_dir (fd . -H -e fish $argv)
    for file in $fish_files_from_dir
        fish_indent -w $file
    end
end
