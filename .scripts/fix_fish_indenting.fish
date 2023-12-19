#!/usr/bin/env fish

set -l fish_files_from_dir (fd . -H -e fish $argv)

for file in $fish_files_from_dir
    fish_indent -w $file
end
