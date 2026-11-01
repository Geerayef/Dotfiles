set -gx FZF_DEFAULT_OPTS '--style=minimal --scheme=path --cycle --layout=reverse --scroll-off=4 --preview-window=wrap-word --bind=ctrl-f:preview-half-page-down,ctrl-b:preview-half-page-up'
set -x fzf_fd_opts -H
set -x fzf_preview_file_cmd bat
set -x fzf_preview_dir_cmd lt
