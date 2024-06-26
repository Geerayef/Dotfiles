set -gx FZF_DEFAULT_OPTS '--scheme=path --cycle --layout=reverse --border=sharp --scroll-off=5 --height=45% --preview-window=wrap,border-sharp --marker="*" --bind=ctrl-f:preview-half-page-down,ctrl-b:preview-half-page-up'
set -x fzf_fd_opts . --hidden
set -x fzf_preview_file_cmd bat
set -x fzf_preview_dir_cmd lt
