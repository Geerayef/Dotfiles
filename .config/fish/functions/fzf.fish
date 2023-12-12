function fzf --wraps=fzf --description="Use fzf-tmux in tmux"
    if set --query TMUX
        fzf-tmux $argv
    else
        command fzf $argv
    end
end
