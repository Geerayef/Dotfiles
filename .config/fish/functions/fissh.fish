function fissh --description "Activate SSH agent and activate the given key."
    eval (ssh-agent -c) && ssh-add $HOME/.ssh/$argv
end
