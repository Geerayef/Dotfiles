function ssh_agent --description "Activate SSH agent with given key"
    eval (ssh-agent -c) && ssh-add $HOME/.ssh/$argv
end
