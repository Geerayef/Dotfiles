function ssh_agent
    eval (ssh-agent -c) && ssh-add $HOME/.ssh/dev_hub
end
