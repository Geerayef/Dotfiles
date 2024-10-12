function fissh --description "Activate SSH agent and activate the given key."
    # eval (ssh-agent -c) && ssh-add $HOME/.ssh/$argv
    ssh-agent -c | sed 's/^echo/#echo/' >$SSH_ENV
    chmod 600 $SSH_ENV
    . $SSH_ENV >/dev/null
    ssh-add $HOME/.ssh/$argv
end
