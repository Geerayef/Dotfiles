function __ssh_agent_started --description "Checks if ssh agent is running."
    if test -z "$SSH_AGENT_PID"
        return 1
    end
    return 0
end

function __ssh_agent_start --description "Start the ssh-agent."
    if not test -f $SSH_ENV
        ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
        chmod 600 $SSH_ENV
        source $SSH_ENV
    end
end

function fish_ssh_agent
    if test -z "$SSH_ENV"
        return 1
    end
    set -gx SSH_ENV "$HOME/.ssh/environment"
    if not __ssh_agent_started
        __ssh_agent_start
    end
end

