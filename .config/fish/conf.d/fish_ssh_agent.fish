function __ssh_agent_started --description "Checks if ssh agent is running."
    if test -f $SSH_ENV
        and test -z "$SSH_AGENT_PID"
        source $SSH_ENV > /dev/null
        echo "Source ~/.ssh/environment"
    end

    if test -z "$SSH_AGENT_PID"
        or test -z "$SSH_CONNECTION"
        return 1
    end

    /usr/bin/ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep -q ssh-agent
    # pgrep ssh-agent
    return $status
    # ssh-add -l > /dev/null 2>&1
    # if test $status -eq 2
    #     return 1
    # end
end

function __ssh_agent_start --description "Start the ssh-agent."
    ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
    chmod 600 $SSH_ENV
    source $SSH_ENV > /dev/null
    echo "Starting ssh agent..."
end

function fish_ssh_agent
    if test -z "$SSH_ENV"
        set -gx SSH_ENV "$HOME/.ssh/environment"
    end

    if not __ssh_agent_started
        __ssh_agent_start
    end
end

