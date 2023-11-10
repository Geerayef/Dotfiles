function _ssh_agent_started --description "Has ssh-agent already started?"
  if begin; test -f $SSH_ENV; and test -z "$SSH_AGENT_PID"; end
    source $SSH_ENV > /dev/null
    echo "Source ~/.ssh/environment"
  end

  if begin; test -z "$SSH_AGENT_PID"; and test -z "$SSH_CONNECTION"; end
    return 1
  end

  ssh-add -l > /dev/null 2>&1
  if test $status -eq 2
    return 1
  end
end
