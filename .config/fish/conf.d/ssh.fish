if test -z "$SSH_ENV"
  set -xg SSH_ENV $HOME/.ssh/environment
end

function _ssh_agent_started --description "Has ssh-agent already started?"
  if begin; test -f $SSH_ENV; and test -z "$SSH_AGENT_PID"; end
    source $SSH_ENV > /dev/null
  end

  if begin; test -z "$SSH_AGENT_PID"; and test -z "$SSH_CONNECTION"; end
    return 1
  end

  ssh-add -l > /dev/null 2>&1
  if test $status -eq 2
    return 1
  end
end

function _ssh_agent_start -d "Start the ssh-agent."
  ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
  chmod 600 $SSH_ENV
  source $SSH_ENV > /dev/null
end

if not _ssh_agent_started
  _ssh_agent_start
  echo "Starting ssh agent..."
end
