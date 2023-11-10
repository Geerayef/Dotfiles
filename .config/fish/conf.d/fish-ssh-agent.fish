if test -z "$SSH_ENV"
  set -gx SSH_ENV $HOME/.ssh/environment
end

if not _ssh_agent_started
  _ssh_agent_start
end
