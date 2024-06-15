## Log messages to stdio.
# @param severity string # Level of severity [INFO|WARN|ERROR|DEBUG|any]
# @return
notify() {
  local severity="$1"
  local message="$2"
  printf "~~~~~ [%s] %s\n" "$severity" "$message"
}
