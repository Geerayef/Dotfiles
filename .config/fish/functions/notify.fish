function notify --description "Notify with messages to stdio."
    set -l severity (string upper "$argv[1]")
    set -l message $argv[2]
    printf "    [ %s ] | %s\n" $severity $message
end
