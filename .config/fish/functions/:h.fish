function :h --description "Redirect --help calls to the system pager by default."
    $argv --help &| $PAGER
end
