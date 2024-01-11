function hlp --description "Redirect --help calls to pager by default."
    $argv --help &| nvimpager -a
end
