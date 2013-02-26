define nosignal
    handle $arg0 nostop noprint pass
end

define nosigpipe
    nosignal SIGPIPE
end

nosigpipe
nosignal SIGTTIN
