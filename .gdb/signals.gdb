define nosignal
    handle $arg0 nostop noprint pass
end

define nosigpipe
    nosignal SIGPIPE
end

define nosigint
    nosignal SIGINT
end

nosigpipe
nosignal SIGTTIN
