#run like "attach 10.2.228.16:6969"
define attach
     #set solib-absolute-prefix /exports/panasonic/am-linux-pf_b_025r/root
     #set solib-search-path /exports/panasonic/am-linux-pf_b_025r/root
    target remote $arg0
    handle SIG32 nostop noprint
    set heuristic-fence-post 10000
end

define thread-lock
    set scheduler-locking $arg0
end

define tbt
    thread apply all bt
end

define make
    shell ubermake.sh
end

