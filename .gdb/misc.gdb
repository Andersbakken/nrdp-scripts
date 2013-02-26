define main
 set $SHOW_CONTEXT = 1
 set $SHOW_NEST_INSN=0
 tbreak main
 run
end
document main
Run program; break on main()
end

define attach-board
  if $argc != 1
    help attach-board
  else
    #set solib-absolute-prefix /exports/panasonic/am-linux-pf_b_025r/root
    #set solib-search-path /exports/panasonic/am-linux-pf_b_025r/root
    target remote $arg0
    handle SIG32 nostop noprint
    set heuristic-fence-post 10000
  end
end
document attach-board
Attach to gdb-server: ip:port
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

