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
  if $argc < 1
    help attach-board
  else
    if $argc == 2
       set solib-absolute-prefix $arg1
       set solib-search-path $arg1
    end
    target remote $arg0
    handle SIG32 nostop noprint
    set heuristic-fence-post 10000
  end
end
document attach-board
Attach to gdb-server: ip:port [so-path]
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

