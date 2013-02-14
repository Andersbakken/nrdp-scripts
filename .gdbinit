
set history save on

#stdc++ pretty printers
#python
#import sys
#import os
#sys.path.insert(0, os.path.expanduser("~") + '/.gdb_printers/stdcxx')
#from libstdcxx.v6.printers import register_libstdcxx_printers
#register_libstdcxx_printers (None)
#end

#run like "bcom 10.2.228.16:6969"
define bcom
    target remote $arg0
    handle SIG32 nostop noprint
    set heuristic-fence-post 10000
end

#run like "panaic 10.2.228.16:6969"
define panaic
  #set solib-absolute-prefix /exports/panasonic/am-linux-pf_b_025r/root
  #set solib-search-path /exports/panasonic/am-linux-pf_b_025r/root
  target remote $arg0
  handle SIG32 nostop noprint
end

define dump-layers
    set environment QT_WEBKIT_LOG rendering
    set environment QT_WEBKIT_SAVE_LAYERS 1
end
define no-dump-layers
    set environment QT_WEBKIT_LOG
    set environment QT_WEBKIT_SAVE_LAYERS
end


define thread-lock
    set scheduler-locking $arg0
end

define nosignal
    handle $arg0 nostop noprint pass
end

define nosigpipe
    nosignal SIGPIPE
end

define tbt
    thread apply all bt
end

define make
    shell ubermake.sh
end

define econtinue
    shell echo tbreak $(emacsclient -e '(sam-what-file)') >/tmp/foo.gdb
    source /tmp/foo.gdb
    continue
end
define ebreak
    shell echo break $(emacsclient -e '(sam-what-file)') >/tmp/foo.gdb
    source /tmp/foo.gdb
end
define ecd
    shell emacsclient -n $PWD
end
define emake
    shell "emacsedit.sh" -m -n
end
define rez
   if $argc == 0
      edit
   else
      edit $arg0
   end
end

nosigpipe
nosignal SIGTTIN