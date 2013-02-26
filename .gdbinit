
#------------
#Options
#------------
set history save on
set $AUTO_BREAK_RESTORE = 1
# These make gdb never pause in its output
set height 0
set width 0
#c++ print options
set print pretty on
set print object on
set print static-members on
set print vtbl on
set print demangle on
set demangle-style gnu-v3
set print sevenbit-strings off

source ~/.gdb/init_os_start.gdb
source ~/.gdb/init_host_start.gdb

#stdc++ pretty printers
#python
#import sys
#import os
#sys.path.insert(0, os.path.expanduser("~") + '/.gdb_printers/stdcxx')
#from libstdcxx.v6.printers import register_libstdcxx_printers
#register_libstdcxx_printers (None)
#end

define setup-detect-target
  set $ARM = 0
  set $X86 = 0
  set $X86_64 = 0
  set $MIPS = 0

  set $64BITS = 0

  set logging file /tmp/gdb_info_target
  set logging overwrite on
  set logging redirect on
  set logging on
  set pagination off
  info target
  set pagination on
  set logging off
  set logging redirect off
  set logging overwrite off

  shell ~/.gdb/detect-target.sh
  source /tmp/gdb_target_arch.gdb
  shell rm -f /tmp/gdb_info_target /tmp/gdb_target_arch.gdb
end
document setup-detect-target
Sets up various globals used throughout the GDB macros to provide
architecture-specific support.
end

source ~/.gdb/misc.gdb
source ~/.gdb/stl.gdb
source ~/.gdb/emacs.gdb
source ~/.gdb/data.gdb
source ~/.gdb/signals.gdb
source ~/.gdb/breakpoints.gdb

source ~/.gdb/init_os_end.gdb
source ~/.gdb/init_host_end.gdb

define hook-run
  setup-detect-target
end
define hook-file
  setup-detect-target
end
define hook-core-file
  setup-detect-target
end
