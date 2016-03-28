
#------------
#Options
#------------
set history save on
set confirm off
set $AUTO_BREAK_RESTORE = 1
# These make gdb never pause in its output
set height 0
set width 0
#c++ print options
set print pretty on
set print object on
set print static-members off
set print vtbl on
set print demangle on
set demangle-style gnu-v3
set print sevenbit-strings off
set print asm-demangle on

source ~/.gdb/init_os_start.gdb
source ~/.gdb/init_host_start.gdb

define setup-detect-target
  set $ARM = 0
  set $X86 = 0
  set $X86_64 = 0
  set $MIPS = 0
  set $PYTHON = 0
  set $CPU = 0

  set $64BITS = 0

  shell echo "import gdb" > /tmp/gdb_has_python.py
  set logging file /tmp/gdb_info_target
  set logging overwrite on
  set logging redirect on
  set logging on
  set pagination off
  info target
  help all
  #source /tmp/gdb_has_python.py
  set pagination on
  set logging off
  set logging redirect off
  set logging overwrite off

  shell ~/.gdb/detect-target.sh
  source /tmp/gdb_target_arch.gdb
  shell rm -f /tmp/gdb_info_target /tmp/gdb_target_arch.gdb /tmp/gdb_has_python.py
  #if ($PYTHON == 1)
  #   source ~/.gdb/python.gdb
  #end
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

define hook-delete
  bclear
end
define hookpost-break
  brestore
end
define hook-run
  setup-detect-target
  brestore
end
define hook-file
  setup-detect-target
end
define hook-core-file
  setup-detect-target
end
define hook-quit
   if $brestore_once_flag
     bsave
   end
end
