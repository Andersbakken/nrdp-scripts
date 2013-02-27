# Save bookmarks
define bsave
    shell rm -f .gdb_brestore.txt
    set logging file .gdb_brestore.txt
    set logging on
    info break
    set logging off
    # reformat on-the-fly to a valid gdb command file
    shell perl -n -e 'print "break $1\n" if /^\d+.+?(\S+)$/g' .gdb_brestore.txt > .gdb_brestore
end
document bsave
  store actual breakpoints
end

define brestore
  source .gdb_brestore
end
document brestore
  restore breakpoints saved by bsave
end

define bclear
  shell rm -f .gdb_brestore
end
document brestore
  clear breakpoints saved by bsave
end

define brestore-once
  if ! $brestore_once_flag
    set $brestore_once_flag = 1
    brestore
  end
end

