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
