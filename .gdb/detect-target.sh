#!/bin/bash

GDB_INFO="/tmp/gdb_info_target"
GDB_FILE="/tmp/gdb_target_arch.gdb"
rm -f "$GDB_FILE"

if grep -i 'Evaluate a Python command' "$GDB_INFO" >/dev/null 2>&1; then
    echo "set \$PYTHON = 1" >> $GDB_FILE;
fi

TARGET_DOUBLET=$(grep 'file type' "$GDB_INFO" | sed 's/\.$//g' | cut -d ' ' -f 4 | uniq | tr -d '\n')
case "$TARGET_DOUBLET" in
    *-i386)
        echo "set \$CPU = \"x86\"" >> $GDB_FILE;
        echo "set \$X86 = 1" >> $GDB_FILE;
        ;;
    *-x86-64)
        echo "set \$CPU = \"x86_64\"" >> $GDB_FILE;
        echo "set \$X86_64 = 1" >> $GDB_FILE;
        echo "set \$64BITS = 1" >> $GDB_FILE;
        ;;
    *-arm*)
        echo "set \$CPU = \"arm\"" >> $GDB_FILE;
        echo "set \$ARM = 1" >> $GDB_FILE;
        ;;
    *-*mips*)
        echo "set \$CPU = \"mips\"" >> $GDB_FILE;
        echo "set \$MIPS = 1" >> $GDB_FILE;
        echo "set \$64BITS = 1" >> $GDB_FILE;
        ;;
esac
