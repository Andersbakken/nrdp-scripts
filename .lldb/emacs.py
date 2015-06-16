#!/usr/bin/python

import lldb
import os

def emake(debugger, command, result, internal_dict):
  os.system("emacsedit.sh -m -n")

def rez(debugger, command, result, internal_dict):
  print "%s" % debugger.frame.file.fullpath;

def ecd(debugger, command, result, internal_dict):
  os.system("emacsedit.sh -n $PWD")

def ebreak(debugger, command, result, internal_dict):
  pipe = os.popen("emacsedit.sh -n -e '(sam-what-file)'", 'rb')
  result = pipe.read().rstrip()
  pipe.close()
  debugger.HandleCommand("br %s" % result)

def __lldb_init_module (debugger, dict):
  debugger.HandleCommand('command script add -f emacs.rez rez')
  debugger.HandleCommand('command script add -f emacs.ebreak ebreak')
  debugger.HandleCommand('command script add -f emacs.emake emake')
  debugger.HandleCommand('command script add -f emacs.ecd ecd')
