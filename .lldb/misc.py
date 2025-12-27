#!/usr/bin/python

import lldb
import os

def source_if_exists(debugger, command, result, internal_dict):
  path = os.path.expanduser(command.strip())
  if os.path.exists(path):
    debugger.HandleCommand('command source %s' % path)

def __lldb_init_module (debugger, dict):
  debugger.HandleCommand('command script add -f misc.source_if_exists source_if_exists')
  debugger.HandleCommand('command alias tbt t a a bt')
