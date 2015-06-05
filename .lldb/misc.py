#!/usr/bin/python

import lldb
import os

def __lldb_init_module (debugger, dict):
  debugger.HandleCommand('command alias tbt t a a bt')
