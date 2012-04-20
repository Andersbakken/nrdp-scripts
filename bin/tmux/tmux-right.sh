#!/bin/sh
uptime | sed 's/.*load average: \([0-9.]*\), \([0-9.]*\), \([0-9.]*\)/Load: \1 \2 \3/'