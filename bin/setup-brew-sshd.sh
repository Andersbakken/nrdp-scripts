#!/usr/bin/env bash

SCRIPT_DIR="$(dirname ${BASH_SOURCE[0]} )"

brew install openssh
cat $SCRIPT_DIR/org.openssh.sshd.plist | sudo tee /Library/LaunchDaemons/org.openssh.sshd.plist
cat $SCRIPT_DIR/sshd_config | sudo tee /opt/homebrew/etc/ssh/sshd_config
sudo launchctl unload /Library/LaunchDaemons/org.openssh.sshd.plist &>/dev/null
sudo launchctl load -w /Library/LaunchDaemons/org.openssh.sshd.plist
echo 'sshd is running at $(grep -o "^Port [0-9]\+")'
