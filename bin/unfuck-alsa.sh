#!/bin/bash

pulseaudio -k
sudo alsa force-reload
pulseaudio -D
