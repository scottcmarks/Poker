#!/bin/bash
cd ~/Poker
find app src -type f -name '*.hs' -not -name '*.th.hs' -exec grep -nH "$*" {} ';'
