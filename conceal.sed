#!/usr/bin/env sed -f
#
# Convert common Haskell operators to unicode

s/->/→/g
s/::/∷/g
s/\\/λ /g
