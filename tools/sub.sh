#!/bin/sh

mv $3 $3.bak
cat $3.bak | runhaskell tools/sub.hs $1 $2 > $3
