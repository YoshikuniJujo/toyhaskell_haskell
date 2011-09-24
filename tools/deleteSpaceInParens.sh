#!/bin/sh

cat $1 | runhaskell tools/deleteSpaceInParens.hs > $1.new
mv $1.new $1
