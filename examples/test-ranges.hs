module TestRange where

from = [0..]
fromThen = [0..10]
fromThenTo = [0,5..10]
main = print (take 5 from) >> print fromThen >> print fromThenTo
