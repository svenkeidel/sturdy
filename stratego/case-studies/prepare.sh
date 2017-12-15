#!/bin/sh

prepare() {
  pushd $1

  java -Xms512m -Xmx1024m -Xss16m -jar ../strategoxt.jar --lib -F --ast -i $1.str > $1.ast
  java -Xms512m -Xmx1024m -Xss16m -jar ../strategoxt.jar --lib -F -i $1.str > $1.core
  
  runhaskell -i../../src ../Combine.hs $1.ast $1.core > $1.aterm
  
  ../pp-aterm -i $1.aterm > $1.aterm.pp
  
  popd
}

prepare arrows
prepare cca
prepare pcf
prepare arith
prepare go2js
