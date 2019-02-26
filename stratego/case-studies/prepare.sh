#!/bin/sh

prepare() {
  pushd $1

  java -Xms512m -Xmx1024m -Xss16m -jar ../strategoxt.jar --lib -F --ast -i $1.str > $1.ast
  java -Xms512m -Xmx1024m -Xss16m -jar ../strategoxt.jar --lib -F -i $1.str > $1.core
  java -Xms512m -Xmx1024m -Xss16m -jar ../strategoxt.jar -i $1.str -o $1.java
  javac -cp ../strategoxt.jar $1.java
  
  runhaskell -i../../src ../Combine.hs $1.ast $1.core > $1.aterm
  
  ../pp-aterm -i $1.aterm > $1.aterm.pp
  
  popd
}

case $1 in
   all)
     prepare arrows
     prepare cca
     prepare pcf
     prepare arith
     prepare go2js
     prepare nnf
   ;;

   *)
     prepare $1
   ;;
esac
