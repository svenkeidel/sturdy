This directory includes a number of cases studies for our abstract interpreter for Stratego.
Each case study is a program transformation implemented in Stratego:
- A Go to JavaScript compiler (`go2js`)
- A desugaring from arrows pretty notation to arrow expressions (`arrows`)
- A normalization procedure for causal commutative arrows (`cca`)
- An interpreter and type checker for PCF

Each case study can be found in its own subfolder in stratego/case-studies. The main source file that runs the case studies is `Main.hs`. To run the case studies, first change directory to the toplevel sturdy source directory. Then build the stratego project using `stack build sturdy-stratego`, and run the case studies using `stack exec case-studies`.