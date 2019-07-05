FROM fpco/stack-build:lts-13.26

RUN git clone https://github.com/svenkeidel/sturdy --branch oopsla-19-artifact

WORKDIR /sturdy
RUN stack build --fast
RUN stack test --fast
