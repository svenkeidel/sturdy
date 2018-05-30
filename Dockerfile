FROM fpco/stack-build:lts-11.10 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --test --haddock