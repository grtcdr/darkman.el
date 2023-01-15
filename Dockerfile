FROM golang:1.19.5-alpine
ENV MAKEFLAGS "-j 8 --no-print-directory"
RUN apk add --update build-base make git scdoc
RUN git clone https://gitlab.com/WhyNotHugo/darkman
RUN cd darkman \
    make \
    make install \
