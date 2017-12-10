FROM debian:9

WORKDIR /pascallite

RUN apt-get update \
    && apt-get install -y fpc

COPY . /pascallite
RUN cd /pascallite \
    && fpc -FuUnits/PascalCoin/ -FuSynapse/lib/ -FuUnits/Utils/ pascallited.pp \
    && ls pascallited \
    && strip pascallited \
    && file pascallited

FROM debian:9

RUN apt-get update \
    && apt-get install -y libssl1.1

COPY --from=0 /pascallite/pascallited /usr/bin
    
ENV RPC_BIND_IP=0.0.0.0
ENV MINING_SERVER_BIND_IP=0.0.0.0

EXPOSE 4003 4004 4009

CMD ["pascallited", "-r"]