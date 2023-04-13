FROM haskell:9.2.7

# Clone repo
RUN git clone https://github.com/FinleyMcIlwaine/eventlog-influxdb.git /eventlog-influxdb
WORKDIR /eventlog-influxdb

# Build eventlog-influxdb
RUN cabal update && cabal install --overwrite-policy=always eventlog-influxdb

ENV GHC_EVENTLOG_SOCKET "/tmp/ghc-eventlog-socket"
ENV GHC_EVENTLOG_INFLUXDB_HOST "influxdb"
ENV GHC_EVENTLOG_INFLUXDB_DB "eventlog"
ENV GHC_EVENTLOG_INFLUXDB_USERNAME "admin"
ENV GHC_EVENTLOG_INFLUXDB_PASSWORD "admin"

CMD eventlog-influxdb
