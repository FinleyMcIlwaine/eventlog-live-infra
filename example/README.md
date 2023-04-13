# Example application for use with `eventlog-live` infrastructure

This program is very basic, and is not Good Haskell:tm:, but does have an
interesting memory profile. It reads the eventlog socket path from the
`GHC_EVENTLOG_SOCKET` environment variable. Set that variable to the desired
socket path, then run with:

```
cabal run example -- +RTS -l -hT --eventlog-flush-interval=1
```

Then just `docker compose up`.
