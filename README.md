# DNSBL-milter

DNSBL milter for collecting RBL results


## Require 
- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Run
```bash
stack build
stack run -- -m {host}:{port} --http-port={6000} {name:provider_domain..} {+RTS -T}
```
- -m {host}:{port} - Colon separated host and port for a milter server
- --http-port      - TCP port for internal http server
- provider_domain  - Space separated list of name-domain pairs with `:` as delimiter.
- {+RTS -T}        - Extended GHC output to prometheus metrics

## Check
```bash
curl http://localhost:6000/
curl http://localhost:6000/check\?domain\=google.com
```

## Metrics
```bash
curl http://localhost:6000/metrics
```
