# DNSBL-milter

## Require 
- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Run
```bash
chmod +x app/Maim.hs
./app/Main.hs {host} {port} {name:provider_domain..} {+RTS -T}
```
- host - milter host.
- port - milter port. 
- provider_domain - Space separated list of name-domain pairs with `:` as delimiter. 
- {+RTS -T} - Extended GHC output to prometheus metrics

## Check
```bash
curl http://localhost/
curl http://127.0.0.1:6000/check\?domain\=google.com
```

## Metrics
```bash
curl http://localhost:6000/metrics
```
