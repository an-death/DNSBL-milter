# DNSBL-milter

## Require 
- [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Run
```bash
chmod +x app/Maim.hs
./app/Main.hs {host} {port} {provider_domains..} {+RTS -T}
```
- host - milter host. Default `localhost`
- port - milter port. Default `8000`
- provider_domain - list of domain separated by spaces. Default `zen.spamhaus.org`
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
