# pact-corpus

This repository contains an archive of all of the modules on Kadena's mainnet.

## Output

The `corpus` directory has one subdirectory for each
chain. For each chain, there are two more subdirectories, `modules` and `latest`.
`modules` contains every module ever deployed on that chain. `latest` contains
only relative symlinks into `modules`, such that each symlink points to the most
recently deployed instance of each module.

An illustration:
```
❯ ls corpus/0/modules/ | grep 'lago.kwBTC'
lago.kwBTC-14500917.pact
lago.kwBTC-20992939.pact

❯ ls corpus/0/latest/ | grep 'lago.kwBTC'
lago.kwBTC-20992939.pact -> ../modules/lago.kwBTC-20992939.pact
```

## Usage

- Download a mainnet pact db.
- Run `rm -r corpus`
- Run `./scripts/pact-modules -i /path/to/mainnet/db -o corpus`.
- Run `tar -czvf corpus-$YEAR-$MM-$DD.tar.gz --directory=corpus .`
- Run `rm -r corpus`
