## Ethereum Haskell API

This is the Ethereum compatible Haskell API which implements the [Generic JSON RPC](https://github.com/ethereum/wiki/wiki/JSON-RPC) spec.

[![Build Status](https://travis-ci.org/airalab/hs-web3.svg?branch=master)](https://travis-ci.org/airalab/hs-web3)
[![Build status](https://ci.appveyor.com/api/projects/status/8ljq93nar8kobk75?svg=true)](https://ci.appveyor.com/project/akru/hs-web3)
[![Hackage](https://img.shields.io/hackage/v/web3.svg)](http://hackage.haskell.org/package/web3)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
[![Code Triagers Badge](https://www.codetriage.com/airalab/hs-web3/badges/users.svg)](https://www.codetriage.com/airalab/hs-web3)

### Installation

    $ git clone https://github.com/airalab/hs-web3 && cd hs-web3
    $ stack setup
    $ stack ghci

> This library runs only paired with [geth](https://github.com/ethereum/go-ethereum)
> or [parity](https://github.com/ethcore/parity) Ethereum node,
> please start node first before using the library.

### Testing
Embedded in the repository is a Truffle project with some contracts that are used as test cases.
You will need `truffle` and `jq`, as well as an Ethereum node to utilize the test suite. For the
most part, it's as simple as `stack test`. If you use an Ethereum node other than http://localhost:8545,
you must update truffle.js as well as pass the `WEB3_PROVIDER=http://node:port/` environment variable
to `stack test`.

Note, if the Ethereum node you're testing on uses an ephemeral blockchain, and restarts between runs of the test suite,
you may need to run `stack clean` prior to `stack test` to redeploy the contracts onto the new chain.

### Web3 monad

Any Ethereum node communication wrapped with `Web3` monadic type.

    > :t web3_clientVersion
    web3_clientVersion :: Provider a => Web3 a Text

To run this computation used `runWeb3'` or `runWeb3` functions.

    > runWeb3 web3_clientVersion
    Right "Parity//v1.4.5-beta-a028d04-20161126/x86_64-linux-gnu/rustc1.13.0"

Function `runWeb3` use default `Web3` provider at `localhost:8545`.

    > :t runWeb3
    runWeb3
      :: MonadIO m => Web3 DefaultProvider b -> m (Either Web3Error b)

### TemplateHaskell generator

[Quasiquotation](https://wiki.haskell.org/Quasiquotation) is used to parse
contract ABI or load from JSON file. [TemplateHaskell](https://wiki.haskell.org/Template_Haskell) driven Haskell contract API generator can automatical create instances for `Event` and `Method`
typeclasses and function helpers.

    > :set -XQuasiQuotes
    > putStr [abiFrom|data/sample.json|]
    Contract:
            Events:
                    Action1(address,uint256)
                    Action2(string,uint256)
            Methods:
                    0x03de48b3 runA1()
                    0x90126c7a runA2(string,uint256)

See example of usage.

```haskell
import Data.Text (unpack)
import Text.Printf

[abiFrom|data/ERC20.json|]

main :: IO ()
main = do
    Right s <- runWeb3 $ do
        n <- name token
        s <- symbol token
        d <- decimals token
        return $ printf "Token %s with symbol %s and decimals %d"
                        (unpack n) (unpack s) d
    putStrLn s
  where token = "0x237D60A8b41aFD2a335305ed458B609D7667D789"
```
