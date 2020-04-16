#!/bin/sh
cabal run --write-ghc-environment=always -w ghc-8.10.1 test-doctest
