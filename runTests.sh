#!/bin/bash

stack setup
stack --allow-different-user test --ta "--jxml testResults.xml"
#stack --system-ghc --allow-different-user test --ta "--jxml testResults.xml"
