#!/bin/bash

stack setup
stack --stack-root /home/james/.stack --allow-different-user test --ta "--jxml testResults.xml"
#stack --system-ghc --allow-different-user test --ta "--jxml testResults.xml"
