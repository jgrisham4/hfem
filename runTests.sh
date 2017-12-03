#!/bin/bash

#stack --stack-root /home/james/.stack --system-ghc --allow-different-user test --ta "--jxml testResults.xml"
stack --system-ghc --allow-different-user test --ta "--jxml testResults.xml"
