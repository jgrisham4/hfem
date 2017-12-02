module Flux
( flux1d
, upwindFlux1d
, centralFlux1d
) where

avg1d :: Fractional a => a -> a -> a
avg1d uL uR = (uL + uR)/2.0

jump1d :: Fractional a => a -> a -> a -> a -> a
jump1d uL uR nL nR = nL * uL + nR * uR

flux1d :: Fractional a => a -> a -> a -> a -> a -> a -> a
flux1d uL uR nL nR c alpha = avg1d uL uR + c * (1.0 - alpha)/2.0 * jump1d uL uR nL nR

upwindFlux1d uL uR nL nR c = flux1d uL uR nL nR c 0.0
centralFlux1d uL uR nL nR c = flux1d uL uR nL nR c 1.0
