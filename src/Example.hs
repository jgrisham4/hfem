module Example
(
) where

newtype TypeA = TypeA Int
data TypeB a  = TypeB [a] Int
data TypeC ta = TypeC ta Int
data TypeD 
