module Bhaskara where 
main = do 
    bask 
    
delta :: Float -> Float -> Float -> Float
delta a b c = (b ^ 2) - 4 * a * c

bas :: Float -> Float -> Float -> Float -> Float
bas a b c i = ((-b) + (delta a b c ** (1 / 2)) * i) / (2 * a)

bask :: Float -> Float -> Float -> [Float]
bask a b c = [bas a b c 1, bas a b c (-1)]