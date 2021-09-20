module Bhaskara where 
main = do 
    bhaskara 
    
delta :: Float -> Float -> Float -> Float
delta a b c = (b ^ 2) - 4 * a * c

base :: Float -> Float -> Float -> Float -> Float
base a b c i = ((-b) + (delta a b c ** (1 / 2)) * i) / (2 * a)

bhaskara :: Float -> Float -> Float -> [Float]
bhaskara a b c = [base a b c 1, base a b c (-1)]