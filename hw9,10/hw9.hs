-- Funny takes two arguments, the first one a type of kind * -> *, 
-- and the second a type of kind *, and constructs a type. 
-- (How did GHCi know what the kind of Funny is? Well, 
-- it does kind inference just like it also does type inference.)
-- Funny is a higher-order type constructor, 
-- in the same way that map is a higher-order function. 
-- Note that types can be partially applied, too, just like functions:
data Funny f a = Funny a (f a)
