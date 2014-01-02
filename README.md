CFG
===

A simple parser of of context free grammars in Chomsky normal form. Exports a single function 

    parse ::  (Eq a, Eq b,  Foldable f) => CFG a b -> f b -> [Parse a b]
    
give it a context free grammer and a string and it will return a list of all the parses. The CFG is defined as 

    -- | A context free grammar in Chomsky normal form. Contains the list of literal 
    --   mappings, a list of all the productions and the dedicated 'stop' symbol.
    data CFG a b = CFG [(a,b)] [(a, (a, a))] a deriving (Show, Eq)


and the resulting parse is 

    -- | A possible parse of a string 
        data Parse a b = 
        -- | A literal node that includes the original symbol and position in the input.
        Lit Int b a 

        -- | A production
        | Prod a (Parse a b) (Parse a b)
