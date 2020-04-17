data Exp 
    =  Constant Int     
    | Variable String     
    | Minus Exp Exp    
    | Greater Exp Exp  
    | Times Exp Exp    
    deriving Show

data Com 
    =  Assign String Exp
    | Seq Com Com              
    | Cond Exp Com Com          
    | While Exp Com             
    | Declare String Exp Com   
    | Print Exp               
    deriving Show

s1 = Declare "x" (Constant 150)
       (Declare "y" (Constant 200)
          (Seq (While (Greater (Variable "x" ) (Constant 0)
                      )
                      (Seq (Assign "x" (Minus (Variable "x")
                                              (Constant 1)
                                       )
                           )
                           (Assign "y" (Minus (Variable "y")
                                              (Constant 1)
                                       )
                           )
                      )
               )
               (Print (Variable "y"))
          )
       )