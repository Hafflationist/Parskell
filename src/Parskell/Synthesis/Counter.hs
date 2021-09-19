{-# LANGUAGE TupleSections #-}

module Parskell.Synthesis.Counter (retrieve, start) where
  
newtype Counter a = Counting { coreFunc :: Int -> (a, Int) }
  
  
  
retrieve :: Counter Int
retrieve = 
    let newCoreFunc curC = (curC + 1, curC + 1)
    in Counting { coreFunc = newCoreFunc }
    
    
    
start :: Counter a -> Int -> a
start Counting {coreFunc = f} x = fst . f $ (x - 1)
  


map :: (a -> b) -> Counter a -> Counter b
map mapper Counting { coreFunc = cf } = 
    let newCoreFunc curC = let (result, postC) = cf curC
                           in (mapper result, postC)
    in Counting { coreFunc = newCoreFunc }
    
    
    
bind :: Counter a -> (a -> Counter b) -> Counter b
bind Counting { coreFunc = func1 } func =
    let newCoreFunc curC = let (result, postC) = func1 curC
                               Counting { coreFunc = nextFunc } = func result
                               (nextResult, postPostC) = nextFunc postC
                           in (nextResult, postPostC)
    in Counting { coreFunc = newCoreFunc }



instance Functor Counter where
    fmap = Parskell.Synthesis.Counter.map
    
instance Applicative Counter where
    pure a = Counting { coreFunc = (a,) }
    Counting { coreFunc = func1 } <*> Counting { coreFunc = func2 } = 
        let newCoreFunc curC = let (resultFunc, postC) = func1 curC
                                   (result, postPostC) = func2 postC
                               in (resultFunc result, postPostC)
        in Counting { coreFunc = newCoreFunc }
        
instance Monad Counter where
    (>>=) = Parskell.Synthesis.Counter.bind