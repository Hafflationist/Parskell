module ParskellTest.Synthesis.CounterTest where

import Control.Monad.Zip
import Data.List
import Parskell.Synthesis.Counter as Counter
import Test.Hspec
import Test.HUnit



listAppender :: [Int] -> Counter [Int]
listAppender xs = do 
    countNum <- Counter.retrieve
    return (countNum : xs)


    
listConnector :: MonadZip mz => mz [a] -> mz [a] -> mz [a]
listConnector = Control.Monad.Zip.mzipWith (flip (++))



counterTest = do
    it "should start counting at 0" $ do
        let listInCounter = listAppender [] >>= listAppender >>= listAppender >>= listAppender >>= listAppender
        let listUff = Counter.start 0 listInCounter
        listUff @?= [4, 3, 2, 1, 0]
        
    it "should start counting at 5" $ do
        let listInCounter = listAppender [] >>= listAppender >>= listAppender >>= listAppender >>= listAppender
        let listUff = Counter.start 5 listInCounter
        listUff @?= [9, 8, 7, 6, 5]
        
    it "should start counting at -5" $ do
        let listInCounter = listAppender [] >>= listAppender >>= listAppender >>= listAppender >>= listAppender
        let listUff = Counter.start (-5) listInCounter
        listUff @?= [-1, -2, -3, -4, -5]
        
    it "should start count seperated" $ do
        let listInCounter1 = listAppender [] >>= listAppender >>= listAppender
        let listInCounter2 = listAppender [] >>= listAppender >>= listAppender
        let listInCounter = listConnector listInCounter1 listInCounter2
        let listUff = Counter.start 0 listInCounter
        listUff @?= [5, 4, 3, 2, 1, 0]