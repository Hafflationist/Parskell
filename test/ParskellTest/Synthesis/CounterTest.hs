module ParskellTest.Synthesis.CounterTest where

import Data.List
import Parskell.Synthesis.Counter as Counter
import Test.Hspec
import Test.HUnit



listAppender xs = do 
    countNum <- Counter.retrieve
    return (countNum : xs)



listConnector listInCounter1 listInCounter2 = do
    list1 <- listInCounter1
    list2 <- listInCounter2
    return (list2 ++ list1) -- because new elements get added on the head
    
    

counterTest = do
    it "should start counting at 0" $ do
        let listInCounter = listAppender [] >>= listAppender >>= listAppender >>= listAppender >>= listAppender
        let listUff = Counter.start listInCounter 0
        listUff @?= [4, 3, 2, 1, 0]
        
    it "should start counting at 5" $ do
        let listInCounter = listAppender [] >>= listAppender >>= listAppender >>= listAppender >>= listAppender
        let listUff = Counter.start listInCounter 5
        listUff @?= [9, 8, 7, 6, 5]
        
    it "should start counting at -5" $ do
        let listInCounter = listAppender [] >>= listAppender >>= listAppender >>= listAppender >>= listAppender
        let listUff = Counter.start listInCounter (-5)
        listUff @?= [-1, -2, -3, -4, -5]
        
    it "should start count seperated" $ do
        let listInCounter1 = listAppender [] >>= listAppender >>= listAppender
        let listInCounter2 = listAppender [] >>= listAppender >>= listAppender
        let listInCounter = listConnector listInCounter1 listInCounter2
        let listUff = Counter.start listInCounter 0
        listUff @?= [5, 4, 3, 2, 1, 0]