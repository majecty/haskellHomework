
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Log
import LogAnalysis

main :: IO()
main = hspec $ do
  describe "Prelude.heade" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "parseMessage" $ do
    it "run sccueess" $ do
      parseMessage "invalid input" `shouldBe` InvalidLM "invalid input"
    it "should parse valid input" $ do
      parseMessage "E 2 562 help help" `shouldBe` ValidLM (LogMessage (Error 2) 562 "help help") 

  describe "validMessagesOnly" $ do
    it "empty return empty" $ do
      validMessagesOnly [] `shouldBe` []
    it "should filter invalid log" $ do
      validMessagesOnly [ InvalidLM "invalid", ValidLM $ LogMessage Info 4123 "info log" ] `shouldBe` [ LogMessage Info 4123 "info log" ]

  describe "compare Mssages" $ do
    it "comapre properly" $ do
      compareMsgs (LogMessage Info 10 "10 info") (LogMessage Warning 20 "20 warning") `shouldBe` LT

  describe "messagesAbout" $ do
    it "match word from log" $ do
      messagesAbout "relish" [ LogMessage Info 321 "Relishsign detected!!", LogMessage Info 341 "wewe" ] `shouldBe` [ LogMessage Info 321 "Relishsign detected!!" ]
