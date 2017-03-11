{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Game.MetaGame.TypesSpec
       where
import SpecHelper
import Data.Data

import qualified Data.Text as T
import Game.MetaGame.Types.Core
-- import Game.MetaGame.Types.Core (generifyLE, canonifyLE)

data T1 = T1 String
        deriving (Eq, Show, Data)
data T1Id = T1Id String
          deriving (Eq,Show)
type instance IdF T1 = T1Id
instance IdAble T1 where
  idOf (T1 string) = T1Id string

data T2 = T2 String
        deriving (Eq, Show, Data)
data T2Id = T2Id String
          deriving (Eq,Show)
type instance IdF T2 = T2Id
instance IdAble T2 where
  idOf (T2 string) = T2Id string

spec :: Spec
spec = do
  describe "Object" $ do
    it "getObject of empty list" $
      (getObject (T1Id "id1") (World []) :: Maybe T1) `shouldBe` Nothing
    it "getObject of Singleton list: included" $
      (getObject (T1Id "id1") (World [Object (T1 "id1")]) :: Maybe T1) `shouldBe` Just (T1 "id1")
    it "getObject of Singleton list: not included" $
      (getObject (T1Id "id1") (World [Object (T1 "id2")]) :: Maybe T1) `shouldBe` Nothing
    it "getObject of NonSingleton list: included" $
      (getObject (T1Id "id1") (World [Object (T2 "2id1"), Object (T1 "id1"), Object (T2 "2id2")]) :: Maybe T1) `shouldBe` Just (T1 "id1")
  describe "LogEntries and Log" $ let
      txt1 = T.pack "test"
      txt2 = T.pack "test2"
      logEntrys = [ ULogE Admin      txt1 txt2
                  , ULogE Guest      txt1 txt2
                  , ULogE (U "user") txt1 txt2
                  , ALogE            txt1 txt2
                  , CLogE            txt1 ]
    in it "generifyLE . canonifyLE . canonifyLE . generifyLE == id" $
        mapM_ (\le -> (do
                       let newle = (generifyLE . canonifyLE . canonifyLE . generifyLE) le
                       (getRestricted newle)`shouldBe` (getRestricted le)
                       (getUnrestricted newle)`shouldBe` (getUnrestricted le)
                      )) logEntrys

main :: IO ()
main = hspec spec
