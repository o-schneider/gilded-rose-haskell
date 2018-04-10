module GildedRoseSpec
  ( spec
  ) where

import           GildedRose
import qualified LegacyGildedRose as Legacy
import           Test.Hspec

spec :: Spec
spec =
  describe "updateQuality" $ do
    it "safety net" $ do
      let days = 1000
          inventories = take days $ iterate updateQuality defaultItems
          legacyInventories = take days $ iterate Legacy.updateQuality defaultItems
      inventories `shouldBe` legacyInventories

defaultItems :: GildedRose
defaultItems =
  [ Item "+5 Dexterity Vest" 10 20
  , Item "Aged Brie" 2 0
  , Item "Elixir of the Mongoose" 5 7
  , Item "Sulfuras, Hand of Ragnaros" 0 80
  , Item "Sulfuras, Hand of Ragnaros" (-1) 80
  , Item "Backstage passes to a TAFKAL80ETC concert" 15 20
  , Item "Backstage passes to a TAFKAL80ETC concert" 10 49
  , Item "Backstage passes to a TAFKAL80ETC concert" 5 48
  , Item "Backstage passes to a TAFKAL80ETC concert" 5 49
  ]
