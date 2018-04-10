module GildedRose where

type GildedRose = [Item]

data Item =
  Item String
       Int
       Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) = name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality)
      | name == "Sulfuras, Hand of Ragnaros" && quality > 0                                           = (Item name sellIn quality)
      | name == "Backstage passes to a TAFKAL80ETC concert" && sellIn <= 0                            = (Item name sellIn' 0)
      | name == "Backstage passes to a TAFKAL80ETC concert" && sellIn < 6 && quality < 48             = (Item name sellIn' (quality + 3))
      | name == "Backstage passes to a TAFKAL80ETC concert" && sellIn < 11 && quality < 49            = (Item name sellIn' (quality + 2))
      | name == "Aged Brie" && quality <= 48 && sellIn < 1                                            = (Item name sellIn' (quality + 2))
      | (name == "Backstage passes to a TAFKAL80ETC concert" || name == "Aged Brie") && quality < 50  = (Item name sellIn' (quality + 1))
      | quality <= 0 || quality == 50                                                                 = (Item name sellIn' quality)
      | sellIn <= 0 && quality > 1                                                                    = (Item name sellIn' (quality - 2))
      | otherwise                                                                                     = (Item name sellIn' (quality - 1))
      where sellIn' = sellIn - 1