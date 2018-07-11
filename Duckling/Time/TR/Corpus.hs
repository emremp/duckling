-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.TR.Corpus
  ( corpus
  , defaultCorpus
  , negativeCorpus
  , latentCorpus
  ) where

import Data.String
import Prelude

import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

defaultCorpus :: Corpus
defaultCorpus = (testContext, testOptions, allExamples ++ custom)
  where
    custom = concat
      [ examples (datetime (2013, 2, 15, 0, 0, 0) Day)
                 [ 	"15/02"
                 , 	"2/15 üzerinde"
                 , 	"2/15"
                 , 	"2-15"
                 , 	"2-15"
                 ]
      , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
                 [ 	"1974/10/31"
                 , 	"10/31/74"
                 , 	"10-31-74"
                 , 	"1974/10/31"
                 , 	"/ 1974 31 / Ekim"
                 , 	"31 Ekim 1974"
                 ]
      , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
                 [ 	"04:00 pm 4/25"
                 ]
      , examples (datetimeHoliday (2013, 11, 28, 0, 0, 0) Day 	"Şükran Günü")
                 [ 	"Şükran Günü"
                 , 	"Şükran"
                 , 	"Şükran 2013"
                 , 	"Bu şükran"
                 , 	"Bir sonraki şükran günü"
                 , 	"9 ayda şükran"
                 , 	"Bundan şükran 9 ay"
                 ]
      , examples (datetimeHoliday (2014, 11, 27, 0, 0, 0) Day 	"Şükran Günü")
                 [ 	"Gelecek yılın şükran"
                 , 	"Bir yılda şükran"
                 , 	"Şükran 2014"
                 ]
      , examples (datetimeHoliday (2012, 11, 22, 0, 0, 0) Day 	"Şükran Günü")
                 [ 	"Geçen şükran"
                 , 	"Şükran günü 2012"
                 , 	"Şükran 3 ay önce"
                 , 	"Şükran 1 yıl önce"
                 ]
      , examples (datetimeHoliday (2016, 11, 24, 0, 0, 0) Day 	"Şükran Günü")
                 [ 	"Şükran 2016"
                 , 	"3 yıl içinde şükran"
                 ]
      , examples (datetimeHoliday (2017, 11, 23, 0, 0, 0) Day 	"Şükran Günü")
                 [ 	"Şükran 2017"
                 ]
      ]

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ 	"yüksek sesle gülmek"
      , 	"1 yetişkin"
      , 	"Biz ayrılır"
      , 	"25"
      , 	"işte bu o"
      , 	"Bu"
      , 	"Bu geçmiş bir"
      , 	"Tekli"
      , 	"Birkaç at"
      , 	"Ikililerine"
      , 	"Birkaç"
      , 	"Onlarca"
      , 	"Tek Saat"
      , 	"Onlarca saat"
      , 	"Fare 6"
      , 	"Sıçan 6"
      , 	"3 30"
      , 	"Üç yirmi"
      , 	"650.650.6500 at"
      , 	"650-650-6500 at"
      , 	"İki altmış bir m"
      , 	"2000 ABC Öde"
      , 	"4a"
      , 	"4a."
      , 	"A4 A5"
      , 	"avuç içi"
      ]

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, xs)
  where
    xs = concat
      [ examples (datetime (2013, 2, 24, 0, 0, 0) Day)
                 [ 	"24"
                 , 	"Açık 24"
                 ]
      , examples (datetime (2013, 2, 12, 7, 0, 0) Hour)
                 [ 	"7"
                 , 	"7a"
                 ]
      , examples (datetime (2013, 2, 12, 19, 0, 0) Hour)
                 [ 	"7p"
                 ]
      , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
                 [ 	"on otuz"
                 ]
      --, examples (datetime (1954, 1, 1, 0, 0, 0) Year)
      --           [ 	"1954"
      --           ]
      , examples (datetime (2013, 5, 1, 0, 0, 0) Month)
                 [ 	"Mayıs ayı"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
                 [ 	"sabah"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
                 [ 	"öğleden sonra"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ 	"akşam"
                 ]
      , examples (datetimeInterval
          ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
                 [ 	"gece"
                 ]
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ 	"Şimdi"
             , 	"şimdi"
             , 	"Şu anda"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ 	"bugün"
             , 	"şu anda"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ 	"2/2013"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ 	"2014 yılında"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ 	"dün"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ 	"yarın"
             , 	"Yarınlar"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ 	"Pazartesi"
             , 	"Pzt"
             , 	"bu pazartesi"
             , 	"Pazartesi, 18 Şubat"
             , 	"Mon, 18 Şubat"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ 	"Salı"
             , 	"Salı 19"
             , 	"Salı 19"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ 	"Per 15"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ 	"Perşembe"
             , 	"Per"
             , 	"Per"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ 	"Cuma"
             , 	"Cum"
             , 	"Cum"
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ 	"Cumartesi"
             , 	"oturdu"
             , 	"oturdu."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ 	"Pazar"
             , 	"Güneş"
             , 	"Güneş."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ 	"Yürüyüşü 1"
             , 	"Martın ilk günü"
             , 	"İlk yürüyüşü"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             [ 	"3 Mart"
             ]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             [ 	"Zirveye Giden Yol"
             ]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ 	"3 Mart 2015"
             , 	"3 Mart 2015"
             , 	"2015 üçüncü yürüyüşü"
             , 	"2015/03/03"
             , 	"3/3/15"
             , 	"2015/03/03"
             , 	"2015/03/03"
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ 	"15'inde"
             , 	"Şubat 15."
             , 	"Şubat 15"
             , 	"15 Şubat"
             , 	"15 Şubat"
             , 	"15 Şubat"
             , 	"15 Şubat"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             [ 	"8 Ağustos"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ 	"1 yıl Mart"
             , 	"Bir yıl Mart"
             ]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ 	"Cum, 18 Temmuz"
             , 	"18 Temmuz, Cum"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             [ 	"Ekim 2014"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ 	"14april 2015"
             , 	"14 Nisan 2015"
             , 	"14Th 15 Nisan"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ 	"gelecek salı"
             , 	"Etrafında bir sonraki Salı"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ 	"Önümüzdeki sonra Cuma"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ 	"Gelecek mart"
             ]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             [ 	"Önümüzdeki sonra Mart"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ 	"Pazar, 10 Şubat"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ 	"Çar, Feb13"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ 	"bu hafta"
             , 	"bu hafta"
             , 	"gelecek hafta"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ 	"geçen hafta"
             , 	"geçen hafta"
             , 	"önceki hafta"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ 	"gelecek hafta"
             , 	"önümüzdeki hafta"
             , 	"Etrafında Haftaya"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ 	"geçen ay"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ 	"gelecek ay"
             ]
  , examples (datetime (2013, 3, 20, 0, 0, 0) Day)
             [ 	"Bir sonraki ayın 20"
             , 	"Bir sonraki ayın 20."
             , 	"Bir sonraki ayın 20. günü"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ 	"Mevcut ayın 20'sinde"
             , 	"Bu ayın 20"
             ]
  , examples (datetime (2013, 1, 20, 0, 0, 0) Day)
             [ 	"Önceki ayın 20"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ 	"bu çeyrek"
             , 	"Bu çyr"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ 	"sonraki çeyrek"
             , 	"Bir sonraki çyr"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ 	"üçüncü çeyrek"
             , 	"3. çeyrek"
             , 	"Üçüncü çyr"
             , 	"3 çyr"
             , 	"3 çyr"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ 	"4. çeyrek 2018"
             , 	"4 çyr 2018"
             , 	"2018 4. çyr"
             , 	"18q4"
             , 	"2018Q4"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ 	"geçen yıl"
             , 	"Geçen yıl"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ 	"bu yıl"
             , 	"Mevcut yıl"
             , 	"Bu yıl"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ 	"Gelecek yıl"
             , 	"Bir sonraki yıl"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ 	"2014 AD",
               	"2014 AD"
             ]
  , examples (datetime (-2014, 1, 1, 0, 0, 0) Year)
             [ 	"2014 yılında BC",
               	"2014 yılında BC"
             ]
  , examples (datetime (14, 1, 1, 0, 0, 0) Year)
             [ 	"14 reklam"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ 	"geçen pazar"
             , 	"Geçen hafta Pazar"
             , 	"Geçen haftaki Pazar"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ 	"geçen salı"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ 	"gelecek salı"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ 	"gelecek çarşamba"
             ]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ 	"Önümüzdeki hafta Çarşamba"
             , 	"Çarşamba önümüzdeki hafta"
             , 	"Önümüzdeki sonra Çarşamba"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ 	"Önümüzdeki sonra Cuma"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ 	"Bu haftanın Pazartesi"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ 	"Bu hafta Salı"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ 	"Bu hafta Çarşamba"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ 	"yarından sonraki gün"
             ]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ 	"Öbürgün 05:00"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ 	"dünden önceki gün"
             ]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ 	"Dünden önceki gün 08:00"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             [ 	"Mart ayının son Pazartesi günü"
             ]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             [ 	"Mart 2014'te son Pazar"
             ]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             [ 	"Ekim üçüncü gün"
             ]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             [ 	"Ekim 2014 ayının ilk haftasında"
             ]
  , examples (datetime (2018, 12, 10, 0, 0, 0) Week)
             [ 	"2018 yılının üçüncü geçtiğimiz hafta"
             , 	"2018 yılının üçüncü geçtiğimiz hafta"
             , 	"2018 3 geçen hafta"
             ]
  , examples (datetime (2018, 10, 15, 0, 0, 0) Week)
             [ 	"Ekim 2018 2. geçen hafta"
             , 	"Ekim 2018 yılının ikinci geçen hafta"
             ]
  , examples (datetime (2013, 5, 27, 0, 0, 0) Day)
             [ 	"Mayıs ayının beşinci son gün"
             , 	"Mayıs ayının 5 son gün"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ 	"Ekim 6th haftası"
             ]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             [ 	"Ekim 7 hafta"
             ]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             [ 	"Ekim 2015 son günü"
             , 	"Ekim 2015 yılında son günü"
             ]
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             [ 	"Eylül 2014 geçen hafta"
             ]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             [ 	"Ekim ayının ilk Salı"
             , 	"Ekim ayında ilk Salı"
             ]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             [ 	"Eylül 2014 üçüncü Salı"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             [ 	"Ekim 2014 ilk Çarşamba"
             ]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             [ 	"Ekim 2014 ikinci Çarşamba"
             ]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             [ 	"Noel 2014 yılından sonra üçüncü Salı"
             ]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ 	"3 am"
             , 	"3 AM"
             , 	"At 3:00"
             , 	"3 oclock am"
             , 	"En 03:00"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ 	"03:18"
             , 	"3: 18a"
             ]
  , examples (datetime (2016, 2, 1, 7, 0, 0) Hour)
             [ 	"7 3 yıl"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ 	"3 de"
             , 	"@ 03:00"
             , 	"03:00"
             , 	"03:00"
             , 	"3 oclock pm"
             , 	"Öğleden sonra 03:00"
             , 	"3ish pm"
             , 	"15:00 yaklaşık"
             , 	"Yaklaşık 15:00"
             , 	"3 p at"
             , 	"3 p at."
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ 	"15 Geçtiğimiz 15:00"
             , 	"3 de gönderilen çeyrek"
             , 	"Öğleden sonra 03:15"
             , 	"15:15"
             , 	"03:15"
             , 	"03:15"
             , 	"3: 15p"
             , 	"3, 15"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ 	"20 Geçtiğimiz 15:00"
             , 	"Öğleden sonra 03:20"
             , 	"Öğleden sonra 03:20"
             , 	"Yirmi sonra 03:00"
             , 	"3: 20p"
             , 	"Üçte yirmi"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ 	"Yarıda üç pm geçmiş"
             , 	"Buçuk 03:00"
             , 	"15:30"
             , 	"öğleden sonra 3:30"
             , 	"ÖĞLEDEN SONRA 3:30"
             , 	"330 pm"
             , 	"öğleden sonra 3:30"
             , 	"03:30"
             , 	"Yarım üç"
             ]
  , examples (datetime (2013, 2, 12, 9, 59, 0) Minute)
             [ 	"Dokuz 59 bir m"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ 	"15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ 	"Öğlen çeyrek"
             , 	"11:45"
             , 	"15 öğlen için"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ 	"8 Bu akşam"
             , 	"Sekiz gece"
             , 	"8 Bu akşam"
             , 	"Akşam 8'de"
             , 	"Sekizde akşam"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ 	"Cum 7:30 de, 20 Eylül"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ 	"9 am Cumartesi günü"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ 	"9 am Cumartesi günü"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ 	"Cum, 18 Temmuz 2014 07:00"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ 	"Bir saniye"
             , 	"Şu andan itibaren bir saniye"
             , 	"In 1 \""
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ 	"Bir dakika içinde"
             , 	"One minute"
             , 	"1' "
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ 	"2 dakika"
             , 	"2 dakika daha"
             , 	"2 dakika sonraya"
             , 	"bir kaç dakika içinde"
             , 	"Dakika bir çift"
             ]
  , examples (datetime (2013, 2, 12, 4, 33, 0) Second)
             [ 	"Üç dakika"
             , 	"Birkaç dakika içinde"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ 	"60 dakika"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ 	"Bir saat bir çeyrek"
             , 	"1/4 saat"
             , 	"Içinde 1/4 h"
             , 	"Içinde 1/4 saat"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ 	"Yarım saat içinde"
             , 	"1/2 saat"
             , 	"1/2 h"
             , 	"Içinde 1/2 saat"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ 	"Bir saat dörtte üçü de"
             , 	"3/4 saat"
             , 	"3/4 h"
             , 	"Içinde 3/4 saat"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ 	"2.5 saat"
             , 	"2 ve bir buçuk saat"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ 	"bir saat içinde"
             , 	"1 saat içinde"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ 	"Bir kaç saat"
             , 	"bir kaç saat içinde"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ 	"Bir kaç saat içinde"
             , 	"Kaç saat"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ 	"24 saat"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ 	"bir günde"
             , 	"Şimdi bir günlük"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
             [ 	"Şu anda bir günlük"
             ]
  , examples (datetime (2016, 2, 12, 0, 0, 0) Day)
             [ 	"Bugünden 3 yıl"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ 	"7 gün"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ 	"1 hafta"
             , 	"bir hafta içinde"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ 	"Yaklaşık yarım saat"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ 	"7 gün önce"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ 	"14 gün önce"
             , 	"İki hafta önce"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ 	"bir hafta önce"
             , 	"bir hafta önce"
             , 	"1 hafta önce"
             ]
  , examples (datetime (2013, 1, 31, 0, 0, 0) Day)
             [ 	"2 persembe geri"
             , 	"2 persembe önce"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ 	"Üç hafta önce"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ 	"üç ay önce"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ 	"iki yıl önce"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ 	"7 gün dolayısıyla"
             ]
  , examples (datetime (2013, 2, 26, 4, 0, 0) Hour)
             [ 	"14 gün dolayısıyla"
             , 	"Bir iki hafta dolayısıyla"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ 	"Haftada dolayısıyla"
             , 	"Bir hafta dolayısıyla"
             , 	"1 hafta dolayısıyla"
             ]
  , examples (datetime (2013, 3, 5, 0, 0, 0) Day)
             [ 	"Üç hafta dolayısıyla"
             ]
  , examples (datetime (2013, 5, 12, 0, 0, 0) Day)
             [ 	"Üç ay dolayısıyla"
             ]
  , examples (datetime (2015, 2, 1, 0, 0, 0) Month)
             [ 	"İki yıl dolayısıyla"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ 	"Bir yıllık ardından yılbaşı"
             , 	"Noel itibaren bir yıl"
             ]
  , examples (datetimeInterval ((2013, 12, 18, 0, 0, 0), (2013, 12, 29, 0, 0, 0)) Day)
             [ 	"18 Aralık itibaren 10 gün boyunca"
             , 	"18 Aralık dan 10 gün"
             , 	"10 gün boyunca 18 Aralık"
             ]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ 	"Bu yaz"
             , 	"Mevcut yaz"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ 	"Bu kış"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 19, 0, 0, 0)) Day)
             [ 	"bu sezon"
             , 	"Mevcut mevsimler"
             ]
  , examples (datetimeInterval ((2012, 9, 23, 0, 0, 0), (2012, 12, 20, 0, 0, 0)) Day)
             [ 	"Geçen sezon"
             , 	"Geçtiğimiz mevsimler"
             , 	"Önceki mevsimler"
             ]
  , examples (datetimeInterval ((2013, 3, 20, 0, 0, 0), (2013, 6, 20, 0, 0, 0)) Day)
             [ 	"gelecek sezon"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ 	"son gece"
             , 	"dün akşam"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 21, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ 	"Gece geç"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day 	"Noel")
             [ 	"Noel"
             , 	"Noel"
             , 	"Noel günü"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 18, 0, 0) Hour 	"Noel")
             [ 	"6 pm noel"
             ]
  , examples (datetimeIntervalHoliday ((2013, 12, 25, 4, 0, 0), (2013, 12, 25, 12, 0, 0)) Hour 	"Noel")
             [ 	"Sabah Noel ait"
             , 	"Noel 2013 sabahı"
             , 	"Bu yılbaşı günün sabahı"
             ]
  , examples (datetimeHoliday (2013, 12, 31, 0, 0, 0) Day 	"Yeni Yıl arifesi")
             [ 	"Yeni Yıl arifesi"
             , 	"Yeni Yıl arifesi"
             ]
  , examples (datetimeHoliday (2014, 1, 1, 0, 0, 0) Day 	"Yılbaşı")
             [ 	"Yılbaşı günü"
             , 	"Yeni yıl günü"
             ]
  , examples (datetimeHoliday (2013, 2, 14, 0, 0, 0) Day 	"Sevgililer Günü")
             [ 	"Sevgililer Günü"
             , 	"Sevgililer Günü"
             ]
  , examples (datetime (2013, 7, 4, 0, 0, 0) Day)
             [ 	"4 Temmuz"
             , 	"Temmuz 4"
             ]
  , examples (datetimeHoliday (2013, 10, 31, 0, 0, 0) Day 	"Cadılar Bayramı")
             [ 	"Cadılar Bayramı"
             , 	"Bir sonraki Cadılar Bayramı"
             , 	"Cadılar Bayramı 2013"
             ]
  , examples (datetimeHoliday (2013, 11, 29, 0, 0, 0) Day 	"Kara Cuma")
             [ 	"Kara Cuma"
             , 	"Bu yılın siyah Cuma"
             , 	"Siyah Cuma 2013"
             ]
  , examples (datetimeHoliday (2017, 11, 24, 0, 0, 0) Day 	"Kara Cuma")
             [ 	"Siyah Cuma 2017"
             ]
  , examples (datetimeHoliday (2013, 10, 16, 0, 0, 0) Day 	"Patronun Günü")
             [ 	"Patronun günü"
             , 	"Patron yönettiği"
             , 	"Patron günü"
             , 	"Bir sonraki patronun günü"
             ]
  , examples (datetimeHoliday (2016, 10, 17, 0, 0, 0) Day 	"Patronun Günü")
             [ 	"Patronun gün 2016"
             ]
  , examples (datetimeHoliday (2021, 10, 15, 0, 0, 0) Day 	"Patronun Günü")
             [ 	"Patronun gün 2021"
             ]
  , examples (datetimeHoliday (2014, 1, 20, 0, 0, 0) Day 	"Martin Luther King'in Günü")
             [ 	"MLK günü"
             , 	"Bir sonraki Martin Luther King günü"
             , 	"Bu MLK günü"
             ]
  , examples (datetimeHoliday (2013, 1, 21, 0, 0, 0) Day 	"Martin Luther King'in Günü")
             [ 	"Son MLK Jr günü"
             , 	"MLK gün 2013"
             ]
  , examples (datetimeHoliday (2012, 1, 16, 0, 0, 0) Day 	"Martin Luther King'in Günü")
             [ 	"Geçen yılın MLK günü"
             , 	"MLK gün 2012"
             , 	"Geçen yılın Sivil Hakları Günü"
             ]
  , examples (datetimeHoliday (2013, 11, 1, 0, 0, 0) Day 	"Dünya Vegan Günü")
             [ 	"Dünya vegan bir gün"
             ]
  , examples (datetimeHoliday (2013, 3, 31, 0, 0, 0) Day 	"Paskalya Pazar")
             [ 	"Paskalya"
             , 	"Paskalya 2013"
             ]
 , examples (datetimeHoliday (2012, 4, 08, 0, 0, 0) Day 	"Paskalya Pazar")
             [ 	"geçen Paskalya"
             ]
  , examples (datetimeHoliday (2013, 4, 1, 0, 0, 0) Day 	"Paskalya Pazartesi")
             [ 	"Paskalya mon"
             ]
  , examples (datetimeHoliday (2010, 4, 4, 0, 0, 0) Day 	"Paskalya Pazar")
             [ 	"Paskalya 2010"
             , 	"Paskalya Pazar 2010"
             ]
  , examples (datetime (2013, 4, 3, 0, 0, 0) Day)
             [ 	"Üç gün sonra Paskalya"
             ]
  , examples (datetimeHoliday (2013, 3, 28, 0, 0, 0) Day 	"Paskalya Öncesi Perşembe")
             [ 	"Paskalya Öncesi Perşembe"
             , 	"Covenant Per"
             , 	"Esrar Per"
             ]
  , examples (datetimeHoliday (2013, 5, 19, 0, 0, 0) Day 	"Hamsin")
             [ 	"Hamsin"
             , 	"Beyaz Pazar 2013"
             ]
  , examples (datetimeHoliday (2013, 5, 20, 0, 0, 0) Day 	"Whit Pazartesi")
             [ 	"Katiyen Pazartesi"
             , 	"Kutsal Ruh Pazartesi"
             ]
  , examples (datetimeHoliday (2013, 3, 24, 0, 0, 0) Day 	"Palmiye Pazar")
             [ 	"palmiye Pazar"
             , 	"Dal Pazar 2013"
             ]
  , examples (datetimeHoliday (2013, 5, 26, 0, 0, 0) Day 	"Trinity Pazar")
             [ 	"Üçlü Pazar"
             ]
  , examples (datetimeHoliday (2013, 2, 12, 0, 0, 0) Day 	"Cadılar")
             [ 	"Gözleme gün 2013"
             ]
  , examples (datetimeIntervalHoliday ((2018, 2, 14, 0, 0, 0), (2018, 4, 1, 0, 0, 0)) Day 	"Lent")
             [ 	"2018'i lent"
             ]
  , examples (datetimeHoliday (2018, 4, 8, 0, 0, 0) Day 	"Ortodoks Paskalya Pazarı")
             [ 	"Ortodoks paskalya 2018"
             ]
  , examples (datetimeHoliday (2018, 2, 19, 0, 0, 0) Day 	"Temiz Pazartesi")
             [ 	"Temiz Pazartesi 2018"
             , 	"Ortodoks shrove Pazartesi 2018"
             ]
  , examples (datetimeHoliday (2018, 3, 31, 0, 0, 0) Day 	"Lazarus Cumartesi")
             [ 	"Lazarus Cumartesi 2018"
             ]
  , examples (datetimeIntervalHoliday ((2018, 2, 19, 0, 0, 0), (2018, 3, 31, 0, 0, 0)) Day 	"Büyük Perhiz")
             [ 	"Büyük oruç 2018"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ 	"bu akşam"
             , 	"bu akşam"
             , 	"Bu gece"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ 	"geçen hafta sonu"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ 	"yarın akşam"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ 	"Yarın öğle"
             , 	"Öğle yemeğinde yarın"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ 	"dün akşam"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ 	"bu haftasonu"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ 	"Pazartesi sabahı"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 9, 0, 0)) Hour)
             [ 	"Sabah erken Pazartesi"
             , 	"Pazartesi sabah erken"
             , 	"Sabahın erken saatlerinde Pazartesi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 21, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ 	"bu gece geç"
             , 	"Geç tonite"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ 	"Sabah 15 Şubat"
             , 	"Sabah Şubat 15"
             , 	"Şubat 15'inden sabahı"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ 	"Son 2 saniye"
             , 	"Son iki saniye"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ 	"Önümüzdeki 3 saniye"
             , 	"Önümüzdeki üç saniye"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ 	"Son 2 dakika"
             , 	"Son iki dakika"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ 	"Önümüzdeki 3 dakika"
             , 	"Önümüzdeki üç dakika"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ 	"Son 1 saat"
             , 	"Son bir saat"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ 	"Önümüzdeki 3 saat"
             , 	"Önümüzdeki üç saat"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ 	"Son 2 gün"
             , 	"son iki gün"
             , 	"Geçtiğimiz 2 gün"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ 	"Önümüzdeki 3 gün"
             , 	"Önümüzdeki üç gün"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ 	"önümüzdeki birkaç gün"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ 	"Son 2 hafta"
             , 	"son iki hafta"
             , 	"Geçtiğimiz 2 hafta"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ 	"Önümüzdeki 3 hafta"
             , 	"Önümüzdeki üç hafta"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ 	"Son 2 ayda"
             , 	"Son iki ay"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ 	"Önümüzdeki 3 ay"
             , 	"Önümüzdeki üç ay"
             ]
  , examples (datetimeInterval ((2011, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ 	"Son 2 yıldır"
             , 	"Son iki yıldır"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ 	"Önümüzdeki 3 yıl"
             , 	"Önümüzdeki üç yıl"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ 	"13-15 Temmuz"
             , 	"15 Temmuz 13"
             , 	"15 aracılığıyla 13 Temmuz"
             , 	"15 arası 13 Temmuz"
             , 	"13 Temmuz - 15 Temmuz"
             , 	"Temmuz 13-15 den"
             , 	"13 ila 15 Temmuz"
             , 	"13 ila 15 Temmuz"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ 	"8 Ağustos-12 Ağustos"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ 	"9:30-11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ 	"- Perşembe günleri 11:00 9:30"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"Perşembe günü 9:30 ile 11:00 arası"
             , 	"09:30 - Perşembe günleri 11:00"
             , 	"Daha sonra 9:30 den ama Perşembe günü saat 11:00 önce"
             , 	"9:30 11:00 Perşembe"
             , 	"Perşembe günü 11:00 e kadar 9:30"
             , 	"09:30 e kadar 11:00 Perşembe"
             , 	"Perşembe günü 11:00 kadar 09:30"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 1, 0, 0), (2013, 2, 13, 2, 31, 0)) Minute)
             [ 	"1-2 arasında yarın: 30 ISH"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ 	"3-4 civarında"
             , 	"3 ila PM 4'e"
             , 	"3-4 civarında"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 30, 0), (2013, 2, 12, 19, 0, 0)) Minute)
             [ 	"3:30-06:00"
             , 	"3: 30-6 pm"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 14, 0, 0)) Hour)
             [ 	"08:00-01:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ 	"9a 11a Perşembe"
             , 	"Bu Per öğleden önce 9-11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ 	"11: 30-1: 30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ 	"Sat, Sep 21 13:30"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ 	"2 hafta içinde"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 14, 0, 0)) Second)
             [ 	"Tarafından 14:00"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 13, 0, 0, 0)) Second)
             [ 	"Tarafından EOD"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 3, 1, 0, 0, 0)) Second)
             [ 	"Tarafından EOM"
             , 	"Tarafından EOM"
             , 	"Ayın sonuna kadar"
             , 	"Ayın sonuna kadar"
             ]
  , examples (datetimeInterval ((2013, 2, 21, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ 	"EOM"
             , 	"EOM"
             , 	"At EOM"
             , 	"ayın sonu"
             , 	"ayın sonu"
             , 	"Ay sonunda"
             ]
  , examples (datetimeInterval ((2013, 2, 1, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ 	"BOM"
             , 	"BOM"
             , 	"At BOM"
             , 	"ayın başlangıcı"
             , 	"Ayın başında"
             , 	"Ayın başında"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 4, 1, 0, 0, 0)) Second)
             [ 	"Bir sonraki ayın sonuna kadar"
             ]
  , examples (datetime (2013, 2, 12, 13, 0, 0) Minute)
             [ 	"16:00 CET"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ 	"Perşembe 08:00 GMT"
             , 	"Perşembe 08:00 gmt"
             , 	"8 GMT Per"
             , 	"8 GMT Per"
             ]
  , examples (datetime (2013, 2, 14, 14, 0, 0) Minute)
             [ 	"Perşembe 08:00 PST"
             , 	"Perşembe 08:00 pst"
             , 	"Per 8 am PST"
             , 	"08:00 pst at Per"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ 	"Bugün 2 de"
             , 	"2 de"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ 	"15:00 yarın"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Minute)
             [ 	"Dek 14:00"
             , 	"Yoluyla 02:00"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ 	"Sonra 14:00"
             , 	"Adresinden 02:00"
             , 	"2pm beri"
             ]
  , examples (datetimeOpenInterval After (2014, 1, 1, 0, 0, 0) Year)
             [ 	"Her zaman 2014 yılından sonra"
             , 	"2014 yılından beri"
             ]
  , examples (datetimeOpenInterval Before (2014, 1, 1, 0, 0, 0) Year)
             [ 	"Bazen 2014'ten önce"
             , 	"2014'e kadar"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ 	"5 gün sonra"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ 	"Önce 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ 	"öğleden sonra"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ 	"08:00 6 çıkar dek"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ 	"At 1:30"
             , 	"01:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ 	"15 dakika içerisinde"
             , 	"' 15'te"
             , 	"15 yılında"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ 	"öğle yemeğinden sonra"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 21, 0, 0)) Hour)
             [ 	"okuldan sonra"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ 	"10:30"
             , 	"Yaklaşık 1030"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ 	"bu sabah"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ 	"gelecek pazartesi"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ 	"12 de"
             , 	"Öğlen"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ 	"12 am"
             , 	"gece yarısında"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ 	"Mart"
             , 	"Martta"
             , 	"Sırasında Mart"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ 	"5 de yarın öğleden sonra"
             , 	"At 5 yarın öğleden sonra"
             , 	"5 de yarın"
             , 	"5 de yarın"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 19, 0, 0)) Hour)
             [ 	"yarın öğleden sonra"
             , 	"Yarın afternoonish"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 13, 0, 0), (2013, 2, 13, 15, 0, 0)) Hour)
             [ 	"13:00-14:00 yarın"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ 	"İlk on"
             , 	"1'inci"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ 	"1030"
             , 	"Etrafında 1030"
             , 	"1030 am"
             ]
  , examples (datetime (2013, 2, 12, 19, 30, 0) Minute)
             [ 	"Akşam 730"
             , 	"Yedi otuz pm"
             ]
  , examples (datetime (2013, 2, 13, 1, 50, 0) Minute)
             [ 	"150ish yarın"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ 	"11 yaşında bu gece"
             ]
  , examples (datetime (2013, 2, 12, 4, 23, 0) Minute)
    -- yes, the result is in the past, we may need to revisit
             [ 	"4:23 at"
             , 	"04:23"
             , 	"Dört yirmi üç bir m"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Day)
             [ 	"Erken Mart"
             ]
  , examples (datetimeInterval ((2013, 3, 11, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ 	"Mart'ın ortası"
             ]
  , examples (datetimeInterval ((2013, 3, 21, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ 	"Geç Mart"
             ]
  , examples (datetimeInterval ((2013, 10, 25, 18, 0, 0), (2013, 10, 28, 0, 0, 0)) Hour)
             [ 	"Ekim ayının son hafta sonu"
             , 	"Ekim ayının son hafta sonu"
             , 	"Ekim ayının son haftasında sonu"
             ]
  , examples (datetimeInterval ((2013, 7, 26, 18, 0, 0), (2013, 7, 29, 0, 0, 0)) Hour)
             [ 	"Temmuz ayının son wkend"
             ]
  , examples (datetimeInterval ((2017, 10, 27, 18, 0, 0), (2017, 10, 30, 0, 0, 0)) Hour)
             [ 	"Ekim 2017 geçen hafta sonu"
             ]
  , examples (datetimeInterval ((2013, 8, 27, 0, 0, 0), (2013, 8, 30, 0, 0, 0)) Day)
             [ 	"27-29 Ağustos"
             , 	"27 Ağustos den - 29"
             ]
  , examples (datetimeInterval ((2013, 10, 23, 0, 0, 0), (2013, 10, 27, 0, 0, 0)) Day)
             [ 	"26 Ekim için 23"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2013, 9, 9, 0, 0, 0)) Day)
             [ 	"1-8 Eylül"
             ]
  , examples (datetimeInterval ((2013, 9, 12, 0, 0, 0), (2013, 9, 17, 0, 0, 0)) Day)
             [ 	"12-16 Eylül"
             ]
  , examples (datetimeInterval ((2013, 8, 19, 0, 0, 0), (2013, 8, 22, 0, 0, 0)) Day)
             [ 	"21 aug için 19"
             ]
  , examples (datetimeInterval ((2013, 4, 21, 0, 0, 0), (2013, 5, 1, 0, 0, 0)) Day)
             [ 	"Nisanın sonu"
             , 	"Nisan ayı sonunda"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2014, 1, 11, 0, 0, 0)) Day)
             [ 	"Ocak başında"
             , 	"Ocak ayı başında"
             ]
  , examples (datetimeInterval ((2012, 9, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Month)
             [ 	"2012 yılının sonuna"
             , 	"2012 yılı sonunda"
             ]
  , examples (datetimeInterval ((2017, 1, 1, 0, 0, 0), (2017, 4, 1, 0, 0, 0)) Month)
             [ 	"2017 başında"
             , 	"2017 yılı başında"
             ]
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Month)
             [ 	"Yılın başlangıcı"
             , 	"Yılın başlangıcı"
             , 	"oğlan"
             , 	"OĞLAN"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2014, 1, 1, 0, 0, 0)) Second)
             [ 	"EOY tarafından"
             , 	"EOY tarafından"
             , 	"Yıl sonuna kadar"
             , 	"Yıl sonuna kadar"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2014, 1, 1, 0, 0, 0)) Month)
             [ 	"EOY"
             , 	"EOY"
             , 	"EOY kısmındaki"
             , 	"yıl sonu"
             , 	"yılın sonu"
             , 	"Yıl sonunda"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 14, 0, 0, 0)) Day)
             [ 	"Bu hafta başında"
             , 	"Mevcut hafta başında"
             , 	"Önümüzdeki hafta başından"
             , 	"Bu hafta başında"
             , 	"Mevcut hafta başında"
             , 	"Önümüzdeki hafta başında"
             ]
  , examples (datetimeInterval ((2013, 2, 4, 0, 0, 0), (2013, 2, 7, 0, 0, 0)) Day)
             [ 	"Geçen hafta başında"
             , 	"Geçtiğimiz hafta başında"
             , 	"Önceki hafta başında"
             , 	"Son hafta başında"
             , 	"Geçtiğimiz hafta başında"
             , 	"Önceki hafta başında"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 2, 21, 0, 0, 0)) Day)
             [ 	"Önümüzdeki hafta başında"
             , 	"Izleyen hafta başında"
             , 	"Etrafında önümüzdeki hafta başında"
             , 	"Önümüzdeki hafta başında"
             , 	"Aşağıdaki hafta başında"
             , 	"Etrafında bir sonraki hafta başında"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
             [ 	"Bu hafta sonu"
             , 	"Mevcut hafta sonu"
             , 	"Haftanın geliyor sonuna"
             , 	"Bu hafta sonunda"
             , 	"Mevcut hafta sonundaki"
             , 	"Önümüzdeki hafta sonunda"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ 	"Geçen hafta sonu"
             , 	"Geçtiğimiz hafta sonu"
             , 	"Önceki hafta sonu"
             , 	"Geçen hafta sonunda"
             , 	"Geçtiğimiz hafta sonunda"
             , 	"Önceki hafta sonundaki"
             ]
  , examples (datetimeInterval ((2013, 2, 22, 0, 0, 0), (2013, 2, 25, 0, 0, 0)) Day)
             [ 	"Önümüzdeki hafta sonu"
             , 	"Izleyen hafta sonu"
             , 	"Civarını gelecek hafta sonu"
             , 	"Önümüzdeki hafta sonunda"
             , 	"Aşağıdaki hafta sonunda"
             , 	"Etrafında bir sonraki hafta sonunda"
             ]
  , examples (datetimeHoliday (2014, 1, 31, 0, 0, 0) Day 	"Çin yeni Yılı")
             [ 	"Çin yeni Yılı"
             , 	"Çince ay Yılbaşı günü"
             ]
  , examples (datetimeHoliday (2013, 2, 10, 0, 0, 0) Day 	"Çin yeni Yılı")
             [ 	"Geçen çin yeni yıl"
             , 	"Son çince Aysal yeni yıl günü"
             ]
  , examples (datetimeHoliday (2018, 2, 16, 0, 0, 0) Day 	"Çin yeni Yılı")
             [ 	"Çin yeni yıl gün 2018"
             ]
  , examples (datetimeHoliday (2018, 9, 18, 0, 0, 0) Day 	"Yom Kippur")
             [ 	"Yom Kippur 2018"
             ]
  , examples (datetimeHoliday (2018, 9, 30, 0, 0, 0) Day 	"Shemini Atzeret")
             [ 	"Shemini Atzeret 2018"
             ]
  , examples (datetimeHoliday (2018, 10, 1, 0, 0, 0) Day 	"Simchat Tevrat")
             [ 	"Tevrat 2018'i Simchat"
             ]
  , examples (datetimeHoliday (2018, 7, 21, 0, 0, 0) Day 	"Tişa B'Av")
             [ 	"Tişa Beav 2018"
             ]
  , examples (datetimeHoliday (2018, 4, 18, 0, 0, 0) Day 	"Yom Ha'atzmaut")
             [ 	"Yom haatzmaut 2018"
             ]
  , examples (datetimeHoliday (2017, 5, 13, 0, 0, 0) Day 	"BaOmer Lag")
             [ 	"2017 Lag B'Omer"
             ]
  , examples (datetimeHoliday (2018, 4, 11, 0, 0, 0) Day 	"Yom HaShoah")
             [ 	"Yom HaShoah 2018"
             , 	"Holokost Günü 2018"
             ]
  , examples (datetimeIntervalHoliday ((2018, 9, 9, 0, 0, 0), (2018, 9, 12, 0, 0, 0)) Day 	"Roş Aşana")
             [ 	"Roşaşana 2018"
             , 	"Roş Aşana 2018"
             , 	"Rosh hashanna 2018"
             ]
  , examples (datetimeIntervalHoliday ((2018, 12, 2, 0, 0, 0), (2018, 12, 10, 0, 0, 0)) Day 	"Hanuka")
             [ 	"Hanuka 2018"
             , 	"Hanukah 2018"
             , 	"Hannukkah 2018"
             ]
  , examples (datetimeIntervalHoliday ((2018, 3, 30, 0, 0, 0), (2018, 4, 8, 0, 0, 0)) Day 	"Fısıh")
             [ 	"Fısıh 2018"
             ]
  , examples (datetimeIntervalHoliday ((2018, 9, 23, 0, 0, 0), (2018, 10, 2, 0, 0, 0)) Day 	"Sukot")
             [ 	"Biraraya toplanması 2018 bayram"
             , 	"2018'i Succos"
             ]
  , examples (datetimeIntervalHoliday ((2018, 5, 19, 0, 0, 0), (2018, 5, 22, 0, 0, 0)) Day 	"Şavuot")
             [ 	"Shavuot 2018"
             ]
  , examples (datetimeHoliday (2017, 11, 30, 0, 0, 0) Day 	"Mevlid")
             [ 	"Mevlit Nabawi 2017"
             ]
  , examples (datetimeHoliday (2018, 6, 15, 0, 0, 0) Day 	"Eid al-fitr")
             [ 	"Ramazan Bayramı 2018"
             ]
  , examples (datetimeHoliday (2018, 8, 21, 0, 0, 0) Day 	"Kurban Bayramı")
             [ 	"Kurban Bayramı 2018"
             , 	"Id Nayramı 2018"
             , 	"Kurban bayram 2018"
             , 	"Bekir Kimliği 2018"
             ]
  , examples (datetimeHoliday (2017, 6, 22, 0, 0, 0) Day 	"Miraç Gecesi")
             [ 	"2017 ark kadr Laylat"
             , 	"Önlemlerin 2017 gecesi"
             ]
  , examples (datetimeHoliday (2018, 6, 11, 0, 0, 0) Day 	"Miraç Gecesi")
             [ 	"Al-Qadr 2018 Laylat"
             , 	"İktidarın 2018 gece"
             ]
  , examples (datetimeHoliday (2018, 9, 11, 0, 0, 0) Day 	"İslami Yeni Yıl")
             [ 	"İslami Yeni Yıl 2018"
             , 	"Amun Cedid 2018"
             ]
  , examples (datetimeHoliday (2017, 9, 30, 0, 0, 0) Day 	"Ashura")
             [ 	"Aşure 2017 günü"
             ]
  , examples (datetimeHoliday (2018, 1, 30, 0, 0, 0) Day 	"Tu BiShvat")
             [ 	"2018 bishvat tu"
             ]
  , examples (datetimeHoliday (2017, 6, 23, 0, 0, 0) Day 	"Jumu'atul-Wida")
             [ 	"Cemaat Ul-Vida 2017"
             , 	"Jumu'atul-Wida 2017"
             ]
  , examples (datetimeHoliday (2018, 6, 8, 0, 0, 0) Day 	"Jumu'atul-Wida")
             [ 	"Cemaat Ul-Vida 2018"
             , 	"Jumu'atul-Wida 2018"
             ]
  , examples (datetimeHoliday (2018, 4, 13, 0, 0, 0) Day 	"Isra ve Miraç")
             [ 	"Isra ve Miraç 2018"
             , 	"Peygamber yükselme 2018"
             ]
  , examples (datetimeHoliday (2019, 4, 3, 0, 0, 0) Day 	"Isra ve Miraç")
             [ 	"Gece yolculuk 2019"
             , 	"Cennet 2019 için yükselme"
             ]
  , examples (datetimeIntervalHoliday ((2018, 5, 16, 0, 0, 0), (2018, 6, 15, 0, 0, 0)) Day 	"Ramazan")
             [ 	"Ramazan 2018"
             ]
  , examples (datetimeHoliday (2017, 10, 17, 0, 0, 0) Day 	"Dhanteras")
             [ 	"2017 yılında dhanatrayodashi"
             ]
  , examples (datetimeHoliday (2019, 10, 25, 0, 0, 0) Day 	"Dhanteras")
             [ 	"Dhanteras 2019"
             ]
  , examples (datetimeHoliday (2019, 10, 26, 0, 0, 0) Day 	"Naraka Chaturdashi")
             [ 	"Kali 2019 chaudas"
             , 	"Choti diwali 2019"
             ]
  , examples (datetimeHoliday (2019, 10, 27, 0, 0, 0) Day 	"Diwali")
             [ 	"Diwali 2019"
             , 	"2019 yılında Deepavali"
             , 	"Lakshmi Puja'nın altı yıl dolayısıyla"
             ]
  , examples (datetimeHoliday (2019, 10, 29, 0, 0, 0) Day 	"Bhai Dooj")
             [ 	"Bhai 2019 dooj"
             ]
  , examples (datetimeHoliday (2019, 11, 2, 0, 0, 0) Day 	"Chhath")
             [ 	"Chhath 2019"
             , 	"Dala puja 2019"
             , 	"2019 yılında Surya Shashthi"
             ]
  , examples (datetimeHoliday (2021, 10, 12, 0, 0, 0) Day 	"Maha Saptami")
             [ 	"Maha Saptami 2021"
             ]
  , examples (datetimeHoliday (2018, 10, 18, 0, 0, 0) Day 	"Vijayadashami")
             [ 	"Dussehra 2018"
             , 	"Beş yıl içinde vijayadashami"
             ]
  , examples (datetimeIntervalHoliday ((2018, 10, 9, 0, 0, 0), (2018, 10, 19, 0, 0, 0)) Day 	"Navaratri")
             [ 	"Navaratri 2018"
             , 	"2018 yılında durga puja"
             ]
  , examples (datetimeHoliday (2018, 8, 26, 0, 0, 0) Day 	"Raksha Bandhan")
             [ 	"Rakhi 2018'i"
             ]
  , examples (datetimeHoliday (2018, 1, 14, 0, 0, 0) Day 	"Tay Pongal")
             [ 	"Pongal 2018"
             , 	"Makara Sankranthi 2018"
             ]
  , examples (datetimeHoliday (2018, 1, 13, 0, 0, 0) Day 	"Boghi")
             [ 	"Bogi pandigai 2018"
             ]
  , examples (datetimeHoliday (2018, 1, 15, 0, 0, 0) Day 	"Mattu Pongal")
             [ 	"Maattu 2018'i Pongal"
             ]
  , examples (datetimeHoliday (2018, 1, 16, 0, 0, 0) Day 	"Kaanum Pongal")
             [ 	"Kaanum Pongal 2018"
             , 	"Kanni Pongal 2018"
             ]
  , examples (datetimeHoliday (2019, 1, 15, 0, 0, 0) Day 	"Tay Pongal")
             [ 	"Makar sankranti 2019"
             , 	"2019 yılında maghi"
             ]
  , examples (datetimeHoliday (2018, 8, 24, 0, 0, 0) Day 	"Thiru Onam")
             [ 	"Onam 2018"
             , 	"Thiru Onam 2018"
             , 	"Thiruvonam 2018"
             ]
  , examples (datetimeHoliday (2019, 2, 10, 0, 0, 0) Day 	"Vasant Panchami")
             [ 	"2019 yılında Vasant Panchami"
             , 	"Basant Panchami 2019"
             ]
  , examples (datetimeHoliday (2019, 3, 20, 0, 0, 0) Day 	"Holika Dahan")
             [ 	"Chhoti holi 2019"
             , 	"Holika dahan 2019"
             , 	"Kamudu ateşi 2019"
             ]
  , examples (datetimeHoliday (2019, 3, 21, 0, 0, 0) Day 	"Kutsal")
             [ 	"Holi 2019"
             , 	"Dhulandi 2019"
             , 	"Phagwah 2019"
             ]
  , examples (datetimeIntervalHoliday ((2013, 4, 26, 0, 0, 0), (2013, 4, 29, 0, 0, 0)) Day 	"Küresel Gençlik Hizmeti Günü")
             [ 	"Gysd 2013"
             , 	"Küresel gençlik hizmet günü"
             ]
  , examples (datetimeHoliday (2013, 5, 24, 0, 0, 0) Day 	"Vesak")
             [ 	"Vesak"
             , 	"Vaisakha"
             , 	"Buda günü"
             , 	"Buda Purnima"
             ]
  , examples (datetimeIntervalHoliday ((2013, 3, 23, 20, 30, 0), (2013, 3, 23, 21, 31, 0)) Minute 	"Dünya Saati")
             [ 	"Dünya Saati"
             ]
  , examples (datetimeIntervalHoliday ((2016, 3, 19, 20, 30, 0), (2016, 3, 19, 21, 31, 0)) Minute 	"Dünya Saati")
             [ 	"Toprak saat 2016"
             ]
  ]
