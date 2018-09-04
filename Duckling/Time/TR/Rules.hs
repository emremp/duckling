-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.TR.Rules
  ( rules
  ) where

import Data.Maybe
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (isNatural, parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("now"          , TG.Second, 0  , "((hemen)\\s*)şimdi|derhal| (şu\\s*an)")
  , ("today"        , TG.Day   , 0  , "bugün|günümüzde"           )
  , ("tomorrow"     , TG.Day   , 1  , "yarın"            )
  , ("yesterday"    , TG.Day   , - 1, "dün"                      )
  ]

  

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
    , pattern =
        [ regex "(şimdi)|(şu ?an)"
        ]
    , prod = \_ -> tt now
  }



  
rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(bu\\s*)?(sabah|öğlen?|akşam|gece)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "sabah"  -> (hour False 4, hour False 12)
              "akşam"  -> (hour False 18, hour False 0)
              "gece"    -> (hour False 18, hour False 0)
              "öğlen"    -> (hour False 12, hour False 14)
              _          -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }
  
ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "week-end"
  , pattern =
    [ regex "hafta\\s*sonu"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "last|this|next <season>"
  , pattern =
    [ regex "(bu|sonraki|son|geçen|önceki) mevsim(ler)?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        n <- case Text.toLower match of
                "bu" -> Just 0
                "son" -> Just (-1)
                "geçen" -> Just (-1)
                "önceki" -> Just (-1)
                "sonraki" -> Just 1
                _ -> Nothing
        tt $ predNth n False season
      _ -> Nothing
  }

ruleDOMLatent :: Rule
ruleDOMLatent = Rule
    { name = "<day-of-month> (ordinal)"
    , pattern = [Predicate isDOMOrdinal]
    , prod = \tokens -> case tokens of
        (token:_) -> do
          n <- getIntValue token
          tt . mkLatent $ dayOfMonth n
        _ -> Nothing
    }
  
ruleTheDOMNumeral :: Rule
ruleTheDOMNumeral = Rule
    { name = "the <day-of-month> (number)"
    , pattern =
      [ regex "the"
      , Predicate isDOMInteger
      ]
    , prod = \tokens -> case tokens of
        (_:token:_) -> do
          n <- getIntValue token
          tt . mkLatent $ dayOfMonth n
        _ -> Nothing
    }
  
ruleTheDOMOrdinal :: Rule
ruleTheDOMOrdinal = Rule
    { name = "the <day-of-month> (ordinal)"
    , pattern =
      [ regex "the"
      , Predicate isDOMOrdinal
      ]
    , prod = \tokens -> case tokens of
        (_:
         Token Ordinal OrdinalData{TOrdinal.value = v}:
         _) -> tt $ dayOfMonth v
        _ -> Nothing
    }
  
ruleNamedDOMOrdinal :: Rule
ruleNamedDOMOrdinal = Rule
    { name = "<named-month>|<named-day> <day-of-month> (ordinal)"
    , pattern =
      [ Predicate $ or . sequence [isAMonth, isADayOfWeek]
      , Predicate isDOMOrdinal
      ]
    , prod = \tokens -> case tokens of
        (Token Time td:token:_) -> Token Time <$> intersectDOM td token
        _ -> Nothing
    }
  
ruleMonthDOMNumeral :: Rule
ruleMonthDOMNumeral = Rule
    { name = "<named-month> <day-of-month> (non ordinal)"
    , pattern =
      [ Predicate isAMonth
      , Predicate isDOMInteger
      ]
    , prod = \tokens -> case tokens of
        (Token Time td:token:_) -> Token Time <$> intersectDOM td token
        _ -> Nothing
    }
  
ruleDOMOfMonth :: Rule
ruleDOMOfMonth = Rule
    { name = "<day-of-month> (ordinal or number) of <named-month>"
    , pattern =
      [ Predicate isDOMValue
      , regex "of|in"
      , Predicate isAMonth
      ]
    , prod = \tokens -> case tokens of
        (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
        _ -> Nothing
    }
  
ruleDOMMonth :: Rule
ruleDOMMonth = Rule
    { name = "<day-of-month> (ordinal or number) <named-month>"
    , pattern =
      [ Predicate isDOMValue
      , Predicate isAMonth
      ]
    , prod = \tokens -> case tokens of
        (token:Token Time td:_) -> Token Time <$> intersectDOM td token
        _ -> Nothing
    }
  
ruleDOMMonthYear :: Rule
ruleDOMMonthYear = Rule
    { name = "<day-of-month>(ordinal or number)/<named-month>/year"
    , pattern =
      [ Predicate isDOMValue
      , regex "[-/\\s]"
      , Predicate isAMonth
      , regex "[-/\\s]"
      , regex "(\\d{4})"
      ]
    , prod = \tokens -> case tokens of
        (token:
         _:
         Token Time td:
         _:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> do
           intVal <- parseInt match
           dom <- intersectDOM td token
           Token Time <$> intersect dom (year intVal)
        _ -> Nothing
    }
  
ruleDOMOrdinalMonthYear :: Rule
ruleDOMOrdinalMonthYear = Rule
    { name = "<day-of-month>(ordinal) <named-month> year"
    , pattern =
      [ Predicate isDOMOrdinal
      , Predicate isAMonth
      , regex "(\\d{2,4})"
      ]
    , prod = \tokens -> case tokens of
        (token:Token Time td:Token RegexMatch (GroupMatch (match:_)):_) -> do
          intVal <- parseInt match
          dom <- intersectDOM td token
          Token Time <$> intersect dom (year intVal)
        _ -> Nothing
    }



ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer", "yaz"      , monthDay  6 21, monthDay  9 23 )
  , ( "fall"  , "sonbahar" , monthDay  9 23, monthDay 12 21 )
  , ( "winter", "kış"      , monthDay 12 21, monthDay  3 20 )
  , ( "spring", "ilkbahar" , monthDay  3 20, monthDay  6 21 )
  ]

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "pazartesi|pzt\\.?" )
  , ( "Tuesday"  , "salı|sal\\.?"      )
  , ( "Wednesday", "çarşamba?|çar\\.?" )
  , ( "Thursday" , "perşembe|per\\.?"  )
  , ( "Friday"   , "cuma|cum\\.?"      )
  , ( "Saturday" , "cumartesi|cmt\\.?" )
  , ( "Sunday"   , "pazar|paz\\.?"     )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonthsWithLatent
  [ ( "January"  , "ocak|oca\\.?"       , False )
  , ( "February" , "şubat|şub\\.?"      , False )
  , ( "March"    , "mart|mar\\.?"       , False )
  , ( "April"    , "nisan|nis\\.?"      , False )
  , ( "May"      , "mayıs|may\\.?"      , False )
  , ( "June"     , "haziran|haz\\.?"    , False )
  , ( "July"     , "temmuz|tem\\.?"     , False )
  , ( "August"   , "ağustos|augağu\\.?" , False )
  , ( "September", "eylül|eyl\\.?"      , False )
  , ( "October"  , "ekim|eki\\.?"       , False )
  , ( "November" , "kasım|kas\\.?"      , False )
  , ( "December" , "aralık|ara\\.?"     , False )
  ]

ruleHHMM :: Rule
ruleHHMM = Rule
    { name = "hh:mm"
    , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)"]
    , prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
          h <- parseInt hh
          m <- parseInt mm
          tt $ hourMinute True h m
        _ -> Nothing
    }

ruleHHMMLatent :: Rule
ruleHHMMLatent = Rule
      { name = "hhmm (latent)"
      , pattern =
        [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)(?!.\\d)"
        ]
      , prod = \tokens -> case tokens of
          (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
            h <- parseInt hh
            m <- parseInt mm
            tt . mkLatent $ hourMinute True h m
          _ -> Nothing
      }
    
  
ruleHHMMSS :: Rule
ruleHHMMSS = Rule
    { name = "hh:mm:ss"
    , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"]
    , prod = \tokens -> case tokens of
        (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
          h <- parseInt hh
          m <- parseInt mm
          s <- parseInt ss
          tt $ hourMinuteSecond True h m s
        _ -> Nothing
    }  
  
ruleMMYYYY :: Rule
ruleMMYYYY = Rule
      { name = "mm/yyyy"
      , pattern =
        [ regex "(0?[1-9]|1[0-2])[/-](\\d{4})"
        ]
      , prod = \tokens -> case tokens of
          (Token RegexMatch (GroupMatch (mm:yy:_)):_) -> do
            y <- parseInt yy
            m <- parseInt mm
            tt $ yearMonthDay y m 1
          _ -> Nothing
      }
    
ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
    { name = "yyyy-mm-dd"
    , pattern =
      [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
      ]
    , prod = \case
        (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
          y <- parseInt yy
          m <- parseInt mm
          d <- parseInt dd
          tt $ yearMonthDay y m d
        _ -> Nothing
    }


ruleDDMM :: Rule
ruleDDMM = Rule
      { name = "dd/mm"
      , pattern =
        [ regex "(3[01]|[12]\\d|0?[1-9])\\s?[/-]\\s?(1[0-2]|0?[1-9])"
        ]
      , prod = \tokens -> case tokens of
          (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
            d <- parseInt dd
            m <- parseInt mm
            tt $ monthDay m d
          _ -> Nothing
      }
    
ruleDDMMYYYY :: Rule
ruleDDMMYYYY = Rule
      { name = "dd/mm/yyyy"
      , pattern =
        [ regex "(3[01]|[12]\\d|0?[1-9])[-/\\s](1[0-2]|0?[1-9])[-/\\s](\\d{2,4})"
        ]
      , prod = \tokens -> case tokens of
          (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
            y <- parseInt yy
            d <- parseInt dd
            m <- parseInt mm
            tt $ yearMonthDay y m d
          _ -> Nothing
      }
    
    -- Clashes with HHMMSS, hence only 4-digit years
ruleDDMMYYYYDot :: Rule
ruleDDMMYYYYDot = Rule
      { name = "dd.mm.yyyy"
      , pattern =
        [ regex "(3[01]|[12]\\d|0?[1-9])\\.(1[0-2]|0?[1-9])\\.(\\d{4})"
        ]
      , prod = \tokens -> case tokens of
          (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
            y <- parseInt yy
            d <- parseInt dd
            m <- parseInt mm
            tt $ yearMonthDay y m d
          _ -> Nothing
      }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 13) n
      _ -> Nothing
  }
      

ruleAtTOD :: Rule
ruleAtTOD = Rule
      { name = "saat <time-of-day>"
      , pattern =
        [ regex "saat|@"
        , Predicate isATimeOfDay
        ]
      , prod = \tokens -> case tokens of
          (_:Token Time td:_) -> tt $ notLatent td
          _ -> Nothing
      }
      
rules :: [Rule]
rules =
  [ 
    ruleNow
    , ruleHHMM
    , ruleHHMMSS
    , rulePartOfDays
    , ruleWeekend
    , ruleSeason
    , ruleDOMLatent
    , ruleTheDOMNumeral
    , ruleTheDOMOrdinal
    , ruleNamedDOMOrdinal
    , ruleMonthDOMNumeral
    , ruleDOMOfMonth
    , ruleDOMMonth
    , ruleDOMMonthYear
    , ruleDOMOrdinalMonthYear
    , ruleMMYYYY
    , ruleYYYYMMDD
    , ruleHHMMLatent
    , ruleTODLatent
    , ruleAtTOD
    , ruleDDMM
    , ruleDDMMYYYY
    , ruleDDMMYYYYDot
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
