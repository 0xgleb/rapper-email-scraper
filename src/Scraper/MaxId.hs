module Scraper.MaxId
  ( extractMaxId
  )
  where

import Protolude

import qualified Data.Text                            as Txt
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Types                    as Twitter

data BreakMode
  = BreakOnStart
  | BreakOnEnd

extractMaxId :: Twitter.URIString -> Maybe Twitter.PV
extractMaxId
  =   fmap Twitter.PVInteger . readMaybe . Txt.unpack
  <=< search BreakOnStart "&"
  <=< search BreakOnEnd "max_id="

  where
    search breakMode searchPart txt
      = let breakFunc = case breakMode of
              BreakOnStart -> Txt.breakOn
              BreakOnEnd   -> Txt.breakOnEnd

            (before, after) = breakFunc searchPart txt

        in case breakMode of
             BreakOnEnd ->
              if Txt.drop (Txt.length before - Txt.length searchPart) before == searchPart
              then Just after
              else Nothing

             BreakOnStart ->
              if searchPart == Txt.take (Txt.length searchPart) after
              then Just before
              else Nothing
