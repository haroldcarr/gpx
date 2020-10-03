{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Gpx where

------------------------------------------------------------------------------
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text             as T
import qualified Prelude
import           Protolude             hiding (state)
import qualified Text.Printf           as TPF
import qualified Xeno.SAX              as XS
import qualified Xeno.Types            as XT
------------------------------------------------------------------------------

--                         end      elev
--(tag       , start elev, previous elev, lowest elev, highest elev, total up, total down)
type Output =
  (ByteString, Double    , Double       , Double     , Double      ,Double   , Double)

gpx :: FilePath -> IO ()
gpx fp = readAndParseGpx fp >>= \case
  Left  e -> panic (show e)
  Right o -> mapM_ putStrLn (formatOutput o)

formatOutput :: Output -> [Text]
formatOutput o@(_,s,p,l,h,u,d) =
  let (_,s',p',l',h',u',d') = metersToFeet o
   in [ fmt "start : " s s'
      , fmt "end   : " p p'
      , fmt "low   : " l l'
      , fmt "high  : " h h'
      , fmt "up    : " u u'
      , fmt "down  : " d d' ]
 where
   fmt t x x' = t <> pf x  <> "m; " <> pf x' <> "ft"
   pf = T.pack . TPF.printf "%.2f"

metersToFeet :: Output -> Output
metersToFeet (x,s,p,l,h,u,d) = (x, f s, f p, f l, f h, f u, f d)
 where f = (*3.2808)

readAndParseGpx :: FilePath -> IO (Either XT.XenoException Output)
readAndParseGpx fp = BS.readFile fp >>= pure . parseGpx

{-# ANN parseGpx ("HLint: ignore Eta reduce" :: Prelude.String) #-}
parseGpx :: ByteString -> Either XT.XenoException Output
parseGpx bs =
  XS.fold
         openTag
         attrKV
         endTag
         textValue
         closeTag
         ("", startValueInit, 0.0, 0.0, 0.0, 0.0, 0.0)
         bs
 where
  openTag  (_t,s,p,l,h,u,d) tag   = -- trace ("open " <> tag)
                                    (tag,s,p,l,h,u,d)

  attrKV    state   _a       _v   = -- trace ("attr " <> a <> " " <> v)
                                    state

  endTag    state          _tag   = -- trace ("end " <> tag)
                                    state

  textValue a@(t,s,p,l,h,u,d) val = -- trace ("text " <> t <> " " <> val) $
    if | t == "ele" && s == startValueInit -- starting value and save as current value
         -> (t,val',val',val',val',u,d)
       | t == "ele" && val' == p           -- no change
         -> a
       | t == "ele" && val' <  p           -- down
         -> (t,s,val', min l val', max h val', u             ,d + (p - val'))
       | t == "ele" && val' >  p           -- up
         -> (t,s,val', min l val', max h val', u + (val' - p),d)
       | otherwise
         -> a
   where val' = Prelude.read @Double (BSC8.unpack val)

  closeTag  (_,s,p,l,h,u,d)  _tag = -- trace ("close " <> tag)
                                ("",s,p,l,h,u,d)

  startValueInit = -99999.99999
