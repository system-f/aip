module Data.Aviation.Aip where

import Network.Stream
import Network.HTTP
import Network.URI
import Prelude
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup.Tree.Util
import Text.HTML.TagSoup.Tree.Zipper
import Control.Monad.Trans.Except

request ::
  Request String
request = 
  let uri =
        URI "http:" (Just (URIAuth "" "www.airservicesaustralia.com" "")) "/aip/aip.asp" "?pg=10" ""
      headers =
        [
        ]
  in  setRequestBody (setHeaders (mkRequest POST uri) headers) ("application/x-www-form-urlencoded", "Submit=I+Agree&check=1")

aipTree ::
  ExceptT ConnError IO [TagTree String]
aipTree =
   ExceptT ((parseTree . rspBody <$>) <$> simpleHTTP request)

aipTreePos ::
  ExceptT ConnError IO (TagTreePos String)
aipTreePos =
  fromTagTree . htmlRoot <$> aipTree
