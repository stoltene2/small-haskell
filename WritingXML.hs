#!/usr/bin/env stack
-- stack script --resolver lts-8.12 --package xml-conduit --package xml-hamlet --package containers --package text --package http-types

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Data.Map (empty, fromList)
import           Prelude  hiding (writeFile)
import           Data.Text hiding (empty, null)
import           Data.Text.Encoding
import           Text.Hamlet.XML
import           Text.XML
import           Network.HTTP.Types (urlEncode)

{-
Resources

* https://www.sitemaps.org/protocol.html
* https://www.yesodweb.com/book/xml

Things to do:
- Adhere to Google's limits about number of entries in page
    - https://support.google.com/webmasters/answer/183668?hl=en&ref_topic=4581190
- Make available to google by adding it to the robot.txt file
    - Sitemap: http://example.com/sitemap_location.xml
- robots.txt
    - https://en.wikipedia.org/wiki/Robots_exclusion_standard
-}


data Link = Link
  { location :: Text
  , lastMod :: Text -- To become Maybe
  , changeFrequency :: Text -- To become Maybe
  , priority :: Double -- To become Maybe
  } deriving (Show)

--------------------------------------------------------------------------------
-- Example links
links :: [Link]
links = [Link "http://www.example.com/ümlat.php&q='<name>'" "2017-01-25" "monthly" 0.5]

--------------------------------------------------------------------------------
-- How to render a url node, this can be cleaned up

urlNode :: Link -> [Node]
urlNode link = [xml|
<url>
    <loc>#{location link}
    <lastmod>#{decodeUtf8 $ urlEncode True $ encodeUtf8 $ lastMod link}
    <changefreq>#{changeFrequency link}
    <priority>#{pack $ show $ priority $ link}
|]

--------------------------------------------------------------------------------
-- Outputs document to a test3.xml file

main :: IO ()
main = writeFile def "test3.xml" $ Document (Prologue [] Nothing []) root []
  where
    root = Element "url-set" (fromList [("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9") ]) [xml|

$if null links
$else
    $forall link <- links
        ^{urlNode link}
|]
