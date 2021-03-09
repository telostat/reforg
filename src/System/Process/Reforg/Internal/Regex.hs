{-# LANGUAGE FlexibleContexts #-}

module System.Process.Reforg.Internal.Regex where

import           Control.Monad.Except        (MonadError, throwError)
import qualified Data.Aeson                  as Aeson
import           Data.Bifunctor              (first, second)
import qualified Data.HashMap.Strict         as HM
import           Data.Maybe                  (isJust)
import qualified Data.Text                   as T
import qualified Text.Regex.PCRE.Light.Char8 as Re


-- | Data definition for regular expressions based on 'Re.Regex'.
--
-- In Reforg, we are working with groups of named matches (and completely ignore
-- unnamed matches, although a 'Just' match with no groups in the regular
-- expression is still a match). This is a fundamental assumption the rest of
-- the primitives are defined on this assumption.
data Regex = MkRegex
  { regexRe    :: !Re.Regex
  , regexNames :: ![(T.Text, Int)]
  } deriving Show


-- | 'Aeson.FromJSON' instance for 'Regex'.
instance Aeson.FromJSON Regex where
  parseJSON = Aeson.withText "Regex" (either fail pure . mkRegex . T.unpack)


-- | Smart constructor for 'Regex' that operates in 'MonadError' context.
--
-- >>> mkRegex "" :: Either String Regex
-- Right (MkRegex {regexRe = Regex 0x00007fc16290d8c0 "", regexNames = []})
-- >>> mkRegex ".*" :: Either String Regex
-- Right (MkRegex {regexRe = Regex 0x00007fc16290d910 ".*", regexNames = []})
-- >>> mkRegex "(.*)" :: Either String Regex
-- Right (MkRegex {regexRe = Regex 0x00007fc16290d970 "(.*)", regexNames = []})
-- >>> mkRegex "(?P<name>.*)" :: Either String Regex
-- Right (MkRegex {regexRe = Regex 0x00007fc16290d9d0 "(?P<name>.*)", regexNames = [("name",0)]})
-- >>> mkRegex "(?P<name1>[a-z]+)([0-9]+)(?P<name2>[A-Z]+)" :: Either String Regex
-- Right (MkRegex {regexRe = Regex 0x00007fc16290da30 "(?P<name1>[a-z]+)([0-9]+)(?P<name2>[A-Z]+)", regexNames = [("name1",0),("name2",2)]})
-- >>> mkRegex "[a-z" :: Either String Regex
-- Left "Regular expression compilation error: missing terminating ] for character class"
mkRegex :: MonadError String m => String -> m Regex
mkRegex = either mkErr mkRegexAux . flip Re.compileM []
  where
    mkRegexAux re = pure $ MkRegex re (first T.pack <$> Re.captureNames re)
    mkErr = throwError . (<>) "Regular expression compilation error: "


-- | Attempts to match a given 'String' to a given 'Regex'.
--
-- The result is a Nothing if 'Re.match' fails to match, 'Just' named group
-- matches otherwise.
--
-- If the regular experssion has no named groups but the match is successful,
-- the result value is still a 'Just' but of an 'HM.empty' value.
--

-- >>> import Data.Either (fromRight)
-- >>> let re1 = either (error "no") id $ mkRegex "(?P<name1>[a-z]+)([0-9]+)(?P<name2>[A-Z]+)"
-- >>> match re1 "aa00AA"
-- Just (fromList [("name2","AA"),("name1","aa")])
-- >>> let re2 = either (error "no") id $ mkRegex ".*[0-9]+.*"
-- >>> match re2 "aa00AA"
-- Just (fromList [])
match :: Regex -> String -> Maybe (HM.HashMap T.Text T.Text)
match r x = group <$> Re.match re x []
  where
    re = regexRe r
    group gvs = HM.fromList $ fmap (second (T.pack . (!!) gvs . (+) 1)) (regexNames r)


-- | (Quickly) checks if the given 'String' matches the given 'Regex'.
--
-- >>> import Data.Maybe (fromJust)
-- >>> let re1 = either (error "no") id $ mkRegex "(?P<name1>[a-z]+)([0-9]+)(?P<name2>[A-Z]+)"
-- >>> isMatch re1 "aa00AA"
-- True
-- >>> let re2 = either (error "no") id $ mkRegex ".*[0-9]+.*"
-- >>> isMatch re2 "aa00AA"
-- True
-- >>> let re3 = either (error "no") id $ mkRegex ".*[0-9]+.*"
-- >>> isMatch re3 "aa--AA"
-- False
isMatch :: Regex -> String -> Bool
isMatch r x = isJust $ Re.match (regexRe r) x []
