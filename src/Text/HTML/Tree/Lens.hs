{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module Text.HTML.Tree.Lens where

import Control.Lens
import Control.Lens.TH
import Data.HashMap.Strict (HashMap)
import Data.Tree
import Prelude hiding (all)
import Text.HTML.Parser
import Text.HTML.Tree

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

type Node = Tree Token

parseFile :: FilePath -> IO [Token]
parseFile fp = parseTokensLazy <$> LT.readFile fp

parseFileTree :: FilePath -> IO (Either ParseTokenForestError [Node])
parseFileTree fp = tokensToForest <$> parseFile fp

htmlTokens :: Iso' LT.Text [Token]
htmlTokens = iso parseTokensLazy renderTokens 

htmlTree :: Prism' [Token] [Node]
htmlTree = prism' tokensFromForest (either (const Nothing) Just . tokensToForest)

html :: Prism' LT.Text [Node]
html = htmlTokens . htmlTree

nodeChildren :: Lens' Node [Node]
nodeChildren = lens subForest (\t cs -> t { subForest = cs })

instance Plated [Tree Token] where
    plate = traverse.nodeChildren

nodeToken :: Lens' Node Token
nodeToken = lens rootLabel (\t l -> t { rootLabel = l })

xall :: Plated a => Fold a b -> Fold a b
xall f = to universe . traverse . f

_Text :: Prism' Token T.Text
_Text = prism' ContentText $ \case
    ContentText s -> Just s
    _             -> Nothing

data Tag = Tag
  { _tagName :: TagName
  , _tagAttrs :: HashMap AttrName AttrValue
  } deriving Show

makeLenses ''Tag

_Tag :: Prism' Token Tag
_Tag = prism' tag2token token2tag
    where tag2token (Tag n attrsMap) = TagOpen n $ map (\(k, v) -> Attr k v) (HM.toList attrsMap)
          token2tag (TagOpen n attrsList) = Just . Tag n . HM.fromList $ map (\(Attr k v) -> (k, v)) attrsList
          token2tag (TagSelfClose n attrsList) = Just . Tag n . HM.fromList $ map (\(Attr k v) -> (k, v)) attrsList
          token2tag _ = Nothing

allTexts :: Fold Node T.Text
allTexts = xall (nodeToken._Text)

allTags :: Fold Node Tag
allTags = xall (nodeToken._Tag)

allWhere :: Plated a => (a -> Bool) -> Fold a a
allWhere p = xall (filtered p)

allNamed :: T.Text -> Fold Node Node
allNamed s = allWhere $ \n -> (n ^? nodeToken._Tag.tagName) == Just s

allText :: Fold Node T.Text
allText = xall (nodeToken._Text)

allAttributed :: [(AttrName, AttrValue)] -> Fold Node Node
allAttributed attrs = allWhere $ \n -> HM.fromList attrs `HM.isSubmapOf` (n ^. nodeToken._Tag.tagAttrs)


