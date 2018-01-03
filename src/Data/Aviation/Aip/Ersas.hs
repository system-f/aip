{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Aviation.Aip.Ersas(
  Ersas(..)
, parseAipTree
) where

import Data.Aviation.Aip.Ersa(Ersa(Ersa))
import Data.Aviation.Aip.AipDate(parseAipDate)
import Data.Aviation.Aip.AipHref(parseAipHref)
import Text.HTML.TagSoup(Tag(TagText))
import Text.HTML.TagSoup.Tree(TagTree(TagBranch, TagLeaf), parseTree)
import Text.HTML.TagSoup.Tree.Util(htmlRoot)
import Text.HTML.TagSoup.Tree.Zipper(TagTreePos(TagTreePos), traverseTree, fromTagTree)
import Text.Parsec(Parsec, Stream, parse)
import Text.Parser.Char(space, char)
import Text.Parser.Combinators(between)
import Papa

newtype Ersas =
  Ersas
    [Ersa]
  deriving (Eq, Ord, Show)

makeWrapped ''Ersas

instance Monoid Ersas where
  mempty =
    Ersas
      mempty
  Ersas x `mappend` Ersas y =
    Ersas (x `mappend` y)

parseAipTree ::
  String
  -> Ersas
parseAipTree =
  let runParse ::
        Stream s Identity t =>
        Parsec s () a
        -> s
        -> Maybe a
      runParse p s =
        parse p "aip" s ^? _Right
      aipTreeTraversal ::
        TagTreePos String
        -> Ersas
      aipTreeTraversal t =
        case t of
          TagTreePos (TagBranch "li" [] (TagBranch "a" [("href", href)] [TagLeaf (TagText n)]:TagLeaf (TagText tx):_)) _ _ _ ->
            let pdate = do  _ <- space
                            between (char '(') (char ')') parseAipDate
            in  case n of
                  "En Route Supplement Australia (ERSA)" ->
                    let p = do  h <- runParse parseAipHref href
                                d <- runParse pdate tx
                                pure (Ersas [Ersa h d])
                    in  fromMaybe mempty p
                  _ ->
                    mempty
          _ ->
            mempty
  in  traverseTree aipTreeTraversal . fromTagTree . htmlRoot . parseTree
