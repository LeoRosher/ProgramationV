{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module AbstractGrammar where
import GHC.Show (Show)

type Strings = String
type Paragraph = String
type H1 = String
type H2 = String
type H3 = String
type H4 = String
type H5 = String
type H6 = String

data Slides = Slides[Slide] deriving Show

data Slide = Slide TitleSlide BodySlide deriving Show

data TitleSlide = TitleSlide Strings deriving Show

data BodySlide = BodySlide [MarkdownBlock] deriving Show

data Cad = Cad Strings deriving Show

data MarkdownBlock = MdParagraph Paragraph
                    | MdH1 H1
                    | MdH2 H2
                    | MdH3 H3
                    | MdH4 H4
                    | MdH5 H5
                    | MdH6 H6
                    | Bold Paragraph
                    | Italic Paragraph
                    | UnorderedList [Paragraph]
                    deriving Show


getSlides :: Slides -> [Slide]
getSlides (Slides slides) = slides

getTitleSlide :: Slide -> TitleSlide
getTitleSlide (Slide titleSlide _) = titleSlide

getTitleContent :: TitleSlide -> String
getTitleContent (TitleSlide title) = title 

getBodySlide :: Slide -> BodySlide
getBodySlide (Slide _ bodySlide) = bodySlide

getMarkdownBlocks :: BodySlide -> [MarkdownBlock]
getMarkdownBlocks (BodySlide markdownBlocks) = markdownBlocks

getMdParagraph :: MarkdownBlock -> Paragraph
getMdParagraph (MdParagraph paragraph) = paragraph

getMdH1 :: MarkdownBlock -> H1
getMdH1 (MdH1 h1) = h1

getMdH2 :: MarkdownBlock -> H2
getMdH2 (MdH2 h2) = h2

getMdH3 :: MarkdownBlock -> H3
getMdH3 (MdH3 h3) = h3

getMdH4 :: MarkdownBlock -> H4
getMdH4 (MdH4 h4) = h4

getMdH5 :: MarkdownBlock -> H5
getMdH5 (MdH1 h5) = h5

getMdH6 :: MarkdownBlock -> H6
getMdH6 (MdH1 h6) = h6

getBold :: MarkdownBlock -> Paragraph
getBold (Bold boldParagraph) = boldParagraph

getItalic :: MarkdownBlock -> Paragraph
getItalic (Italic italicParagraph) = italicParagraph

getUnorderedList :: MarkdownBlock -> [Paragraph]
getUnorderedList (UnorderedList list) = list