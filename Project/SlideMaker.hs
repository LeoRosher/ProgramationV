module SlideMaker where
import AbstractGrammar
import System.IO
import Scanner(scanner)
import UU.Parsing
import Parser


title = "Slides"
index = 0


createFile :: Slides -> String
createFile slides = "<!DOCTYPE html>" ++ "\n" ++
                    "<html>" ++ "\n" ++
                    createHead ++ "\n" ++
                    createBody slides ++ "\n" ++
                    "</html>" ++ "\n"


createHead :: String
createHead = "<head>" ++ "\n" ++
            "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"++ "\n" ++
            "<title>"++ show title ++"</title>"++ "\n" ++
            "<link rel=\"stylesheet\" href=\"slides.css\">"++ "\n" ++
            "<script src=\"https://kit.fontawesome.com/887a835504.js\" crossorigin=\"anonymous\"></script>"++ "\n" ++
            "</head>"

createBody :: Slides -> String
createBody slides =  "<body>" ++ "\n" ++
                        "<div class=\"carrousel\">" ++ "\n" ++
                        "<div class=\"contentCarrousel\">" ++ "\n" ++
                        createSlides (getSlides slides) 1 ++
                        "</div>" ++ "\n" ++
                        "</div>" ++ "\n" ++
                        "</body>" ++ "\n"

createSlides :: [Slide] -> Int -> String
createSlides [] _ = ""
createSlides (x:xs) index = createSlide x index ++ createSlides xs (index + 1)

createSlide :: Slide -> Int -> String
createSlide slide index = "<div class=\"itemCarrousel\" id=\"itemCarrousel-" ++ show index ++ "\">"++ "\n" ++
                                "<div class=\"itemCarrouselTarjeta\">"++ "\n" ++
                                processSlideContent slide ++
                                "</div>"++ "\n" ++
                                "<div class=\"itemCarrouselArrows\">"++ "\n" ++
                                "<a class=\"arrowContainer\" href=\"#itemCarrousel-"++ show (index - 1) ++ "\">" ++ "\n" ++
                                    "<img src=\"resources/chevron-left.svg\"></img>"++ "\n" ++
                                "</a>"++ "\n" ++
                                "<a class=\"arrowContainer\" href=\"#itemCarrousel-"++ show (index + 1) ++"\">"++ "\n" ++
                                    "<img src=\"resources/chevron-right.svg\"></img>"++ "\n" ++
                                "</a>"++ "\n" ++
                                "</div>"++ "\n" ++
                            "</div>" ++ "\n"

createParagraph :: Paragraph -> String
createParagraph paragraph = "<p>" ++ paragraph ++ "</p>" ++ "\n"

createH1 :: H1 -> String
createH1 h1 = "<h1>" ++ h1 ++ "</h1>" ++ "\n"

createH2 :: H2 -> String
createH2 h2 = "<h2>" ++ h2 ++ "</h2>" ++ "\n"

createH3 :: H3 -> String
createH3 h3 = "<h3>" ++ h3 ++ "</h3>" ++ "\n"

createH4 :: H4 -> String
createH4 h4 = "<h4>" ++ h4 ++ "</h4>" ++ "\n"

createH5 :: H5 -> String
createH5 h5 = "<h5>" ++ h5 ++ "</h5>" ++ "\n"

createH6 :: H6 -> String
createH6 h6 = "<h6>" ++ h6 ++ "</h6>" ++ "\n"

createBold :: Paragraph -> String
createBold bold = "<b>" ++ bold ++ "</b>" ++ "\n"

createItalic :: Paragraph -> String
createItalic italic = "<i>" ++ italic ++ "</i>" ++ "\n"

createBoldAndItalic :: Paragraph -> String
createBoldAndItalic boldAndItalic = "<b>" ++ "\n" ++
                                    "<i>" ++ boldAndItalic ++ "</i>" ++ "\n" ++
                                    "</b>" ++ "\n"

createTitleSlide :: TitleSlide -> String
createTitleSlide title = "<h1 style=\"text-align: center;\">" ++ getTitleContent title ++ "</h1>" ++ "\n"

createList :: [Paragraph] -> String
createList list = "<ul>"++ createElemList list ++"</ul>"

createElemList :: [String] -> String
createElemList [] = ""
createElemList (x:xs) = "<li>" ++ x ++ "</li>" ++ "\n" ++ createElemList xs

createLink :: Paragraph -> String
createLink link = "<a href=\""++ link ++"\">"++ link ++"</a>" ++ "\n"

createImg :: Paragraph -> String
createImg img = "<img src=\"" ++ img ++ "\">" ++"</img>" ++ "\n"

createLineBreak :: Paragraph -> String
createLineBreak br = "<br>" ++ "\n"

createBodyContent :: [MarkdownBlock] -> String
createBodyContent [] = ""
createBodyContent (x:xs) = createElem x ++ createBodyContent xs

createElem :: MarkdownBlock -> String
createElem (MdParagraph paragraph) = createParagraph paragraph
createElem (MdH1 h1) = createH1 h1
createElem (MdH2 h2) = createH2 h2
createElem (MdH3 h3) = createH3 h3
createElem (MdH4 h4) = createH4 h4
createElem (MdH5 h5) = createH5 h5
createElem (MdH6 h6) = createH6 h6
createElem (Bold bold) = createBold bold
createElem (Italic italic) = createItalic italic
createElem (BoldAndItalic boldAndItalic) = createBoldAndItalic boldAndItalic
createElem (UnorderedList list) = createList list
createElem (Link link) = createLink link
createElem (Img img) = createImg img
createElem (Break br) = createLineBreak br

processSlideContent :: Slide -> String
processSlideContent slide = let tittle = getTitleSlide slide
                                body = getBodySlide slide
                                contentSlide = getMarkdownBlocks body
                            in
                                createTitleSlide tittle ++ createBodyContent contentSlide




main :: IO ()
main = do
    putStrLn "Introduce the .p5 file: "
    p5 <- getLine
    input <- readFile p5

    let token = scanner input
    tree <- parseIO pSlides token
    putStrLn "Introduce the name of the file: "
    name <- getLine

    let fileName = name ++ ".html"
    handle <- openFile fileName WriteMode
    
    hPutStrLn handle (createFile tree)

    hClose handle
    
    putStrLn $ fileName ++ " created succesfully"
