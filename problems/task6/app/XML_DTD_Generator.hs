module Main where

import Text.XML.Light

data Book = Book { title :: String, author :: String } deriving (Show)

books :: [Book]
books = [ Book "Island" "Aldous Huxley",
          Book "1984" "George Orwell",
          Book "Brave New World" "Aldous Huxley" ]

bookToXML :: Book -> Element
bookToXML (Book t a) =
    unode "book" [ unode "title" t, unode "author" a ]

root :: Element
root = unode "books" (map bookToXML books)

-- Generate the XML content
generateXML :: String
generateXML = ppTopElement root

-- Generate the DTD content
generateDTD :: String
generateDTD = unlines
    [ "<!ELEMENT books (book*)>"
    , "<!ELEMENT book (title, author)>"
    , "<!ELEMENT title (#PCDATA)>"
    , "<!ELEMENT author (#PCDATA)>"
    ]

-- Main function to print both XML and DTD
main :: IO ()
main = do
    putStrLn "XML Content:"
    putStrLn generateXML
    putStrLn "\nDTD Content:"
    putStrLn generateDTD
