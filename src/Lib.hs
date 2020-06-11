module Lib
    ( startWindow
    ) where


import Graphics.UI.Gtk 
import System.IO



parseCommitFile filepath = do 
  content <- readFile filepath
  let linesOfFile = lines content

  print ""



startWindow :: String -> IO ()
startWindow filepath = do
  initGUI
  window <- windowNew
  on window objectDestroy mainQuit
  set window [ 
      containerBorderWidth := 100
    , windowTitle := "Hello World"
    , windowResizable := False]

  button <- buttonNew
  set button [ buttonLabel := "Commit"]
  on button buttonActivated $ do
    putStrLn "A \"clicked\"-handler to say \"destroy\""
    widgetDestroy window

  text <- labelNew (Just "Title")
  -- set text [frameLabelXAlign := 0]
  miscSetAlignment text 0 0.5


  commitMsg <- entryNew
  -- For some reason entryWidthChars adds 8 chars.
  set commitMsg [
      entryWidthChars := 64
    , entryMaxLength := 72
    -- , entrySelectionBound := readAttr 1
    -- , entryActivate := Signal (True, commitMsg, (\s -> s))
    ]
  -- commitMsg `on` entryActivate $ set commitMsg [entryText := "This should trigger commit"]

  -- commitMsg `on` entryInsertAtCursor $ putStrLn
  -- commitMsg `on` entryBackspace  $ print "HI"
  -- commitMsg `on` entryCopyClipboard $ print "HI"
  -- commitMsg `on` entryPreeditChanged  $ (\s -> putStrLn $ "Hello " ++ s) 


  textfield <- textViewNew 
  set textfield [
      textViewWrapMode    := WrapWord 
    , textViewAcceptsTab  := False
    , textViewLeftMargin  := 5
    , textViewRightMargin := 5
    , textViewPixelsAboveLines := 2
    ]
  
  textViewSetBorderWindowSize textfield TextWindowTop    10
  textViewSetBorderWindowSize textfield TextWindowBottom 10

  -- texttable <- textTagTableNew
  -- textTagTableAdd self tag
  textbuffer <- textBufferNew Nothing
  iter <- textBufferGetStartIter textbuffer
  -- textBufferInsert textbuffer iter "\n"
  -- textBufferInsert textbuffer iter "\n"
  -- textBufferInsert textbuffer iter "\n"
  textViewBackwardDisplayLineStart textfield iter
  -- iter <- textBufferGetStartIter textbuffer

  textViewSetBuffer textfield textbuffer  

  
  box <- vBoxNew False 10
  containerAdd box text
  containerAdd box commitMsg
  containerAdd box textfield
  containerAdd box button

  set window [ containerChild := box ]
  -- set window [ containerChild := text ]


  widgetShowAll window
  mainGUI -- main loop