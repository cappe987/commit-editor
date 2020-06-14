module Lib
    ( startWindow
    , parseCommitFile
    ) where


import Graphics.UI.Gtk 
import System.IO
import Data.List as List
import Data.Text as Text
import Control.Monad

data CommitInfo = CommitInfo {
    branch :: String
  , origin :: String
  }

-- parseCommitFile :: String -> IO ()
parseCommitFile :: String -> IO CommitInfo
parseCommitFile filepath = do 
  content <- readFile filepath
  let linesOfFile = List.lines content

  let branchLine = List.dropWhile (\s -> not $ "On branch" `List.isInfixOf` s) linesOfFile
  let branch = List.drop 12 $ List.head branchLine -- 13 is where the branch name starts

  --35 is where remote name starts
  let originBranch = List.takeWhile (/= '.') $ List.drop 34 $ List.head $ List.tail branchLine 
  let originMapped = List.map (\c -> if c == '\'' then '/' else c) originBranch
  let originFormatted = if List.null originMapped then "" else List.init originMapped

  return $ CommitInfo {branch = branch, origin = originFormatted}



sendCommit :: String -> Entry -> TextView -> IO Bool
sendCommit path msg comments = do
  text <- entryGetText msg :: IO String
  commentText <- textViewGetBuffer comments

  startIter <- textBufferGetStartIter commentText
  endIter   <- textBufferGetEndIter commentText
  commentString <- textBufferGetText commentText startIter endIter False :: IO String

  let fullCommit = "\n" ++ text ++ "\n" ++ commentString


  let t = Text.strip $ Text.pack text
  if unpack t == "" then 
    do 
      putStrLn "No commit message..."
      return False
  else
    do
      putStrLn "Committing..."
      appendFile path fullCommit
      return True



startWindow :: String -> IO ()
startWindow filepath = do

  commitInfo <- parseCommitFile filepath 

  initGUI

  -- Window
  window <- windowNew
  on window objectDestroy mainQuit
  set window [ 
      containerBorderWidth := 100
    , windowTitle := "Hello World"
    , windowResizable := False]



  -- Branch information
  branchLabel <- 
    labelNew (Just $ "Branch: " ++ branch commitInfo ++ "\nRemote: " ++ origin commitInfo)
  miscSetAlignment branchLabel 0 0.5

  text <- labelNew (Just "Title")
  miscSetAlignment text 0 0.5


  -- Title of commit
  commitMsg <- entryNew
  -- For some reason entryWidthChars adds 8 chars.
  set commitMsg [
      entryWidthChars := 64
    , entryMaxLength := 72
    ]


  -- Comment field
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



  -- Commit button
  button <- buttonNew
  set button [ buttonLabel := "Commit"]
  -- Trigger sending the commit
  on button buttonActivated $ do 
    res <- sendCommit filepath commitMsg textfield 
    Control.Monad.when res $ widgetDestroy window


  -- Vertical box holding all the widgets
  box <- vBoxNew False 10
  containerAdd box branchLabel
  containerAdd box text
  containerAdd box commitMsg
  containerAdd box textfield
  containerAdd box button

  set window [ containerChild := box ]

  

  widgetShowAll window
  mainGUI -- main loop