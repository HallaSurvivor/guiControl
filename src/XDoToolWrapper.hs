module XDoToolWrapper 
  ( Key(..)
  , sendKey
  , sendKeys
  ) where

import Core
import Control.Monad.IO.Class (liftIO) 
import System.Exit      (ExitCode(..))
import System.Process (readProcessWithExitCode)

data Key = Key Char
         | Alt Key
         | Ctrl Key

safeXDoTool :: [String] -> R ()
safeXDoTool args = do
  (exitcode, stdout, stderr) <- liftIO $ readProcessWithExitCode "xdotool" args ""
  liftIO $ printError exitcode stderr
    where
      printError ExitSuccess _ = return ()
      printError _ stderr      = print stderr

keyToString :: Key -> String
keyToString (Key k) = 
  case k of
    ' '    -> "space"
    '`'    -> "quoteleft"
    '~'    -> "asciitilde"
    '!'    -> "exclam"
    '@'    -> "at"
    '#'    -> "numbersign"
    '$'    -> "dollar"
    '%'    -> "percent"
    '^'    -> "asciicircum"
    '&'    -> "ampersand"
    '*'    -> "asterisk"
    '('    -> "parenleft"
    ')'    -> "parenright"
    '-'    -> "minus"
    '_'    -> "underscore"
    '='    -> "equal"
    '+'    -> "plus"
    '['    -> "bracketleft"
    '{'    -> "braceleft"
    ']'    -> "bracketright"
    '}'    -> "braceright"
    '\\'   -> "backslash"
    '|'    -> "bar"
    ';'    -> "semicolon"
    ':'    -> "colon"
    '\''   -> "quoteright"
    '\"'   -> "quotedbl"
    ','    -> "comma"
    '<'    -> "less"
    '.'    -> "period"
    '>'    -> "greater"
    '/'    -> "slash"
    '?'    -> "question"
    '\n'   -> "Return"
    '\t'   -> "Tab"
    '\DEL' -> "BackSpace"
    '\ESC' -> "Escape"
    _      -> [k]
keyToString (Alt k)  = "alt+" ++ keyToString k
keyToString (Ctrl k) = "ctrl+" ++ keyToString k

sendKeys :: [Key] -> R ()
sendKeys ks = safeXDoTool ["key", keys]
  where
    keys = unwords $ map keyToString ks

sendKey :: Key -> R ()
sendKey = sendKeys . return
