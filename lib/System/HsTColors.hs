--------------------------------------------------------------------------------
-- |
-- Module: System.HsTColors
-- Note: A minimal library which contains some small and simple functions to
-- make/print strings with terminal colors (and more).
--
-- Based on: https://github.com/schell/steeloverseer/blob/master/src/ANSIColors.hs
--
--------------------------------------------------------------------------------
module System.HsTColors
       where

import Debug.Trace (trace)
import Data.Maybe (fromJust)
import Data.Typeable (typeOf, cast, Typeable)

class (Show a) => HasANSICode a where
  toANSICode :: a -> String
  showANSICode :: a -> String
  showANSICode a = "\27[" ++ toANSICode a ++ "m"

--------------------------------------------------------------------------------
data ANSINone = ANSINone deriving (Ord,Eq)
instance HasANSICode ANSINone where
  toANSICode ANSINone = "0"
instance Show ANSINone where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSICode = ANSICode String deriving (Ord,Eq)
instance HasANSICode ANSICode where
  toANSICode (ANSICode s) = s
instance Show ANSICode where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSIColor = ANSIBlack
               | ANSIRed
               | ANSIGreen
               | ANSIYellow
               | ANSIBlue
               | ANSIMagenta
               | ANSICyan
               | ANSIWhite
               deriving (Ord,Eq)
instance HasANSICode ANSIColor where
  toANSICode ANSIBlack   = "30"
  toANSICode ANSIRed     = "31"
  toANSICode ANSIGreen   = "32"
  toANSICode ANSIYellow  = "33"
  toANSICode ANSIBlue    = "34"
  toANSICode ANSIMagenta = "35"
  toANSICode ANSICyan    = "36"
  toANSICode ANSIWhite   = "37"
instance Show ANSIColor where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSIBkColor = ANSIBkBlack
                 | ANSIBkRed
                 | ANSIBkGreen
                 | ANSIBkYellow
                 | ANSIBkBlue
                 | ANSIBkMagenta
                 | ANSIBkCyan
                 | ANSIBkWhite
                 deriving (Ord,Eq)
instance HasANSICode ANSIBkColor where
  toANSICode ANSIBkBlack   = "40"
  toANSICode ANSIBkRed     = "41"
  toANSICode ANSIBkGreen   = "42"
  toANSICode ANSIBkYellow  = "43"
  toANSICode ANSIBkBlue    = "44"
  toANSICode ANSIBkMagenta = "45"
  toANSICode ANSIBkCyan    = "46"
  toANSICode ANSIBkWhite   = "47"
instance Show ANSIBkColor where
  show = showANSICode

--------------------------------------------------------------------------------
data ANSIDecoration = ANSIBold
                    | ANSICursive
                    | ANSIUnderline
                    | ANSIBlink
                    | ANSIBackground
                    | ANSICancel
                    deriving (Ord,Eq)
instance HasANSICode ANSIDecoration where
  toANSICode ANSIBold       = "1"
  toANSICode ANSICursive    = "3"
  toANSICode ANSIUnderline  = "4"
  toANSICode ANSIBlink      = "5"
  toANSICode ANSIBackground = "7"
  toANSICode ANSICancel     = "9"
instance Show ANSIDecoration where
  show = showANSICode

--------------------------------------------------------------------------------
-- Basic functions
-- |The 'uncolor' function returns a String without the coloring
uncolor :: String -> String
uncolor ('\27':('[':(_:('m':ss))))     = uncolor ss
uncolor ('\27':('[':(_:(_:('m':ss))))) = uncolor ss
uncolor (s:ss)                         = s : uncolor ss
uncolor ""                             = ""

-- | the 'uncolLength' returns the real length of an strnig, ignoring the coloring
uncolLength :: String -> Int
uncolLength = length . uncolor

-- |The 'uncolor' function returns a String without the coloring
isColored :: String -> Bool
isColored ('\27':('[':(_:('m':_))))     = True
isColored ('\27':('[':(_:(_:('m':_))))) = True
isColored (_:ss)                        = isColored ss
isColored ""                            = False

--------------------------------------------------------------------------------
--  String coloring
colorString :: HasANSICode a => a -> String -> String
colorString c s = show c ++ s ++ show ANSINone

blackString, redString, greenString, yellowString, blueString, magentaString, cyanString, whiteString :: String -> String
blackString   = colorString ANSIBlack
redString     = colorString ANSIRed
greenString   = colorString ANSIGreen
yellowString  = colorString ANSIYellow
blueString    = colorString ANSIBlue
magentaString = colorString ANSIMagenta
cyanString    = colorString ANSICyan
whiteString   = colorString ANSIWhite
blackBkString, redBkString, greenBkString, yellowBkString, blueBkString, magentaBkString, cyanBkString, whiteBkString :: String -> String
blackBkString   = colorString ANSIBkBlack
redBkString     = colorString ANSIBkRed
greenBkString   = colorString ANSIBkGreen
yellowBkString  = colorString ANSIBkYellow
blueBkString    = colorString ANSIBkBlue
magentaBkString = colorString ANSIBkMagenta
cyanBkString    = colorString ANSIBkCyan
whiteBkString   = colorString ANSIBkWhite
boldString, cursiveString, backgroundString, underlineString, blinkString, cancelString :: String -> String
boldString       = colorString ANSIBold
cursiveString    = colorString ANSICursive
backgroundString = colorString ANSIBackground
underlineString  = colorString ANSIUnderline
blinkString      = colorString ANSIBlink
cancelString     = colorString ANSICancel

--------------------------------------------------------------------------------
--  Show colored things
mkColored :: (HasANSICode a, Typeable b, Show b) => a -> b -> String
mkColored a x = colorString a $ if typeOf x == typeOf ""
                                then (fromJust $ cast x :: String)
                                else show x

mkBlack, mkRed, mkGreen, mkYellow, mkBlue, mkMagenta, mkCyan, mkWhite :: (Typeable b, Show b) => b -> String
mkBlack   = mkColored ANSIBlack
mkRed     = mkColored ANSIRed
mkGreen   = mkColored ANSIGreen
mkYellow  = mkColored ANSIYellow
mkBlue    = mkColored ANSIBlue
mkMagenta = mkColored ANSIMagenta
mkCyan    = mkColored ANSICyan
mkWhite   = mkColored ANSIWhite
mkBkBlack, mkBkRed, mkBkGreen, mkBkYellow, mkBkBlue, mkBkMagenta, mkBkCyan, mkBkWhite :: (Typeable b, Show b) => b -> String
mkBkBlack   = mkColored ANSIBkBlack
mkBkRed     = mkColored ANSIBkRed
mkBkGreen   = mkColored ANSIBkGreen
mkBkYellow  = mkColored ANSIBkYellow
mkBkBlue    = mkColored ANSIBkBlue
mkBkMagenta = mkColored ANSIBkMagenta
mkBkCyan    = mkColored ANSIBkCyan
mkBkWhite   = mkColored ANSIBkWhite
mkBold, mkCursive, mkBackground, mkUnderline, mkBlink, mkCancel :: (Typeable b, Show b) => b -> String
mkBold       = mkColored ANSIBold
mkCursive    = mkColored ANSICursive
mkBackground = mkColored ANSIBackground
mkUnderline  = mkColored ANSIUnderline
mkBlink      = mkColored ANSIBlink
mkCancel     = mkColored ANSICancel

--------------------------------------------------------------------------------
--  Put colored strings
colorPutStrLn :: (HasANSICode a, Typeable b, Show b) => a -> b -> IO ()
colorPutStrLn c = putStrLn . mkColored c

blackPutStrLn, redPutStrLn, greenPutStrLn, yellowPutStrLn, bluePutStrLn, magentaPutStrLn, cyanPutStrLn, whitePutStrLn :: (Typeable b, Show b) => b -> IO ()
blackPutStrLn   = colorPutStrLn ANSIBlack
redPutStrLn     = colorPutStrLn ANSIRed
greenPutStrLn   = colorPutStrLn ANSIGreen
yellowPutStrLn  = colorPutStrLn ANSIYellow
bluePutStrLn    = colorPutStrLn ANSIBlue
magentaPutStrLn = colorPutStrLn ANSIMagenta
cyanPutStrLn    = colorPutStrLn ANSICyan
whitePutStrLn   = colorPutStrLn ANSIWhite

colorPutStr :: (HasANSICode a, Typeable b, Show b) => a -> b -> IO ()
colorPutStr c = putStr . mkColored c

blackPutStr, redPutStr, greenPutStr, yellowPutStr, bluePutStr, magentaPutStr, cyanPutStr, whitePutStr :: (Typeable b, Show b) => b -> IO ()
blackPutStr   = colorPutStr ANSIBlack
redPutStr     = colorPutStr ANSIRed
greenPutStr   = colorPutStr ANSIGreen
yellowPutStr  = colorPutStr ANSIYellow
bluePutStr    = colorPutStr ANSIBlue
magentaPutStr = colorPutStr ANSIMagenta
cyanPutStr    = colorPutStr ANSICyan
whitePutStr   = colorPutStr ANSIWhite

--------------------------------------------------------------------------------
--  Trace colored strings
colorTrace :: (HasANSICode a, Typeable b, Show b) => a -> b -> b' -> b'
colorTrace a x = trace (mkColored a x)

blackTrace, redTrace, greenTrace, yellowTrace, blueTrace, magentaTrace, cyanTrace, whiteTrace :: (Typeable a, Show a) => a -> a' -> a'
blackTrace   = colorTrace ANSIBlack
redTrace     = colorTrace ANSIRed
greenTrace   = colorTrace ANSIGreen
yellowTrace  = colorTrace ANSIYellow
blueTrace    = colorTrace ANSIBlue
magentaTrace = colorTrace ANSIMagenta
cyanTrace    = colorTrace ANSICyan
whiteTrace   = colorTrace ANSIWhite
boldTrace, cursiveTrace, backgroundTrace, underlineTrace, blinkTrace, cancelTrace :: (Typeable a, Show a) => a -> a' -> a'
boldTrace       = colorTrace ANSIBold
cursiveTrace    = colorTrace ANSICursive
backgroundTrace = colorTrace ANSIBackground
underlineTrace  = colorTrace ANSIUnderline
blinkTrace      = colorTrace ANSIBlink
cancelTrace     = colorTrace ANSICancel

--------------------------------------------------------------------------------
--  IdTrace colored strings
colorIdTrace :: (HasANSICode a, Typeable b, Show b) => a -> b -> b
colorIdTrace a x = colorTrace a x x

blackIdTrace, redIdTrace, greenIdTrace, yellowIdTrace, blueIdTrace, magentaIdTrace, cyanIdTrace, whiteIdTrace :: (Typeable a, Show a) => a -> a
blackIdTrace   = colorIdTrace ANSIBlack
redIdTrace     = colorIdTrace ANSIRed
greenIdTrace   = colorIdTrace ANSIGreen
yellowIdTrace  = colorIdTrace ANSIYellow
blueIdTrace    = colorIdTrace ANSIBlue
magentaIdTrace = colorIdTrace ANSIMagenta
cyanIdTrace    = colorIdTrace ANSICyan
whiteIdTrace   = colorIdTrace ANSIWhite
boldIdTrace, cursiveIdTrace, backgroundIdTrace, underlineIdTrace, blinkIdTrace, cancelIdTrace :: (Typeable a, Show a) => a -> a
boldIdTrace       = colorIdTrace ANSIBold
cursiveIdTrace    = colorIdTrace ANSICursive
backgroundIdTrace = colorIdTrace ANSIBackground
underlineIdTrace  = colorIdTrace ANSIUnderline
blinkIdTrace      = colorIdTrace ANSIBlink
cancelIdTrace     = colorIdTrace ANSICancel
