import Control.Applicative
import Control.Monad
import Control.Exception
import Data.List
import Data.Function
import System.Environment
import System.IO
import System.Process

brightnessPath = "/sys/class/leds/asus::kbd_backlight/brightness"
maxBrightnessPath = "/sys/class/leds/asus::kbd_backlight/max_brightness"

stripRightParen ")" = Just ""
stripRightParen (x:xs) = (x :) <$> stripRightParen xs
stripRightParen _ = Nothing

stripParens ('(' : xs) = stripRightParen xs
stripParens _ = Nothing

msg str = do
  putStrLn str
  userLines <- lines <$> readProcess "who" [] ""
  forM_ (nubBy ((==) `on` head . words) userLines) $ \userLine ->
    -- Example line:
    -- "eyal     tty8         2013-07-07 01:14 (:0)"
    case words userLine of
    [user, tty, date, time, parenDisp] ->
      case stripParens parenDisp of
      Just disp -> system $ "DISPLAY=" ++ disp ++ " sudo -u " ++ user ++ " notify-send " ++ show str
      _ -> fail $ "Invalid display in who output: " ++ show parenDisp ++ " (" ++ userLine ++ ")"
    _ -> fail $ "Invalid line in who output (" ++ userLine ++ ")"

parse :: Read a => FilePath -> IO a
parse filePath = do
  str <- unwords . words <$> readFile filePath
  case reads str of
    [(x, "")] -> return x
    _ -> fail $ "Invalid content of " ++ filePath ++ ": " ++ show str

logExceptions :: String -> IO a -> IO a
logExceptions text act = act `catch` \(SomeException e) -> msg (text ++ show e) >> throwIO e

setBrightness :: Int -> IO ()
setBrightness val = logExceptions "setBrightness failed: " $ do
  writeFile brightnessPath $ show val
  newBrightness <- parse brightnessPath
  if newBrightness /= val
    then msg $ "Brightness attempted to be set to " ++ show val ++ " but remained " ++ show newBrightness
    else msg $ "Brightness set to " ++ show newBrightness

main = do
  brightness <- parse brightnessPath
  maxBrightness <- parse maxBrightnessPath
  putStrLn $ show maxBrightness
  args <- getArgs
  case args of
    [] -> msg $ "Current brightness is: " ++ show brightness
    ["down"]
      | brightness > 0 -> setBrightness (brightness-1)
      | otherwise -> msg "Brightness level is already minimum"
    ["up"]
      | brightness < maxBrightness -> setBrightness $ brightness+1
      | otherwise -> msg "Brightness level is already maximume"
    _ -> do
      progName <- getProgName
      hPutStrLn stderr $ "Usage: " ++ progName ++ " [down|up]"
