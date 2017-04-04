-- file: ch07/toupper-imp.hs
import System.IO
import Control.Monad
import Data.Char(toUpper)

main :: IO ()
main  =  do 
       	inh <- openFile "Home.txt" ReadMode
       	outh <- openFile "HomeTokens.txt" WriteMode
       	mainloop inh outh 
       	hClose inh
       	hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh;
       if ineof
           then return ()
           else do 
           			  {
                    inpStr <- hGetLine inh;
             		    if take 2 inpStr == "Fe" || take 2 inpStr == "Br" || take 2 inpStr == "IT" || take 2 inpStr == "Ti" || take 2 inpStr == "Co"
                      then hPutStrLn outh inpStr;
                      else hPutStr outh "";
                    mainloop inh outh
                }

                


