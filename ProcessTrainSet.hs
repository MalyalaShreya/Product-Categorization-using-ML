-- file: ch07/toupper-imp.hs
import System.IO
import Control.Monad
import Data.Char(toUpper)
import Data.List
main :: IO ()
main  =  do 
        inh <- openFile "Test5.txt" ReadMode
        outh <- openFile "TestFile5.txt" WriteMode
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
                    if take 2 inpStr == "Br" || take 2 inpStr == "IT"  
                      then hPutStrLn outh inpStr;
                      else 
                          if take 2 inpStr == "Fe" then hPutStr outh (inpStr++" ")
                            else
                              if take 2 inpStr == "Ti" then hPutStrLn outh ("\n"++inpStr)
                                else hPutStr outh "";
                    mainloop inh outh
                }
