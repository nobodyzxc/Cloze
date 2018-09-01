import Data.List
import System.IO
import System.Environment

scoop (w:ord) = w:replicate l '.' ++ [d]
  where l = length ord - 1
        d = last ord

check tests = all (== True) $ map (any unistar . words) tests


unqualified [] = []
unqualified (t:est) =
  if any unistar $ words t
     then unqualified est
     else "\n" ++ t ++ unqualified est

takeans word = take idx word
  where Just idx = elemIndex '*' word

unistar = (==) 1 . length . filter (== '*')

handle example = (unwords ps, as)
  where (ps, as) = sub list
        list = words example
        sub [] = ([], [])
        sub (x:xs) = if unistar x
                        then (scoop ans:px, ans:ax)
                        else (x:px, ax)
                          where (px, ax) = sub xs
                                ans = takeans x

test [] [] = putStr "test finished!"
test fails [] = test [] $ reverse fails
test fails (example:rest) = do
  putStr "\ESC[2J\ESC[0;0H"
  putStrLn $ show (length rest + 1) ++ " words left, "
              ++ show (length fails) ++ " words failed.\n"
  putStrLn $ problem ++ "\n"
  putStrLn . unwords $ map scoop answers
  input <- getLine
  if words input == answers
     then test fails rest
     else do
       putStr $ replicate (pred . length $ unwords answers) '-'
                ++ "> " ++ unwords answers
       _ <- getLine
       test (example:fails) rest
         where (problem, answers) = handle example

main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  raws <- fmap lines . readFile $
    if null args then error "testfile?" else head args
  let tests = filter (not . null) raws
    in if check tests
          then test [] tests
          else error $ unqualified tests
                      ++ "\n\n**exist non-stared sentence(s)**\n"
