module Main where
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import RungeKutta (rungeKutta, rungeKuttaFelhberg, RungeKuttaFelhbergResult)

prompt :: String -> IO String
prompt text = do
    putStr text
    >> hFlush stdout
    >> getLine

main :: IO()
main = do
    let 
        f t y = t * y
        descricaoF = "y' = t.y(t)"
        -- printResults r = putStrLn ("\nt: " ++ show () ++ "\ny: " ++ show (y r) )
    putStrLn $ "Runge-Kutta-Felhberg para " ++ descricaoF
    t0 <- prompt "t0: "
    y0 <- prompt "y0: "
    h <- prompt "h: "
    epsilon <- prompt "ε: "
    n_iter <- prompt "Numero de passos dados: "
    case rungeKuttaFelhberg f (read t0 :: Float) (read y0) (read h) (read epsilon) (read n_iter :: Int) of
        Just results -> mapM_ print results
        Nothing -> putStrLn "h não pode ser 0"
