module Main where
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import RungeKutta (rungeKutta)

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
        printResults (t, y) = putStrLn ("\nt: " ++ show t ++ "\ny: " ++ show y )
    putStrLn $ "Runge-Kutta para " ++ descricaoF
    t0 <- prompt "t0: "
    y0 <- prompt "y0: "
    tMax <- prompt "Até que valor iterar(tMax): "
    h <- prompt "Tamanho do intervalo(h): "
    case rungeKutta f (read t0) (read y0) (read tMax) (read h) of
        Just results -> mapM_ printResults results
        Nothing -> putStrLn "h não pode ser 0"
