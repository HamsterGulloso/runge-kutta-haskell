module RungeKutta (
    rungeKutta
) where

rungeKutta ::
    (Fractional n)
    => (Ord n)
    => (n -> n -> n)
    -> n
    -> n
    -> n
    -> n
    -> Maybe [(n, n)] 

rungeKutta _ _ _ _ 0 = Nothing

rungeKutta f t0 y0 tMax h =
    let t1 = t0 + h
        k1 = f t0 y0
        k2 = f (t0 + (h / 2)) (y0 + (h / 2) * k1)
        k3 = f (t0 + (h / 2)) (y0 + (h / 2) * k2)
        k4 = f t1 (y0 + h * k3)
        y1 = y0 + (h / 6) * (k1 + 2*k2 + 2*k3 + k4)
    in
    if
        (h > 0 && tMax < t0)
        || (h < 0 && tMax > t0)
    then
        Just []
    else
        rungeKutta f t1 y1 tMax h
        >>= \rk -> Just ((t0, y0) : rk)

rungeKuttaFelhberg ::
    (Fractional n)
    => (Ord n)
    => (Integral i)
    => (n -> n -> n)
    -> n
    -> n
    -> n
    -> n
    -> i
    -> Maybe [(n, n)] 
rungeKuttaFelhberg _ _ _ 0 _ _ = Nothing
rungeKuttaFelhberg f t0 w0 h epsilon n_iter =
    let
        k1 = h * f t0 w0
        k2 = h * f (t0 + h / 4) (w0 + k1 / 4)
        k3 = h * f (t0 + 3 * h / 8) (w0 + 3 * k1 / 32 + 9 * k2 / 32)
        k4 = h * f (t0 + 12 * h / 13) (w0 + 1932 * k1 / 2197 - 7200 * k2 / 2197 + 7296 * k3 / 2197)
        k5 = h * f (t0 + h) (w0 + 439 * k1 / 216 - 8 * k2 + 3680 * k3 / 513 - 845 * k4 / 4104)
        k6 = h * f (t0 + h / 2) (w0 - 8 * k1 / 27 + 2 * k2 - 3544 * k3 / 2565 + 1859 * k4 / 4104 - 11 * k5 / 40)
        w1 = w0 + 25 * k1 / 216 + 1408 * k3 / 2565 + 2197 * k4 / 4104 - k5 / 5
        w'1 = w0 + 16 * k1 / 135 + 6656 * k3 / 12825 + 28561 * k4 / 56430 - 9 * k5 / 50 + 2 * k6 / 55
    in
        Nothing
        


    
