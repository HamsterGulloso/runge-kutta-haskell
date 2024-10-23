module RungeKutta (
    rungeKutta
) where

rungeKutta :: (Fractional n) => (Ord n) => (n -> n -> n) -> n -> n -> n -> n -> Maybe [(n, n)] 

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
