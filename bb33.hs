
-- a la https://www.sligocki.com//2023/10/16/bb-3-3-is-hard.html
-- this is Busy Beaver (3,3) candidate `1RB2RA1LC_2LC1RB2RB_---2LA1LA`
-- as in https://bbchallenge.org/1RB2RA1LC_2LC1RB2RB_---2LA1LA


-- we can prove the following rules
-- A(a, 6k+0, c) -> A(a + 0, 8k + c - 1, 2) if 8k + c >= 1 [for A(2,1,2), c is 2 or 3 or 5]
-- A(a, 6k+1, c) -> A(a + 1, 8k + c - 1, 3) if 8k + c >= 1 [for A(2,1,2), c is 2 or 3 or 5]
-- A(a, 6k+2, c) -> A(a - 1, 8k + c + 3, 2) if a >= 1
-- A(a, 6k+3, c) -> A(a + 0, 8k + c + 1, 5)
-- A(a, 6k+4, c) -> A(a + 1, 8k + c + 3, 2)
-- A(a, 6k+5, c) -> A(a + 0, 8k + c + 5, 3)
-- A(0, 6k+2, c) -> HALT(16k + 2c + 7)

maxTick :: Int
maxTick = 1

aFn :: Integer -> Integer -> Integer -> Int -> [[Integer]]
aFn a b c tick = let (k, krem) = divMod b 6 in
    if tick >= maxTick then ([a,b,c] : (aFn a b c 0)) else (aFnK a k krem c tick)

aFnK :: Integer -> Integer -> Integer -> Integer -> Int -> [[Integer]]
aFnK a k 0 c tick = aFn (a + 0) (8 * k + c - 1) 2 (tick+1)
aFnK a k 1 c tick = aFn (a + 1) (8 * k + c - 1) 3 (tick+1)
aFnK 0 k 2 c tick = [[16 * k + 2 * c + 7]]
aFnK a k 2 c tick = aFn (a - 1) (8 * k + c + 3) 2 (tick+1)
aFnK a k 3 c tick = aFn (a + 0) (8 * k + c + 1) 5 (tick+1)
aFnK a k 4 c tick = aFn (a + 1) (8 * k + c + 3) 2 (tick+1)
aFnK a k 5 c tick = aFn (a + 0) (8 * k + c + 5) 3 (tick+1)

main = mapM_ (\ ((a:_), tick) -> print [a, tick]) (zip (aFn 2 1 2 0) [1..])
