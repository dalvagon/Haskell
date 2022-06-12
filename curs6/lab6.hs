-- Leahu Vlad A6

import Nat

-- EX 1
minList :: Ord a => [a] -> Either String a
minList [] = Left "Empty"
minList l = Right  (head (quickSort l))
{-
*Main> minList ['x', 't', 'y', 'c']
Right 'c'
*Main> minList [4.6, 6, 5.9, 1.5, 1.55]
Right 1.5
*Main> minList [1 * 2.5* 7.5, 4 + 6 * 5.1, 5 ** 2 + 3]
Right 18.75
-}

-- EX 2
-------------------------
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort (filter (<= x) xs) ++ [x] ++ quickSort (filter (> x) xs)

quickSortT :: Ord a => [a] -> Int
quickSortT [] = 0
quickSortT (x : xs) = length (filter (<= x) xs) + quickSortT (filter (<= x) xs) + length (filter (> x) xs) + quickSortT (filter (> x) xs)
{-
*Main> quickSortT [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] 
45
*Main> quickSortT [1..10]
45
*Main> quickSortT [1..100]
4950
*Main> quickSortT (reverse [1..100])
4950
*Main> quickSortT [6, 7, 8, 9, 10, 5, 4, 3, 2, 1]
25
*Main> quickSortT [9, 7, 8, 9, 10, 5, 4, 3, 2, 1] 
27
*Main> quickSortT [4, 7, 8, 9, 10, 5, 9, 3, 2, 1]
22
*Main> quickSortT [5, 6, 1, 5, 1, 5, 2, 5 ,43, 6, 2, 35, 3, 5, 5 , 1]
54
Complexitatea QuickSort depinde de ce element se alege ca pivot:
- in cazul in care se alege primul element dint-o lista ordonata descrescator, se fac comparatii cu fiecare dintre celealte elemente 
- in cazul in care se alege primul element dint-o lista ordonata crescator, se intampla acelasi lucru
In aceste doua cazuri complexitatea este (n)*(n + 1)/2 = O(n)
- daca se alege drept pivot un alt element, complexitatea nu atinge O(n logn)
-}
-------------------------
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert y (x : xs)
    | x < y = x : insert y xs
    | otherwise = y : x : xs

insertSort :: Ord a => [a] -> [a]
insertSort = foldr insert []

insertT :: Ord a => a -> [a] -> Int
insertT x [] =  0
insertT y (x : xs)
    | x < y = 1 + insertT y xs
    | otherwise = 1

insertSortT :: Ord a => [a] -> Int
insertSortT [] = 0
insertSortT (x : xs) = insertT x xs + insertSortT xs
{-
*Main> insertSortT [1..10]
9
*Main> insertSortT [1..1000]
999
*Main> insertSortT (reverse [1..10])  
45
*Main> insertSortT (reverse [1..1000])
499500
Se observa ca in cazul cel mai favorabil, InsertSort are complexitatea O(n).
In cazul cel mai nefavorabil, se parcurg
(n - 1) pozitii pentru a se insera primul element la locul sau, (n - 2) pentru
al doilea => (n - 1) + (n - 2) + ... + 1 =>complexitatea este O(n ^ 2)
-}
-------------------------
removeL :: Ord a => a -> [a] -> [a]
removeL _ [] = []
removeL x (y : ys)
    | x == y = ys
    | otherwise = y : removeL x ys

minL :: Ord a => [a] -> a
minL [x] = x
minL [x, y] = min x y
minL (x : y : ys)
    | x < y = minL (x : ys)
    | otherwise = minL (y : ys)

selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort l = minL l : selectSort (removeL (minL l) l)


removeLT :: Ord a => a -> [a] -> Int
removeLT _ [] = 0
removeLT x (y : ys)
    | x == y = 1
    | otherwise = 1 + removeLT x ys

selectSortT :: Ord a => [a] -> Int
selectSortT [] = 0
selectSortT l = (length l - 1) + selectSortT (removeL (minL l) l) + removeLT (minL l) l
{-
*Main> selectSortT [1..100]
5050
*Main> selectSortT (reverse [1..100])
10000
*Main> selectSortT [1..1000]
500500
*Main> selectSortT (reverse [1..1000])
1000000
    In ambele cazuri, aflarea minimului are complexitatea O(n).
    In cazul cel mai favorabil, cand lista este ordonata crescator, eliminarea minimului se face in O(1).
Se fac n eliminari si n aflari ale minimului => n + n ^ 2 => complexitatea O(n ^ 2)
    In cazul cel mai nefavorabil , eliminarea minimului se face in O(n).
Se fac n eliminari si n aflari ale minimului => n ^ 2 + n ^ 2 => complexitatea O(n ^ 2)
-}
-------------------------
firstHalf' :: [a] -> Int -> [a]
firstHalf' [] _ = []
firstHalf' (x : xs) 0 = [x]
firstHalf' (x : xs) len = x : firstHalf' xs (len - 1)

firstHalf :: [a] -> [a]
firstHalf [x] = []
firstHalf l = firstHalf' l (length l `div` 2 - 1)


secondHalf' :: [a] -> Int -> [a]
secondHalf' [] _ = []
secondHalf' l len
        | len == length l - 1 = [(!!) l len]
        | otherwise = (!!) l len : secondHalf' l (len + 1)

secondHalf :: [a] -> [a]
secondHalf l = secondHalf' l (length l `div` 2)

merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort (firstHalf l)) (mergeSort (secondHalf l))


mergeSortT :: Ord a => [a] -> Int
mergeSortT [] = 0
mergeSortT [x] = 1
mergeSortT l = mergeSortT (firstHalf l) + mergeSortT (secondHalf l) + (length l - 1)
{-
*Main> mergeSortT [1..1000]
9977
*Main> mergeSortT (reverse [1..1000])
9977
In ambele cazuri, concatenarea se face in O(n) si fiecare lista este sortata prin partitionare - 
se fac cel mult log n partitionari => complexitatea este O(n log n).
(De exmplu )
-}
-------------------------
{-
*Main> quickSort ['x', 't', 'y', 'c'] 
"ctxy"
*Main> insertSort ['x', 't', 'y', 'c']
"ctxy"
*Main> selectSort ['x', 't', 'y', 'c']
"ctxy"
*Main> mergeSort  ['x', 't', 'y', 'c']
"ctxy"
-}

-- EX 4
maxList :: Ord a => [a] -> Either [Char] a
maxList [] = Left "Empty"
maxList l = Right ((!!) (quickSort l) (length l - 1))
{-
*Main> maxL [1..100]
Right 100
*Main> maxL (reverse [1..100])
Right 100
-}

-- EX 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

fib' :: [Int] -> [Int]
fib' = map fib
l :: [Int]
l = [0..]
fibInf :: [Int]
fibInf = fib' l
{-
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,
-}

-- EX 6
isPrime' :: Int -> Int ->  Bool
isPrime' 0 _ = False
isPrime' 1 _ = False
isPrime' x 1 = True
isPrime' x d = (x `mod` d) /= 0 && isPrime' x (d - 1)

isPrime :: Int -> Bool
isPrime x = isPrime' x (x `div` 2)
infPrimeBool :: [Bool]
infPrimeBool = map isPrime l
-- EX 7
infPrime :: [Int]
infPrime = filter isPrime l
{-
*Main> infPrime
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,
-}



-- EX 8
data List a = Nil | Cons a (List a) deriving Show
-----------------------------------------------
toList :: [a] -> List a
toList = foldr Cons Nil

filterL :: (a -> Bool) -> List a -> List a
filterL _ Nil = Nil
filterL f (Cons x l)
    | f x = Cons x (filterL f l)
    | otherwise = filterL f l

concatL :: List a -> List a -> List a
concatL Nil Nil = Nil
concatL l Nil = l
concatL Nil l = l
concatL (Cons hd tl) l = Cons hd (concatL tl l)

quickSortL :: Ord a => List a -> List a
quickSortL Nil = Nil
quickSortL (Cons x l) = concatL (concatL (quickSortL (filterL (<= x) l)) (Cons x Nil)) (quickSortL (filterL (> x) l))
{-
*Main> l = (reverse [1..100])
*Main> quickSortL (toList l) 
Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 (Cons 21 (Cons 22 (Cons 23 (Cons 24 (Cons 25 (Cons 26 (Cons 27 (Cons 28 (Cons 29 (Cons 30 (Cons 31 (Cons 32 (Cons 33 (Cons 34 (Cons 35 (Cons 36 (Cons 37 (Cons 38 (Cons 39 (Cons 40 (Cons 41 (Cons 42 (Cons 43 (Cons 44 (Cons 45 (Cons 46 (Cons 47 (Cons 48 (Cons 49 (Cons 50 (Cons 51 (Cons 52 (Cons 53 (Cons 54 (Cons 55 (Cons 56 (Cons 57 (Cons 58 (Cons 59 (Cons 60 (Cons 61 (Cons 62 (Cons 63 (Cons 64 (Cons 65 (Cons 66 (Cons 67 (Cons 68 (Cons 69 (Cons 70 (Cons 71 (Cons 72 (Cons 73 (Cons 74 (Cons 75 (Cons 76 (Cons 77 (Cons 78 (Cons 79 (Cons 80 (Cons 81 (Cons 82 (Cons 83 (Cons 84 (Cons 85 (Cons 86 (Cons 87 (Cons 88 (Cons 89 (Cons 90 (Cons 91 (Cons 92 (Cons 93 (Cons 94 (Cons 95 (Cons 96 (Cons 97 (Cons 98 (Cons 99 (Cons 100 Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}
-----------------------------------------------
instance Semigroup (List a) where
    (<>) = concatL

instance Monoid (List a) where
    mempty = Nil

instance Foldable List where
    foldMap f Nil = mempty
    foldMap f (Cons x l) = (<>) (f x) (foldMap f l)
{-
*Main> foldl (+) 0 (toList [1..10])
55
*Main> maximum (Cons 1(Cons 4 (Cons 2 (Cons 3 Nil))))
4
-}
--------------------------------------

insertL :: Ord a => a -> List a -> List a
insertL x Nil = Cons x Nil
insertL y (Cons x l)
    | x < y = Cons x (insertL y l)
    | otherwise = Cons y (Cons x l)

insertSortL :: Ord a => List a -> List a
insertSortL = foldr insertL Nil
{-
*Main> insertSortL (toList (reverse [1..100]))
Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 (Cons 21 (Cons 22 (Cons 23 (Cons 24 (Cons 25 (Cons 26 (Cons 27 (Cons 28 (Cons 29 (Cons 30 (Cons 31 (Cons 32 (Cons 33 (Cons 34 (Cons 35 (Cons 36 (Cons 37 (Cons 38 (Cons 39 (Cons 40 (Cons 41 (Cons 42 (Cons 43 (Cons 44 (Cons 45 (Cons 46 (Cons 47 (Cons 48 (Cons 49 (Cons 50 (Cons 51 (Cons 52 (Cons 53 (Cons 54 (Cons 55 (Cons 56 (Cons 57 (Cons 58 (Cons 59 (Cons 60 (Cons 61 (Cons 62 (Cons 63 (Cons 64 (Cons 65 (Cons 66 (Cons 67 (Cons 68 (Cons 69 (Cons 70 (Cons 71 (Cons 72 (Cons 73 (Cons 74 (Cons 75 (Cons 76 (Cons 77 (Cons 78 (Cons 79 (Cons 80 (Cons 81 (Cons 82 (Cons 83 (Cons 84 (Cons 85 (Cons 86 (Cons 87 (Cons 88 (Cons 89 (Cons 90 (Cons 91 (Cons 92 (Cons 93 (Cons 94 (Cons 95 (Cons 96 (Cons 97 (Cons 98 (Cons 99 (Cons 100 Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}

-----------------------------------------------

removeLL :: Ord a => a -> List a -> List a
removeLL _ Nil = Nil
removeLL x (Cons y ys)
    | x == y = ys
    | otherwise = Cons y (removeLL x ys)

minLL :: Ord a => List a -> a
minLL (Cons x Nil) = x
minLL (Cons x (Cons y Nil)) = min x y
minLL (Cons x (Cons y l))
    | x < y = minLL (Cons x l)
    | otherwise = minLL (Cons y l)

selectSortL :: Ord a => List a -> List a
selectSortL Nil = Nil
selectSortL l = Cons (minLL l) (selectSortL (removeLL (minLL l) l))
{-
*Main> selectSortL (toList (reverse [1..1000]))
Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 (Cons 21 (Cons 22 (Cons 23 (Cons 24 (Cons 25 (Cons 26 (Cons 27 (Cons 28 (Cons 29 (Cons 30 (Cons 31 (Cons 32 (Cons 33 (Cons 34 (Cons 35 (Cons 36 (Cons 37 (Cons 38 (Cons 39 (Cons 40 (Cons 41 (Cons 42 (Cons 43 (Cons 44 (Cons 45 (Cons 46 (Cons 47 (Cons 48 (Cons 49 (Cons 50 (Cons 51 (Cons 52 (Cons 53 (Cons 54 (Cons 55 (Cons 56 (Cons 57 (Cons 58 (Cons 59 (Cons 60 (Cons 61 (Cons 62 (Cons 63 (Cons 64 (Cons 65 (Cons 66 (Cons 67 (Cons 68 (Cons 69 (Cons 70 (Cons 71 (Cons 72 (Cons 73 (Cons 74 (Cons 75 (Cons 76 (Cons 77 (Cons 78 (Cons 79 (Cons 80 (Cons 81 (Cons 82 (Cons 83 (Cons 84 (Cons 85 (Cons 86 (Cons 87 (Cons 88 (Cons 89 (Cons 90 (Cons 91 (Cons 92 (Cons 93 (Cons 94 (Cons 95 (Cons 96 (Cons 97 (Cons 98 (Cons 99 (Cons 100 (Cons 101 (Cons 102 (Cons 103 (Cons 104 (Cons 105 (Cons 106 (Cons 107 (Cons 108 (Cons 109 (Cons 110 (Cons 111 (Cons 112 (Cons 113 (Cons 114 (Cons 115 (Cons 116 (Cons 117 (Cons 118 (Cons 119 (Cons 120 (Cons 121 (Cons 122 (Cons 123 (Cons 124 (Cons 125 (Cons 126 (Cons 127 (Cons 128 (Cons 129 (Cons 130 (Cons 131 (Cons 132 (Cons 133 (Cons 134 (Cons 135 (Cons 136 (Cons 137 (Cons 138 (Cons 139 (Cons 140 (Cons 141 (Cons 142 (Cons 143 (Cons 144 (Cons 145 (Cons 146 (Cons 147 (Cons 148 (Cons 149 (Cons 150 (Cons 151 (Cons 
152 (Cons 153 (Cons 154 (Cons 155 (Cons 156 (Cons 157 (Cons 158 (Cons 159 (Cons 160 (Cons 161 (Cons 162 (Cons 163 (Cons 164 (Cons 165 (Cons 166 (Cons 167 (Cons 168 (Cons 169 (Cons 170 (Cons 171 (Cons 172 (Cons 173 (Cons 174 (Cons 175 (Cons 176 (Cons 177 (Cons 178 (Cons 179 (Cons 180 (Cons 181 (Cons 182 (Cons 183 (Cons 184 (Cons 185 (Cons 186 (Cons 187 (Cons 188 (Cons 189 (Cons 190 (Cons 191 (Cons 192 (Cons 193 (Cons 194 (Cons 195 (Cons 196 (Cons 197 (Cons 198 (Cons 199 (Cons 200 (Cons 201 (Cons 202 (Cons 203 (Cons 204 (Cons 205 (Cons 206 (Cons 207 (Cons 208 (Cons 209 (Cons 210 (Cons 211 (Cons 212 (Cons 213 (Cons 214 (Cons 215 (Cons 216 (Cons 217 (Cons 218 (Cons 219 (Cons 220 (Cons 221 (Cons 222 (Cons 223 (Cons 224 (Cons 225 (Cons 226 (Cons 227 (Cons 228 (Cons 229 (Cons 230 (Cons 231 (Cons 232 
(Cons 233 (Cons 234 (Cons 235 (Cons 236 (Cons 237 (Cons 238 (Cons 239 (Cons 240 (Cons 241 (Cons 242 (Cons 243 (Cons 244 (Cons 245 (Cons 246 (Cons 247 (Cons 248 (Cons 249 (Cons 250 (Cons 251 (Cons 252 (Cons 253 (Cons 254 (Cons 255 (Cons 256 (Cons 257 (Cons 258 (Cons 259 (Cons 260 (Cons 261 (Cons 262 (Cons 263 (Cons 264 (Cons 265 (Cons 266 (Cons 267 (Cons 268 (Cons 269 (Cons 270 (Cons 271 (Cons 272 (Cons 273 (Cons 274 (Cons 275 (Cons 276 (Cons 277 (Cons 278 (Cons 279 (Cons 280 (Cons 281 (Cons 282 (Cons 283 (Cons 284 (Cons 285 (Cons 286 (Cons 287 (Cons 288 (Cons 289 (Cons 290 (Cons 291 (Cons 292 (Cons 293 (Cons 294 (Cons 295 (Cons 296 (Cons 297 (Cons 298 (Cons 299 (Cons 300 (Cons 301 (Cons 302 (Cons 303 (Cons 304 (Cons 305 (Cons 306 (Cons 307 (Cons 308 (Cons 309 (Cons 310 (Cons 311 (Cons 312 (Cons 313 (Cons 314 (Cons 315 (Cons 316 (Cons 317 (Cons 318 (Cons 319 (Cons 320 (Cons 321 (Cons 322 (Cons 323 (Cons 324 (Cons 325 (Cons 326 (Cons 327 (Cons 328 (Cons 329 (Cons 330 (Cons 331 (Cons 332 (Cons 333 (Cons 334 (Cons 335 (Cons 336 (Cons 337 (Cons 338 (Cons 339 (Cons 340 (Cons 341 (Cons 342 (Cons 343 (Cons 344 (Cons 345 (Cons 346 (Cons 347 (Cons 348 (Cons 349 (Cons 350 (Cons 351 (Cons 352 (Cons 
353 (Cons 354 (Cons 355 (Cons 356 (Cons 357 (Cons 358 (Cons 359 (Cons 360 (Cons 361 (Cons 362 (Cons 363 (Cons 364 (Cons 365 (Cons 366 (Cons 367 (Cons 368 (Cons 369 (Cons 370 (Cons 371 (Cons 372 (Cons 373 (Cons 374 (Cons 375 (Cons 376 (Cons 377 (Cons 378 (Cons 379 (Cons 380 (Cons 381 (Cons 382 (Cons 383 (Cons 384 (Cons 385 (Cons 386 (Cons 387 (Cons 388 (Cons 389 (Cons 390 (Cons 391 (Cons 392 (Cons 393 (Cons 394 (Cons 395 (Cons 396 (Cons 397 (Cons 398 (Cons 399 (Cons 400 (Cons 401 (Cons 402 (Cons 403 (Cons 404 (Cons 405 (Cons 406 (Cons 407 (Cons 408 (Cons 409 (Cons 410 (Cons 411 (Cons 412 (Cons 413 (Cons 414 (Cons 415 (Cons 416 (Cons 417 (Cons 418 (Cons 419 (Cons 420 (Cons 421 (Cons 422 (Cons 423 (Cons 424 (Cons 425 (Cons 426 (Cons 427 (Cons 428 (Cons 429 (Cons 430 (Cons 431 (Cons 432 (Cons 433 
(Cons 434 (Cons 435 (Cons 436 (Cons 437 (Cons 438 (Cons 439 (Cons 440 (Cons 441 (Cons 442 (Cons 443 (Cons 444 (Cons 445 (Cons 446 (Cons 447 (Cons 448 (Cons 449 (Cons 450 (Cons 451 (Cons 452 (Cons 453 (Cons 454 (Cons 455 (Cons 456 (Cons 457 (Cons 458 (Cons 459 (Cons 460 (Cons 461 (Cons 462 (Cons 463 (Cons 464 (Cons 465 (Cons 466 (Cons 467 (Cons 468 (Cons 469 (Cons 470 (Cons 471 (Cons 472 (Cons 473 (Cons 474 (Cons 475 (Cons 476 (Cons 477 (Cons 478 (Cons 479 (Cons 480 (Cons 481 (Cons 482 (Cons 483 (Cons 484 (Cons 485 (Cons 486 (Cons 487 (Cons 488 (Cons 489 (Cons 490 (Cons 491 (Cons 492 (Cons 493 (Cons 494 (Cons 495 (Cons 496 (Cons 497 (Cons 498 (Cons 499 (Cons 500 (Cons 501 (Cons 502 (Cons 503 (Cons 504 (Cons 505 (Cons 506 (Cons 507 (Cons 508 (Cons 509 (Cons 510 (Cons 511 (Cons 512 (Cons 513 (Cons 514 (Cons 515 (Cons 516 (Cons 517 (Cons 518 (Cons 519 (Cons 520 (Cons 521 (Cons 522 (Cons 523 (Cons 524 (Cons 525 (Cons 526 (Cons 527 (Cons 528 (Cons 529 (Cons 530 (Cons 531 (Cons 532 (Cons 533 (Cons 534 (Cons 535 (Cons 536 (Cons 537 (Cons 538 (Cons 539 (Cons 540 (Cons 541 (Cons 542 (Cons 543 (Cons 544 (Cons 545 (Cons 546 (Cons 547 (Cons 548 (Cons 549 (Cons 550 (Cons 551 (Cons 552 (Cons 553 (Cons 
554 (Cons 555 (Cons 556 (Cons 557 (Cons 558 (Cons 559 (Cons 560 (Cons 561 (Cons 562 (Cons 563 (Cons 564 (Cons 565 (Cons 566 (Cons 567 (Cons 568 (Cons 569 (Cons 570 (Cons 571 (Cons 572 (Cons 573 (Cons 574 (Cons 575 (Cons 576 (Cons 577 (Cons 578 (Cons 579 (Cons 580 (Cons 581 (Cons 582 (Cons 583 (Cons 584 (Cons 585 (Cons 586 (Cons 587 (Cons 588 (Cons 589 (Cons 590 (Cons 591 (Cons 592 (Cons 593 (Cons 594 (Cons 595 (Cons 596 (Cons 597 (Cons 598 (Cons 599 (Cons 600 (Cons 601 (Cons 602 (Cons 603 (Cons 604 (Cons 605 (Cons 606 (Cons 607 (Cons 608 (Cons 609 (Cons 610 (Cons 611 (Cons 612 (Cons 613 (Cons 614 (Cons 615 (Cons 616 (Cons 617 (Cons 618 (Cons 619 (Cons 620 (Cons 621 (Cons 622 (Cons 623 (Cons 624 (Cons 625 (Cons 626 (Cons 627 (Cons 628 (Cons 629 (Cons 630 (Cons 631 (Cons 632 (Cons 633 (Cons 634 
(Cons 635 (Cons 636 (Cons 637 (Cons 638 (Cons 639 (Cons 640 (Cons 641 (Cons 642 (Cons 643 (Cons 644 (Cons 645 (Cons 646 (Cons 647 (Cons 648 (Cons 649 (Cons 650 (Cons 651 (Cons 652 (Cons 653 (Cons 654 (Cons 655 (Cons 656 (Cons 657 (Cons 658 (Cons 659 (Cons 660 (Cons 661 (Cons 662 (Cons 663 (Cons 664 (Cons 665 (Cons 666 (Cons 667 (Cons 668 (Cons 669 (Cons 670 (Cons 671 (Cons 672 (Cons 673 (Cons 674 (Cons 675 (Cons 676 (Cons 677 (Cons 678 (Cons 679 (Cons 680 (Cons 681 (Cons 682 (Cons 683 (Cons 684 (Cons 685 (Cons 686 (Cons 687 (Cons 688 (Cons 689 (Cons 690 (Cons 691 (Cons 692 (Cons 693 (Cons 694 (Cons 695 (Cons 696 (Cons 697 (Cons 698 (Cons 699 (Cons 700 (Cons 701 (Cons 702 (Cons 703 (Cons 704 (Cons 705 (Cons 706 (Cons 707 (Cons 708 (Cons 709 (Cons 710 (Cons 711 (Cons 712 (Cons 713 (Cons 714 (Cons 715 (Cons 716 (Cons 717 (Cons 718 (Cons 719 (Cons 720 (Cons 721 (Cons 722 (Cons 723 (Cons 724 (Cons 725 (Cons 726 (Cons 727 (Cons 728 (Cons 729 (Cons 730 (Cons 731 (Cons 732 (Cons 733 (Cons 734 (Cons 735 (Cons 736 (Cons 737 (Cons 738 (Cons 739 (Cons 740 (Cons 741 (Cons 742 (Cons 743 (Cons 744 (Cons 745 (Cons 746 (Cons 747 (Cons 748 (Cons 749 (Cons 750 (Cons 751 (Cons 752 (Cons 753 (Cons 754 (Cons 
755 (Cons 756 (Cons 757 (Cons 758 (Cons 759 (Cons 760 (Cons 761 (Cons 762 (Cons 763 (Cons 764 (Cons 765 (Cons 766 (Cons 767 (Cons 768 (Cons 769 (Cons 770 (Cons 771 (Cons 772 (Cons 773 (Cons 774 (Cons 775 (Cons 776 (Cons 777 (Cons 778 (Cons 779 (Cons 780 (Cons 781 (Cons 782 (Cons 783 (Cons 784 (Cons 785 (Cons 786 (Cons 787 (Cons 788 (Cons 789 (Cons 790 (Cons 791 (Cons 792 (Cons 793 (Cons 794 (Cons 795 (Cons 796 (Cons 797 (Cons 798 (Cons 799 (Cons 800 (Cons 801 (Cons 802 (Cons 803 (Cons 804 (Cons 805 (Cons 806 (Cons 807 (Cons 808 (Cons 809 (Cons 810 (Cons 811 (Cons 812 (Cons 813 (Cons 814 (Cons 815 (Cons 816 (Cons 817 (Cons 818 (Cons 819 (Cons 820 (Cons 821 (Cons 822 (Cons 823 (Cons 824 (Cons 825 (Cons 826 (Cons 827 (Cons 828 (Cons 829 (Cons 830 (Cons 831 (Cons 832 (Cons 833 (Cons 834 (Cons 835 
(Cons 836 (Cons 837 (Cons 838 (Cons 839 (Cons 840 (Cons 841 (Cons 842 (Cons 843 (Cons 844 (Cons 845 (Cons 846 (Cons 847 (Cons 848 (Cons 849 (Cons 850 (Cons 851 (Cons 852 (Cons 853 (Cons 854 (Cons 855 (Cons 856 (Cons 857 (Cons 858 (Cons 859 (Cons 860 (Cons 861 (Cons 862 (Cons 863 (Cons 864 (Cons 865 (Cons 866 (Cons 867 (Cons 868 (Cons 869 (Cons 870 (Cons 871 (Cons 872 (Cons 873 (Cons 874 (Cons 875 (Cons 876 (Cons 877 (Cons 878 (Cons 879 (Cons 880 (Cons 881 (Cons 882 (Cons 883 (Cons 884 (Cons 885 (Cons 886 (Cons 887 (Cons 888 (Cons 889 (Cons 890 (Cons 891 (Cons 892 (Cons 893 (Cons 894 (Cons 895 (Cons 896 (Cons 897 (Cons 898 (Cons 899 (Cons 900 (Cons 901 (Cons 902 (Cons 903 (Cons 904 (Cons 905 (Cons 906 (Cons 907 (Cons 908 (Cons 909 (Cons 910 (Cons 911 (Cons 912 (Cons 913 (Cons 914 (Cons 915 (Cons 916 (Cons 917 (Cons 918 (Cons 919 (Cons 920 (Cons 921 (Cons 922 (Cons 923 (Cons 924 (Cons 925 (Cons 926 (Cons 927 (Cons 928 (Cons 929 (Cons 930 (Cons 931 (Cons 932 (Cons 933 (Cons 934 (Cons 935 (Cons 936 (Cons 937 (Cons 938 (Cons 939 (Cons 940 (Cons 941 (Cons 942 (Cons 943 (Cons 944 (Cons 945 (Cons 946 (Cons 947 (Cons 948 (Cons 949 (Cons 950 (Cons 951 (Cons 952 (Cons 953 (Cons 954 (Cons 955 (Cons 
956 (Cons 957 (Cons 958 (Cons 959 (Cons 960 (Cons 961 (Cons 962 (Cons 963 (Cons 964 (Cons 965 (Cons 966 (Cons 967 (Cons 968 (Cons 969 (Cons 970 (Cons 971 (Cons 972 (Cons 973 (Cons 974 (Cons 975 (Cons 976 (Cons 977 (Cons 978 (Cons 979 (Cons 980 (Cons 981 (Cons 982 (Cons 983 (Cons 984 (Cons 985 (Cons 986 (Cons 987 (Cons 988 (Cons 989 (Cons 990 (Cons 991 (Cons 992 (Cons 993 (Cons 994 (Cons 995 (Cons 996 (Cons 997 (Cons 998 (Cons 999 (Cons 1000 Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}
-----------------------------------------------

toHList :: List a -> [a]
toHList Nil = []
toHList (Cons x l) = x : toHList l

firstHalfL' :: List a -> Int -> List a
firstHalfL' Nil _ = Nil
firstHalfL' (Cons x xs) 0 = Cons x Nil
firstHalfL' (Cons x xs) len = Cons x  (firstHalfL' xs (len - 1))

firstHalfL :: List a -> List a
firstHalfL (Cons x Nil) = Nil
firstHalfL l = firstHalfL' l (length l `div` 2 - 1)


secondHalfL' :: List a -> Int -> List a
secondHalfL' Nil _ = Nil
secondHalfL' l len
        | len == length l - 1 = Cons ((!!) (toHList l) len) Nil
        | otherwise = Cons ((!!) (toHList l) len) (secondHalfL' l (len + 1))

secondHalfL :: List a -> List a
secondHalfL l = secondHalfL' l (length l `div` 2)

mergeL :: Ord a => List a -> List a -> List a
mergeL Nil l = l
mergeL l Nil = l
mergeL (Cons x xs) (Cons y ys)
    | x < y = Cons x  (mergeL xs (Cons y ys))
    | otherwise = Cons y  (mergeL (Cons x xs) ys)

mergeSortL :: Ord a => List a -> List a
mergeSortL Nil = Nil
mergeSortL (Cons x Nil) = Cons x Nil
mergeSortL l = mergeL (mergeSortL(firstHalfL l)) (mergeSortL (secondHalfL l))
{-
*Main> mergeSortL (toList (reverse [1..1000]))  
Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 (Cons 11 (Cons 12 (Cons 13 (Cons 14 (Cons 15 (Cons 16 (Cons 17 (Cons 18 (Cons 19 (Cons 20 (Cons 21 (Cons 22 (Cons 23 (Cons 24 (Cons 25 (Cons 26 (Cons 27 (Cons 28 (Cons 29 (Cons 30 (Cons 31 (Cons 32 (Cons 33 (Cons 34 (Cons 35 (Cons 36 (Cons 37 (Cons 38 (Cons 39 (Cons 40 (Cons 41 (Cons 42 (Cons 43 (Cons 44 (Cons 45 (Cons 46 (Cons 47 (Cons 48 (Cons 49 (Cons 50 (Cons 51 (Cons 52 (Cons 53 (Cons 54 (Cons 55 (Cons 56 (Cons 57 (Cons 58 (Cons 59 (Cons 60 (Cons 61 (Cons 62 (Cons 63 (Cons 64 (Cons 65 (Cons 66 (Cons 67 (Cons 68 (Cons 69 (Cons 70 (Cons 71 (Cons 72 (Cons 73 (Cons 74 (Cons 75 (Cons 76 (Cons 77 (Cons 78 (Cons 79 (Cons 80 (Cons 81 (Cons 82 (Cons 83 (Cons 84 (Cons 85 (Cons 86 (Cons 87 (Cons 88 (Cons 89 (Cons 90 (Cons 91 (Cons 92 (Cons 93 (Cons 94 (Cons 95 (Cons 96 (Cons 97 (Cons 98 (Cons 99 (Cons 100 (Cons 101 (Cons 102 (Cons 103 (Cons 104 (Cons 105 (Cons 106 (Cons 107 (Cons 108 (Cons 109 (Cons 110 (Cons 111 (Cons 112 (Cons 113 (Cons 114 (Cons 115 (Cons 116 (Cons 117 (Cons 118 (Cons 119 (Cons 120 (Cons 121 (Cons 122 (Cons 123 (Cons 124 (Cons 125 (Cons 126 (Cons 127 (Cons 128 (Cons 129 (Cons 130 (Cons 131 (Cons 132 (Cons 133 (Cons 134 (Cons 135 (Cons 136 (Cons 137 (Cons 138 (Cons 139 (Cons 140 (Cons 141 (Cons 142 (Cons 143 (Cons 144 (Cons 145 (Cons 146 (Cons 147 (Cons 148 (Cons 149 (Cons 150 (Cons 151 (Cons 
152 (Cons 153 (Cons 154 (Cons 155 (Cons 156 (Cons 157 (Cons 158 (Cons 159 (Cons 160 (Cons 161 (Cons 162 (Cons 163 (Cons 164 (Cons 165 (Cons 166 (Cons 167 (Cons 168 (Cons 169 (Cons 170 (Cons 171 (Cons 172 (Cons 173 (Cons 174 (Cons 175 (Cons 176 (Cons 177 (Cons 178 (Cons 179 (Cons 180 (Cons 181 (Cons 182 (Cons 183 (Cons 184 (Cons 185 (Cons 186 (Cons 187 (Cons 188 (Cons 189 (Cons 190 (Cons 191 (Cons 192 (Cons 193 (Cons 194 (Cons 195 (Cons 196 (Cons 197 (Cons 198 (Cons 199 (Cons 200 (Cons 201 (Cons 202 (Cons 203 (Cons 204 (Cons 205 (Cons 206 (Cons 207 (Cons 208 (Cons 209 (Cons 210 (Cons 211 (Cons 212 (Cons 213 (Cons 214 (Cons 215 (Cons 216 (Cons 217 (Cons 218 (Cons 219 (Cons 220 (Cons 221 (Cons 222 (Cons 223 (Cons 224 (Cons 225 (Cons 226 (Cons 227 (Cons 228 (Cons 229 (Cons 230 (Cons 231 (Cons 232 
(Cons 233 (Cons 234 (Cons 235 (Cons 236 (Cons 237 (Cons 238 (Cons 239 (Cons 240 (Cons 241 (Cons 242 (Cons 243 (Cons 244 (Cons 245 (Cons 246 (Cons 247 (Cons 248 (Cons 249 (Cons 250 (Cons 251 (Cons 252 (Cons 253 (Cons 254 (Cons 255 (Cons 256 (Cons 257 (Cons 258 (Cons 259 (Cons 260 (Cons 261 (Cons 262 (Cons 263 (Cons 264 (Cons 265 (Cons 266 (Cons 267 (Cons 268 (Cons 269 (Cons 270 (Cons 271 (Cons 272 (Cons 273 (Cons 274 (Cons 275 (Cons 276 (Cons 277 (Cons 278 (Cons 279 (Cons 280 (Cons 281 (Cons 282 (Cons 283 (Cons 284 (Cons 285 (Cons 286 (Cons 287 (Cons 288 (Cons 289 (Cons 290 (Cons 291 (Cons 292 (Cons 293 (Cons 294 (Cons 295 (Cons 296 (Cons 297 (Cons 298 (Cons 299 (Cons 300 (Cons 301 (Cons 302 (Cons 303 (Cons 304 (Cons 305 (Cons 306 (Cons 307 (Cons 308 (Cons 309 (Cons 310 (Cons 311 (Cons 312 (Cons 313 (Cons 314 (Cons 315 (Cons 316 (Cons 317 (Cons 318 (Cons 319 (Cons 320 (Cons 321 (Cons 322 (Cons 323 (Cons 324 (Cons 325 (Cons 326 (Cons 327 (Cons 328 (Cons 329 (Cons 330 (Cons 331 (Cons 332 (Cons 333 (Cons 334 (Cons 335 (Cons 336 (Cons 337 (Cons 338 (Cons 339 (Cons 340 (Cons 341 (Cons 342 (Cons 343 (Cons 344 (Cons 345 (Cons 346 (Cons 347 (Cons 348 (Cons 349 (Cons 350 (Cons 351 (Cons 352 (Cons 
353 (Cons 354 (Cons 355 (Cons 356 (Cons 357 (Cons 358 (Cons 359 (Cons 360 (Cons 361 (Cons 362 (Cons 363 (Cons 364 (Cons 365 (Cons 366 (Cons 367 (Cons 368 (Cons 369 (Cons 370 (Cons 371 (Cons 372 (Cons 373 (Cons 374 (Cons 375 (Cons 376 (Cons 377 (Cons 378 (Cons 379 (Cons 380 (Cons 381 (Cons 382 (Cons 383 (Cons 384 (Cons 385 (Cons 386 (Cons 387 (Cons 388 (Cons 389 (Cons 390 (Cons 391 (Cons 392 (Cons 393 (Cons 394 (Cons 395 (Cons 396 (Cons 397 (Cons 398 (Cons 399 (Cons 400 (Cons 401 (Cons 402 (Cons 403 (Cons 404 (Cons 405 (Cons 406 (Cons 407 (Cons 408 (Cons 409 (Cons 410 (Cons 411 (Cons 412 (Cons 413 (Cons 414 (Cons 415 (Cons 416 (Cons 417 (Cons 418 (Cons 419 (Cons 420 (Cons 421 (Cons 422 (Cons 423 (Cons 424 (Cons 425 (Cons 426 (Cons 427 (Cons 428 (Cons 429 (Cons 430 (Cons 431 (Cons 432 (Cons 433 
(Cons 434 (Cons 435 (Cons 436 (Cons 437 (Cons 438 (Cons 439 (Cons 440 (Cons 441 (Cons 442 (Cons 443 (Cons 444 (Cons 445 (Cons 446 (Cons 447 (Cons 448 (Cons 449 (Cons 450 (Cons 451 (Cons 452 (Cons 453 (Cons 454 (Cons 455 (Cons 456 (Cons 457 (Cons 458 (Cons 459 (Cons 460 (Cons 461 (Cons 462 (Cons 463 (Cons 464 (Cons 465 (Cons 466 (Cons 467 (Cons 468 (Cons 469 (Cons 470 (Cons 471 (Cons 472 (Cons 473 (Cons 474 (Cons 475 (Cons 476 (Cons 477 (Cons 478 (Cons 479 (Cons 480 (Cons 481 (Cons 482 (Cons 483 (Cons 484 (Cons 485 (Cons 486 (Cons 487 (Cons 488 (Cons 489 (Cons 490 (Cons 491 (Cons 492 (Cons 493 (Cons 494 (Cons 495 (Cons 496 (Cons 497 (Cons 498 (Cons 499 (Cons 500 (Cons 501 (Cons 502 (Cons 503 (Cons 504 (Cons 505 (Cons 506 (Cons 507 (Cons 508 (Cons 509 (Cons 510 (Cons 511 (Cons 512 (Cons 513 (Cons 514 (Cons 515 (Cons 516 (Cons 517 (Cons 518 (Cons 519 (Cons 520 (Cons 521 (Cons 522 (Cons 523 (Cons 524 (Cons 525 (Cons 526 (Cons 527 (Cons 528 (Cons 529 (Cons 530 (Cons 531 (Cons 532 (Cons 533 (Cons 534 (Cons 535 (Cons 536 (Cons 537 (Cons 538 (Cons 539 (Cons 540 (Cons 541 (Cons 542 (Cons 543 (Cons 544 (Cons 545 (Cons 546 (Cons 547 (Cons 548 (Cons 549 (Cons 550 (Cons 551 (Cons 552 (Cons 553 (Cons 
554 (Cons 555 (Cons 556 (Cons 557 (Cons 558 (Cons 559 (Cons 560 (Cons 561 (Cons 562 (Cons 563 (Cons 564 (Cons 565 (Cons 566 (Cons 567 (Cons 568 (Cons 569 (Cons 570 (Cons 571 (Cons 572 (Cons 573 (Cons 574 (Cons 575 (Cons 576 (Cons 577 (Cons 578 (Cons 579 (Cons 580 (Cons 581 (Cons 582 (Cons 583 (Cons 584 (Cons 585 (Cons 586 (Cons 587 (Cons 588 (Cons 589 (Cons 590 (Cons 591 (Cons 592 (Cons 593 (Cons 594 (Cons 595 (Cons 596 (Cons 597 (Cons 598 (Cons 599 (Cons 600 (Cons 601 (Cons 602 (Cons 603 (Cons 604 (Cons 605 (Cons 606 (Cons 607 (Cons 608 (Cons 609 (Cons 610 (Cons 611 (Cons 612 (Cons 613 (Cons 614 (Cons 615 (Cons 616 (Cons 617 (Cons 618 (Cons 619 (Cons 620 (Cons 621 (Cons 622 (Cons 623 (Cons 624 (Cons 625 (Cons 626 (Cons 627 (Cons 628 (Cons 629 (Cons 630 (Cons 631 (Cons 632 (Cons 633 (Cons 634 
(Cons 635 (Cons 636 (Cons 637 (Cons 638 (Cons 639 (Cons 640 (Cons 641 (Cons 642 (Cons 643 (Cons 644 (Cons 645 (Cons 646 (Cons 647 (Cons 648 (Cons 649 (Cons 650 (Cons 651 (Cons 652 (Cons 653 (Cons 654 (Cons 655 (Cons 656 (Cons 657 (Cons 658 (Cons 659 (Cons 660 (Cons 661 (Cons 662 (Cons 663 (Cons 664 (Cons 665 (Cons 666 (Cons 667 (Cons 668 (Cons 669 (Cons 670 (Cons 671 (Cons 672 (Cons 673 (Cons 674 (Cons 675 (Cons 676 (Cons 677 (Cons 678 (Cons 679 (Cons 680 (Cons 681 (Cons 682 (Cons 683 (Cons 684 (Cons 685 (Cons 686 (Cons 687 (Cons 688 (Cons 689 (Cons 690 (Cons 691 (Cons 692 (Cons 693 (Cons 694 (Cons 695 (Cons 696 (Cons 697 (Cons 698 (Cons 699 (Cons 700 (Cons 701 (Cons 702 (Cons 703 (Cons 704 (Cons 705 (Cons 706 (Cons 707 (Cons 708 (Cons 709 (Cons 710 (Cons 711 (Cons 712 (Cons 713 (Cons 714 (Cons 715 (Cons 716 (Cons 717 (Cons 718 (Cons 719 (Cons 720 (Cons 721 (Cons 722 (Cons 723 (Cons 724 (Cons 725 (Cons 726 (Cons 727 (Cons 728 (Cons 729 (Cons 730 (Cons 731 (Cons 732 (Cons 733 (Cons 734 (Cons 735 (Cons 736 (Cons 737 (Cons 738 (Cons 739 (Cons 740 (Cons 741 (Cons 742 (Cons 743 (Cons 744 (Cons 745 (Cons 746 (Cons 747 (Cons 748 (Cons 749 (Cons 750 (Cons 751 (Cons 752 (Cons 753 (Cons 754 (Cons 
755 (Cons 756 (Cons 757 (Cons 758 (Cons 759 (Cons 760 (Cons 761 (Cons 762 (Cons 763 (Cons 764 (Cons 765 (Cons 766 (Cons 767 (Cons 768 (Cons 769 (Cons 770 (Cons 771 (Cons 772 (Cons 773 (Cons 774 (Cons 775 (Cons 776 (Cons 777 (Cons 778 (Cons 779 (Cons 780 (Cons 781 (Cons 782 (Cons 783 (Cons 784 (Cons 785 (Cons 786 (Cons 787 (Cons 788 (Cons 789 (Cons 790 (Cons 791 (Cons 792 (Cons 793 (Cons 794 (Cons 795 (Cons 796 (Cons 797 (Cons 798 (Cons 799 (Cons 800 (Cons 801 (Cons 802 (Cons 803 (Cons 804 (Cons 805 (Cons 806 (Cons 807 (Cons 808 (Cons 809 (Cons 810 (Cons 811 (Cons 812 (Cons 813 (Cons 814 (Cons 815 (Cons 816 (Cons 817 (Cons 818 (Cons 819 (Cons 820 (Cons 821 (Cons 822 (Cons 823 (Cons 824 (Cons 825 (Cons 826 (Cons 827 (Cons 828 (Cons 829 (Cons 830 (Cons 831 (Cons 832 (Cons 833 (Cons 834 (Cons 835 
(Cons 836 (Cons 837 (Cons 838 (Cons 839 (Cons 840 (Cons 841 (Cons 842 (Cons 843 (Cons 844 (Cons 845 (Cons 846 (Cons 847 (Cons 848 (Cons 849 (Cons 850 (Cons 851 (Cons 852 (Cons 853 (Cons 854 (Cons 855 (Cons 856 (Cons 857 (Cons 858 (Cons 859 (Cons 860 (Cons 861 (Cons 862 (Cons 863 (Cons 864 (Cons 865 (Cons 866 (Cons 867 (Cons 868 (Cons 869 (Cons 870 (Cons 871 (Cons 872 (Cons 873 (Cons 874 (Cons 875 (Cons 876 (Cons 877 (Cons 878 (Cons 879 (Cons 880 (Cons 881 (Cons 882 (Cons 883 (Cons 884 (Cons 885 (Cons 886 (Cons 887 (Cons 888 (Cons 889 (Cons 890 (Cons 891 (Cons 892 (Cons 893 (Cons 894 (Cons 895 (Cons 896 (Cons 897 (Cons 898 (Cons 899 (Cons 900 (Cons 901 (Cons 902 (Cons 903 (Cons 904 (Cons 905 (Cons 906 (Cons 907 (Cons 908 (Cons 909 (Cons 910 (Cons 911 (Cons 912 (Cons 913 (Cons 914 (Cons 915 (Cons 916 (Cons 917 (Cons 918 (Cons 919 (Cons 920 (Cons 921 (Cons 922 (Cons 923 (Cons 924 (Cons 925 (Cons 926 (Cons 927 (Cons 928 (Cons 929 (Cons 930 (Cons 931 (Cons 932 (Cons 933 (Cons 934 (Cons 935 (Cons 936 (Cons 937 (Cons 938 (Cons 939 (Cons 940 (Cons 941 (Cons 942 (Cons 943 (Cons 944 (Cons 945 (Cons 946 (Cons 947 (Cons 948 (Cons 949 (Cons 950 (Cons 951 (Cons 952 (Cons 953 (Cons 954 (Cons 955 (Cons 
956 (Cons 957 (Cons 958 (Cons 959 (Cons 960 (Cons 961 (Cons 962 (Cons 963 (Cons 964 (Cons 965 (Cons 966 (Cons 967 (Cons 968 (Cons 969 (Cons 970 (Cons 971 (Cons 972 (Cons 973 (Cons 974 (Cons 975 (Cons 976 (Cons 977 (Cons 978 (Cons 979 (Cons 980 (Cons 981 (Cons 982 (Cons 983 (Cons 984 (Cons 985 (Cons 986 (Cons 987 (Cons 988 (Cons 989 (Cons 990 (Cons 991 (Cons 992 (Cons 993 (Cons 994 (Cons 995 (Cons 996 (Cons 997 (Cons 998 (Cons 999 (Cons 1000 Nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-}

minListL :: Ord a => List a -> Either String a
minListL Nil = Left "Empty"
minListL l = Right  (head (toHList (quickSortL l)))

maxListL :: Ord a => List a -> Either [Char] a
maxListL Nil = Left "Empty"
maxListL l = Right ((!!) (toHList (quickSortL l)) (length l - 1))

mapL :: (a -> b) -> List a -> List b
mapL _ Nil = Nil
mapL f (Cons x l) = Cons (f x) (mapL f l)

fibL' :: List Int-> List Int
fibL' = mapL fib

l' :: List Int
l' = toList [0..]

fibInfL :: List Int
fibInfL = fibL' l'
{-
*Main> fibInfL
Cons 0 (Cons 1 (Cons 1 (Cons 2 (Cons 3 (Cons 5 (Cons 8 (Cons 13 (Cons 21 (Cons 34 (Cons 55 (Cons 89 (Cons 144 (Cons 233 (Cons 377 (Cons 610 (Cons 987 (Cons 1597 (Cons 2584 (Cons 4181 (Cons 6765 (Cons 10946 (Cons 17711 (Cons 28657 (Cons 46368 (Cons 75025 (Cons 121393 (Cons 196418 (Cons 317811 (Cons 514229 (Cons 832040 (Cons 13
-}

infPrimeBoolL :: List Bool
infPrimeBoolL= mapL isPrime l'

infPrimeL :: List Int
infPrimeL = filterL isPrime l'
{-
*Main> infPrimeL
Cons 2 (Cons 3 (Cons 5 (Cons 7 (Cons 11 (Cons 13 (Cons 17 (Cons 19 (Cons 23 (Cons 29 (Cons 31 (Cons 37 (Cons 41 (Cons 43 (Cons 47 (Cons 53 (Cons 59 (Cons 61 (Cons 67 (Cons 71 (Cons 73 (Cons 79 (Cons 83 (Cons 89 (Cons 97 (Cons 101 (Cons 103 (Cons 107 (Cons 109 (Cons 113 (Cons 127 (Cons 131 (Cons 137 (Cons 139 (Cons 149 (Cons 151 (Cons 157 (Cons 163 (Cons 167 (Cons 173 (Cons 179 (Cons 181 (Cons 191 (Cons 193 (Cons 197 (Cons 199 (Cons 211 (Cons 223 (Cons 227 (Cons 229 (Cons 233 (Cons 239 (Cons 241 (Cons 251 (Cons 257 (Cons 263 (Cons 269 (Cons 271 (Cons 277 (Cons 281 (Cons 283 (Cons 293 (Cons 307 (Cons 311 (Cons 313 (Cons 317 (Cons 331 (Cons 337 (Cons 347 (Cons 349 (Cons 353 (Cons 359 (Cons 367 (Cons 373 (Cons 379 (Cons 383 (Cons 389 (Cons 397 (Cons 401 (Cons 409 (Cons 419 (Cons 421 (Cons 431 (Cons 433 (Cons 439 (Cons 443 (Cons 449 (Cons 457 (Cons 461 (Cons 463 (Cons 467 (Cons 479 (Cons 487
-}