type Row = [Integer]
type Matrix = [Row]

-- Remove col i from the passed row
removeCol :: Row -> Int -> Row
removeCol row i = headOfRow ++ tailOfRow
    where
        headOfRow = take i row
        tailOfRow = drop (i + 1) row

-- Remove col i from all rows of the matrix
removeCols :: Matrix -> Int -> Matrix
removeCols [] _ = []
removeCols (x:xs) i = removeCol x i : removeCols xs i

-- Calculate one element from the sum of the matrix
getDetSumElement :: Matrix -> Int -> Int
getDetSumElement matrix i = sign * element * detSum restOfTheMatrix 0
    where
        -- Sign is calculated as -1 on the power of i + j
        -- Here we always use the first row for calculating the det,
        -- so we always use i + 1. On the other hand in this program
        -- i is always from 0 to n - 1, so we need (i + 1) + 1, which
        -- equals i + 2. Therefore the sign is -1 on power of i + 2.
        -- It is equal to -1 on power of i.
        sign = (-1) ^ i
        element = fromIntegral $ matrix !! 0 !! i
        matrixWithoutFirstRow = tail matrix
        restOfTheMatrix = removeCols matrixWithoutFirstRow i

detSum :: Matrix -> Int -> Int
detSum matrix i | len == 1 = fromIntegral $ matrix !! 0 !! 0
                | len == i = 0
                | otherwise = currentSumElement + detSum matrix (i + 1)
                    where
                       currentSumElement = getDetSumElement matrix i
                       len = length matrix

isSquare :: Matrix -> Int -> Bool
isSquare [] _ = True
isSquare (r:rows) len = len == length r && isSquare rows len

isValid :: Matrix -> Bool
isValid matrix = isSquare matrix (length matrix)

-- This just starts the recursive sum
det :: Matrix -> Maybe Int
det matrix | notValid matrix = Nothing
           | otherwise = Just $ detSum matrix 0
         where
             notValid = not . isValid

-- The program calculates the determinant of the matrix with size n by
-- using the formula that sums Aij * aij * (-1)^(i + j),
-- where j goes from 1 to n and i is some number from 1 to n that
-- remains the same for all elements of the sum.
-- This program makes it work by having i always be 0 and
-- therefore always the first row's elements are used.
mat = [[3, 4, 6], [5, 3, 3], [6, 7, 8]]
main = case det mat of
           Just d  -> print d
           Nothing -> putStrLn "There was an error calculating the determinant!"

