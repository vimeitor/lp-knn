import qualified Data.List as List
import qualified Data.Map as Map

data Flower = Flower {
    attributes :: [Float],
    specie :: String
} deriving (Read, Show)

data Data = Data {
    trainSet :: [Flower],
    testSet :: [Flower]
}

type DistanceFunc = Flower -> Flower-> Float
type VoteFunc = WeightPair -> String
type AccuracyFunc = [String] -> [String] -> Float
type WeightPair = [(Float, String)]

euclideanDistance :: DistanceFunc
euclideanDistance (Flower a _) (Flower a2 _) =
    sqrt $ sum $ zipWith (\x y -> (x - y) ** 2) a a2

manhattanDistance :: DistanceFunc
manhattanDistance (Flower a _) (Flower a2 _) =
    sum $ zipWith (\x y -> abs (x - y)) a a2

allDistances :: DistanceFunc -> [Flower] -> (Flower) -> WeightPair
allDistances f l e = zip d s
    where d = map (f e) l
          s = [sp | Flower {specie = sp} <- l]

simpleVote :: VoteFunc
simpleVote l = snd $ maximum reverted
    where ones = map (\x -> (snd x, 1)) l
          added = Map.toList (Map.fromListWith (+) ones)
          reverted = map (\x -> (snd x, fst x)) added

weightedVote :: VoteFunc
weightedVote l = snd $ maximum revert
    where invert = map (\x -> (snd x, 1.0 / (fst x))) l
          add = Map.toList (Map.fromListWith (+) invert)
          revert = map (\x -> (snd x, fst x)) add

evaluateAccuracy :: AccuracyFunc
evaluateAccuracy l ll = (total $ coincide l ll) / (fromIntegral $ length l)
    where coincide p1 p2 = [x == y | (x, y) <- zip p1 p2]
          total t = foldl (\x y -> if y == True then x + 1 else x) 0 t

evaluateError :: [String] -> [String] -> Float
evaluateError l ll = 1 - evaluateAccuracy l ll

knn :: DistanceFunc -> VoteFunc -> AccuracyFunc -> Data -> Int -> Float
knn fdist fvote facc (Data train test) n = eval
    where dists = map (\x -> take n $ List.sort $ (allDistances fdist train x)) test
          spec = map fvote dists
          eval = facc spec [sp | Flower {specie = sp} <- test]

-- parseLines ["4.9,2.4,3.3,1.0,Iris-versicolor\r"]
-- [["4.9","2.4","3.3","1.0","versicolor"]]
parseLines :: [String] -> [[String]]
parseLines [] = []
parseLines (x:xs) = [(init l ++ [la])] ++ (parseLines xs)
    where l = wordsWhen (==',') x
          la = init (drop 5 $ last l)
          wordsWhen p s =
              case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                      where (w, s'') = break p s'


-- getFlowers [["4.9","2.4","3.3","1.0","versicolor"]]
--[Flower {attributes = [4.9,2.4,3.3,1.0], specie = "versicolor"}]
getFlowers :: [[String]] -> [Flower]
getFlowers [] = []
getFlowers ([sl, sw, pl, pw, s]:xs) =
    [Flower { attributes = [slen, swid, plen, pwid], specie = s}] ++ getFlowers xs
        where slen = read sl :: Float
              swid = read sw :: Float
              plen = read pl :: Float
              pwid = read pw :: Float

main = do
    trainContents <- readFile "iris.train.txt"
    testContents <- readFile "iris.test.txt"

    let trainLines = lines trainContents
    let testLines = lines testContents

    let flowerData = Data {
        trainSet = getFlowers $ parseLines trainLines,
        testSet = getFlowers $ parseLines testLines
    }

    print $ knn euclideanDistance simpleVote evaluateAccuracy flowerData 2
