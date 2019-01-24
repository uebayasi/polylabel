module PolyLabelSpec where

import           Test.Hspec

import           Control.Lens               ((%~), (&), (.~), (^.), (^?))
import qualified Control.Lens               as Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set                   as Set
import           Debug.Trace                (trace)
import           Linear.V2
import           System.Environment
import           System.IO

import           PolyLabel


hoge :: IO ()
hoge = do
    putStrLn "hoge"
    putStrLn "fuga"


spec = before_ hoge $ do
    describe "cell" $
        it "cell" $
            1.0 `shouldNotBe` 0.9999999999
    describe "centroid" $ do
        it "centroid1" $
            let shape = head unitPolygon in
            getCentroid unitPolygon
            `shouldBe`
            makeCell (V2 0.5 0.5) 0 unitPolygon
        it "centroid2" $
            let
                shape = head unitPolygon
                c = getCentroid unitPolygon
                (V2 ox oy) = _o c
                x = 1 - 1/6
                y = 1 + 1/6
            in
                (ox - x < 0.000001, oy - y < 0.000001)
                `shouldBe`
                (True, True)
    describe "getSegDistSq" $ do
        it "getSegDistSq1" $
            getSegDistSq (V2 0.5 0.5) (V2 0.0 0.0) (V2 1.0 0.0) `shouldBe` (0.5 * 0.5)
        it "getSegDistSq2" $
            getSegDistSq (V2 0.5 0.25) (V2 0.0 0.0) (V2 1.0 0.0) `shouldBe` (0.25 * 0.25)
    describe "pointToShapeDist" $ do
        it "pointToShapeDist1" $
            let (d, _) = pointToShapeDist (V2 0.5 0.5) unitShape
            in d `shouldBe` 0.5
        it "pointToShapeDist2" $
            let (d, _) = pointToShapeDist (V2 0.5 0.25) unitShape
            in d `shouldBe` 0.25
        it "pointToShapeDist3" $
            let (d, _) = pointToShapeDist (V2 0.5 (-0.25)) unitShape
            in d `shouldBe` -0.25
        it "pointToShapeDist4" $
            let (d, _) = pointToShapeDist (V2 0.5 1.25) unitShape
            in d `shouldBe` -0.25
    describe "splitCell" $
        it "splitCell-unit" $
            let
                shape = head unitPolygon
                cell = makeCell (V2 0.5 0.5) 0.5 unitPolygon
                max' = 0.25 + 0.25 * sqrt 2
                ohdmax c =
                    (_o c, _h c, _d c, _max c)
            in
                map ohdmax (splitCell unitPolygon cell)
                `shouldBe`
                [ (V2 0.25 0.25,  0.25,  0.25, max')
                , (V2 0.75 0.25,  0.25,  0.25, max')
                , (V2 0.25 0.75,  0.25,  0.25, max')
                , (V2 0.75 0.75,  0.25,  0.25, max')
                ]
    describe "filterCells" $
        it "filterCells" $
            let
                shape = head testPolygon
                cells = splitCell testPolygon $
                    makeCell (V2 0.75 1.25) 0.25 testPolygon
                bestDist = 1/6 * sqrt 2 -- from known centroid
                (bestDist', cells') = filterCells 0.001 bestDist cells
                bestDistX = (0.5 - 0.125) * sqrt 2
                diff = abs $ bestDist' - bestDistX
            in
                -- XXX Check the filtered cell
                diff < 0.001 `shouldBe` True
    describe "solveCells" $ do
        it "solveCells1" $
            let
                shape = head testPolygon
                centCell = getCentroid testPolygon
                cells = splitCell testPolygon $
                    makeCell (V2 1.0 1.0) 1.0 testPolygon
                diff = abs $ solveTestPolygon centCell cells
            in
                diff < 0.001 `shouldBe` True
        it "solveCells2" $
            let
                shape = head testPolygon
                (_, cells) =
                    --makeCell (V2 1.0 1.0) 1.0 testPolygon
                    getCells testPolygon
                centCell = getCentroid testPolygon
                diff = abs $ solveTestPolygon centCell cells
            in
                diff < 0.001 `shouldBe` True
    describe "getCells" $ do
        it "getCells-unit" $
            let
                shape = head unitPolygon
                ((minX, minY, maxX, maxY), cells) = getCells unitPolygon
            in
                --trace ("(minX,minY,maxX,maxY): " ++ show (minX,minY,maxX,maxY)) $
                length cells `shouldBe` 1
        it "getCells-wide" $
            let
                shape = head widePolygon
                ((minX, minY, maxX, maxY), cells) = getCells widePolygon
            in
                --trace ("(minX,minY,maxX,maxY): " ++ show (minX,minY,maxX,maxY)) $
                length cells `shouldBe` 2
    describe "polyLabel" $
        it "polyLabel" $
            let
                (V2 ox oy) = polyLabel testPolygon 0.001
                (x, y) = (0.58, 1.41)
            in
                --trace ("points: " ++ show points) $
                --length points > 0 `shouldBe` True
                (abs (ox - x) < 0.01, abs (oy - y) < 0.01) `shouldBe` (True, True)
{-
        -- XXX Fix solveCells to consider multiple shapes in one polygon
        it "polyLabel2" $ do
            water1 <- decodePolygon "test/water1.json"
            1 `shouldBe` 1
        it "polyLabel3" $ do
            water2 <- decodePolygon "test/water2.json"
            1 `shouldBe` 1
-}



unitShape :: Shape
unitShape = [V2 0.0 0.0, V2 1.0 0.0, V2 1.0 1.0, V2 0.0 1.0]


unitPolygon :: Polygon
unitPolygon = [unitShape]


wideShape :: Shape
wideShape = [V2 0.0 0.0, V2 1.5 0.0, V2 1.5 1.0, V2 0.0 1.0]


widePolygon :: Polygon
widePolygon = [wideShape]



{-
 x0 1 2
y
0 +-+
  | |
1 | +-+
  |   |
2 +---+
-}
testPolygon :: Polygon
testPolygon =
    [
    [ V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 2.0 1.0
    , V2 2.0 2.0
    , V2 0.0 2.0
    ]
    ]


solveTestPolygon :: Cell -> [Cell] -> Double
solveTestPolygon bestCell cells =
    let
        solvedCells = solveCells testPolygon 0.001 (_d bestCell) $
            bestCell : cells
        -- Take the first result
        a = head solvedCells
        distX = 0.584 -- XXX
        diff = _d a - distX
    in
        diff


decodePolygon :: String -> IO [[V2 Double]]
decodePolygon path = do
    h <- openFile path ReadMode
    json <- BS.hGetContents h
    case eitherDecode json :: Either String [[[Int]]] of
        Left err ->
            undefined
        Right v ->
            return $
                map (map listToVec2) v


listToVec2 :: [Int] -> V2 Double
listToVec2 []     = undefined
listToVec2 [_]    = undefined
listToVec2 [x, y] = V2 (fromIntegral x) (fromIntegral y)
listToVec2 _      = undefined
