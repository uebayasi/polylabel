module PolyLabel where


import           Control.Lens    ((^.))
import           Data.List       (sortBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Debug.Trace     (trace)
import           Linear.V2



-- POLYLABEL


-- Polygon vertices
type Shape = [V2 Double]
type Polygon = [Shape]


-- XXX Determine the best point instead of returning multiple candidates
-- XXX - For each point, calc the distances to shape segments, and pick up the longest distance
-- XXX - Compare the longest distances of solved cells, and pick up a cell with the shortest distance
polyLabel :: Polygon -> Double -> V2 Double
polyLabel polygon precision =
    let
        precision' = if precision == 0 then 1.0 else precision
        shape = head polygon
        ((minX, minY, maxX, maxY), cells) = getCells polygon
        bestCell =
            let
                centCell = getCentroid polygon
                bboxCell = makeCell (V2 ((minX + maxX) / 2) ((minY + maxY) / 2)) 0 polygon
            in
                if _d bboxCell > _d centCell then bboxCell else centCell

        solvedCells = solveCells polygon precision' (_d bestCell) $
            bestCell : cells
    in
        --trace ("cells: " ++ show cells) $
        --trace ("bestCell: " ++ show bestCell) $
        --trace ("solvedCells: " ++ show solvedCells) $
        -- head $ map _o solvedCells
        _o $ head $ sortByLongest solvedCells


getCells :: Polygon -> ((Double, Double, Double, Double), [Cell])
getCells polygon =
    let
        shape = head polygon
        (minX, minY, maxX, maxY) =
            let xs = map (^._x) shape
                ys = map (^._y) shape
            in (minimum xs, minimum ys, maximum xs, maximum ys)
        width = maxX - minX
        height = maxY - minY
        size = min width height
        h = size / 2

        cells :: [Cell]
        cells = do
            let
                i = floor $ width / size
                j = floor $ height / size
            x <- [x | x <- map ((+ minX) . (* size) . fromIntegral) [0 .. i], x < maxX]
            y <- [y | y <- map ((+ minY) . (* size) . fromIntegral) [0 .. j], y < maxY]
            return $ makeCell (V2 (x + h) (y + h)) h polygon
    in
        --trace ("cells: " ++ show cells) $
        ((minX, minY, maxX, maxY), cells)


-- 1. Filter cells whose (_max cell) is smaller than (_d bestCell + precision)
-- 2. Find the best cell (largest _d)
-- 3. Filter cells again with the new best cell
-- 4. Split remaining cells and repeat
-- 5. Stop if
--      o All the remaining cells have the same _d
--      o Or, only one cell remains
solveCells :: Polygon -> Double -> Double -> [Cell] -> [Cell]
solveCells polygon precision bestDist cells =
    let
        (bestDist', cells') =
            --trace ("solveCells': " ++ show cells) $
            filterCells precision bestDist cells
    in
        if null cells' then
            cells
        else
            let
                cells'' = concatMap (splitCell polygon) cells'
            in
                --trace ("bestDist': " ++ show bestDist') $
                --trace ("cells': " ++ show cells') $
                --trace ("cells'': " ++ show cells'') $
                solveCells polygon precision bestDist' cells''


filterCells :: Double -> Double -> [Cell] -> (Double, [Cell])
filterCells precision bestDist cells =
    let
        cells' = filter (\cell -> _max cell > bestDist + precision) cells
        bestDist' = maximum $ map _d cells'
        cells'' = filter (\cell -> _max cell > bestDist' + precision) cells'
    in
        --trace ("cells': " ++ show cells') $
        --trace ("bestDist': " ++ show bestDist') $
        --trace ("cells'': " ++ show cells'') $
        (bestDist', cells'')


splitCell :: Polygon -> Cell -> [Cell]
splitCell polygon cell =
    let
        (V2 x y) = _o cell
        h = _h cell / 2
    in
        [ makeCell (V2 (x - h) (y - h)) h polygon
        , makeCell (V2 (x + h) (y - h)) h polygon
        , makeCell (V2 (x - h) (y + h)) h polygon
        , makeCell (V2 (x + h) (y + h)) h polygon
        ]



-- CELL


data Cell = Cell
    { _o       :: V2 Double -- center
    , _h       :: Double
    , _d       :: Double
    , _max     :: Double
    , _longest :: Double
    }
    deriving (Show)

instance Eq Cell where
    a == b = _max a == _max b
instance Ord Cell where
    compare a b = compare (_max a) (_max b)


makeCell :: V2 Double -> Double -> Polygon -> Cell
makeCell o h polygon =
    let
        shape = head polygon -- XXX
        (d, longest) = pointToShapeDist o shape
        max = d + h * sqrt 2
    in
        Cell o h d max longest


sortByLongest :: [Cell] -> [Cell]
sortByLongest =
    let
        cmpLongest a b = compare (_longest a) (_longest b)
    in
        sortBy cmpLongest



-- DISTANCE


-- XXX _longest
pointToPolygonDist :: V2 Double -> Polygon -> Double
pointToPolygonDist o polygon =
    let
        ds = map (fst . pointToShapeDist o) polygon
    in
        minimum ds


pointToShapeDist :: V2 Double -> Shape -> (Double, Double)
pointToShapeDist o@(V2 ox oy) shape =
    let
        pqs = zip (last shape : shape) shape
        xxx (p@(V2 px py), q@(V2 qx qy)) =
            let
                inside = ((py > oy) /= (qy > oy)) && (ox < (qx - px) * (oy - py) / (qy - py) + px)
                dist = getSegDistSq o p q
            in
                (inside, dist)
        xs = map xxx pqs
        ninsides = length $ filter id $ map fst xs
        sign = if odd ninsides then 1 else -1
        dists = map snd xs
        mindist = minimum dists
        maxdist = maximum dists
    in
        --trace ("pqs: " ++ show pqs) $
        --trace ("xs: " ++ show xs) $
        --trace ("sign: " ++ show sign) $
        --trace ("mindist: " ++ show mindist) $
        (sign * sqrt mindist, sign * sqrt maxdist)


getSegDistSq :: V2 Double -> V2 Double -> V2 Double -> Double
getSegDistSq (V2 ox oy) (V2 px py) (V2 qx qy) =
    let
        dx = qx - px
        dy = qy - py
        (x, y) =
            if dx /= 0 || dy /= 0 then
                let
                    t = ((ox - px) * dx + (oy - py) * dy) / (dx * dx + dy * dy)
                in
                    if t > 1 then (qx, qy)
                    else if t > 0 then (px + dx * t, py + dy * t)
                    else (px, py)
            else
                (px, py)
        pdx = ox - x
        pdy = oy - y
    in
        pdx * pdx + pdy * pdy



-- CENTROID


getCentroid :: Polygon -> Cell
getCentroid polygon =
    let
        shape = head polygon -- XXX
        pqs = zip (last shape : shape) shape

        xxx (V2 px py, V2 qx qy) =
            let
                f = px * qy - qx * py
                dx = (px + qx) * f
                dy = (py + qy) * f
                darea = f * 3
            in
                (V2 dx dy, darea)
        fs = map xxx pqs
        V2 x y = foldr ((+) . fst) (V2 0 0) fs
        area = sum $ map snd fs
    in
        --trace ("pqs: " ++ show pqs) $
        --trace ("fs: " ++ show fs) $
        --trace ("area: " ++ show area) $
        if area == 0
            then let V2 hx hy = head shape in makeCell (V2 hx hy) 0 polygon
            else makeCell (V2 (x / area) (y / area)) 0 polygon
