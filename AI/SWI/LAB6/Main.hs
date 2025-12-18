-- Main.hs

module Main where

import Control.Monad (when)
import Data.List     (nubBy, sort, dropWhileEnd)
import Data.Char     (isSpace)
import Text.Read     (readMaybe)

-- ===== Базовые типы =====

data Vec3 = Vec3 { vx :: Double, vy :: Double, vz :: Double }
  deriving (Eq, Show)

data Ellipsoid = Ellipsoid
  { c  :: Vec3   -- центр
  , ax :: Double -- полуось по x
  , ay :: Double -- полуось по y
  , az :: Double -- полуось по z
  } deriving (Show)

type Point = Vec3
type Edge  = (Point, Point)

-- ===== Векторная математика =====

(.+.) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) .+. (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)

(.-.) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) .-. (Vec3 x2 y2 z2) = Vec3 (x1-x2) (y1-y2) (z1-z2)

(.*) :: Double -> Vec3 -> Vec3
s .* (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

dist2 :: Vec3 -> Vec3 -> Double
dist2 a b = let Vec3 dx dy dz = b .-. a
            in dx*dx + dy*dy + dz*dz

norm :: Vec3 -> Double
norm v = sqrt (dist2 v (Vec3 0 0 0))

-- ===== Набор автотестов  =====

runCase :: String -> Ellipsoid -> [Point] -> IO ()
runCase name ell cube = do
  let eps = 1.0e-6
  putStrLn $ "=== " ++ name ++ " ==="
  case validateCube cube eps of
    Left err -> putStrLn $ "Ошибка куба: " ++ err
    Right (edgeLen, edges) -> do
      putStrLn $ "Куб принят. Длина ребра ≈ " ++ show edgeLen
      case validateEllipsoid ell of
        Left errE -> putStrLn $ "Ошибка эллипсоида: " ++ errE
        Right ()  -> do
          let pts = intersectEllipsoidCube ell cube edges edgeLen eps
          printPoints pts
  putStrLn ""

selftestOk :: IO ()
selftestOk =
  let ell  = Ellipsoid (Vec3 0 0 0) 1.5 1.5 0.5
      cube =
        [ Vec3 (-1) (-1) (-1), Vec3   1  (-1) (-1)
        , Vec3 (-1)   1  (-1), Vec3   1   1  (-1)
        , Vec3 (-1) (-1)   1 , Vec3   1  (-1)  1
        , Vec3 (-1)   1    1 , Vec3   1   1   1
        ]
  in runCase "selftestOk (есть пересечения)" ell cube

selftestTouch :: IO ()
selftestTouch =
  let ell  = Ellipsoid (Vec3 0 0 0) 1 1 1
      cube =
        [ Vec3 (-1) (-1) (-1), Vec3   1  (-1) (-1)
        , Vec3 (-1)   1  (-1), Vec3   1   1  (-1)
        , Vec3 (-1) (-1)   1 , Vec3   1  (-1)  1
        , Vec3 (-1)   1    1 , Vec3   1   1   1
        ]
  in runCase "selftestTouch (соприкосновение)" ell cube

selftestNone :: IO ()
selftestNone =
  let ell  = Ellipsoid (Vec3 3 0 0) 0.4 0.4 0.4
      cube =
        [ Vec3 (-1) (-1) (-1), Vec3   1  (-1) (-1)
        , Vec3 (-1)   1  (-1), Vec3   1   1  (-1)
        , Vec3 (-1) (-1)   1 , Vec3   1  (-1)  1
        , Vec3 (-1)   1    1 , Vec3   1   1   1
        ]
  in runCase "selftestNone (пересечений нет)" ell cube

-- ===== Эллипсоидная F(t) =====

ellipsoidF :: Ellipsoid -> Point -> Double
ellipsoidF (Ellipsoid (Vec3 cx cy cz) ax ay az) (Vec3 x y z) =
  let nx = (x - cx) / ax
      ny = (y - cy) / ay
      nz = (z - cz) / az
  in nx*nx + ny*ny + nz*nz - 1.0

pointOnSegment :: Point -> Point -> Double -> Point
pointOnSegment a b t = a .+. (t .* (b .-. a))

-- Принадлежность поверхности с допуском
onEllipsoid :: Ellipsoid -> Point -> Double -> Bool
onEllipsoid ell p eps =
  let t  = ellipsoidF ell p + 1.0 
      d  = abs (t - 1.0)
      tol = max (50*eps) 1.0e-6
  in d <= tol

-- ===== Решение квадратного уравнения =====

solveQuadratic :: Double -> Double -> Double -> Double -> [Double]
solveQuadratic a b c eps
  | abs a <= eps =
      if abs b <= eps
      then if abs c <= eps then [0.0,1.0] else []
      else [-c / b]
  | otherwise =
      let d = b*b - 4*a*c
      in if d < -eps
         then []
         else if abs d <= eps
              then [-b / (2*a)]
              else let s  = sqrt (max 0 d)
                       t1 = (-b - s) / (2*a)
                       t2 = (-b + s) / (2*a)
                   in [t1,t2]

in01 :: Double -> Double -> Bool
in01 eps t = t >= -eps && t <= 1.0+eps

-- ===== Бисекция + сканирование =====

bisection :: (Double -> Double) -> Double -> Double -> Double -> Int -> Maybe Double
bisection f lo hi eps maxIt = go lo hi maxIt
  where
    go a b 0 = Just ((a+b)/2)
    go a b n =
      let m  = (a+b)/2
          fa = f a
          fm = f m
      in if (b-a) <= eps
         then Just m
         else if fa == 0 then Just a
         else if fm == 0 then Just m
         else if fa * fm <= 0
              then go a m (n-1)
              else go m b (n-1)

scanWithBisection :: (Double -> Double) -> Double -> Double -> Double -> [Double]
scanWithBisection f a b eps =
  let steps = 400 :: Int
      step  = (b - a) / fromIntegral steps
      epsT  = max 1.0e-9 (50*eps)
      ks    = [0 .. steps-1]
      brackets = [ (t0,t1)
                 | k <- ks
                 , let t0 = a + fromIntegral k*step
                 , let t1 = t0 + step
                 , let f0 = f t0
                 , let f1 = f t1
                 , f0*f1 <= 0
                 ]
      roots = [ r
              | (lo,hi) <- brackets
              , Just r <- [bisection f lo hi epsT 60]
              ]
  in uniqByTol 1.0e-9 (sort roots)

-- ===== Пересечение отрезка с эллипсоидом =====

segmentEllipsoidIntersections :: Ellipsoid -> Point -> Point -> Double -> [Point]
segmentEllipsoidIntersections ell@(Ellipsoid c ax ay az) a b eps =
  let d = b .-. a
      u@(Vec3 ux uy uz) = a .-. c
      Vec3 vx vy vz    = d
      qa = (vx/ax)^2 + (vy/ay)^2 + (vz/az)^2
      qb = 2 * ((ux*vx)/(ax*ax) + (uy*vy)/(ay*ay) + (uz*vz)/(az*az))
      qc = (ux/ax)^2 + (uy/ay)^2 + (uz/az)^2 - 1.0
      tsAnal  = filter (in01 eps) (solveQuadratic qa qb qc eps)
      ptsAnal = map (pointOnSegment a b) tsAnal
  in if not (null ptsAnal)
     then ptsAnal
     else
       let f t    = ellipsoidF ell (pointOnSegment a b t)
           tsNum  = filter (in01 eps) (scanWithBisection f 0 1 eps)
       in map (pointOnSegment a b) tsNum

-- ===== Проверка куба =====

approx :: Double -> Double -> Double -> Bool
approx a b tol = abs (a-b) <= tol

minPositive :: [Double] -> Maybe Double
minPositive xs =
  case filter (>0) xs of
    [] -> Nothing
    ys -> Just (minimum ys)

classifyDistances
  :: [Double] -> Double -> Double -> (Int,Int,Int,Int)
classifyDistances ds e2 eps =
  let tol  = max eps (e2*1.0e-6)
      e22  = 2*e2
      e23  = 3*e2
      step (ce,cf,cs,cb) d
        | abs (d - e2)  <= tol = (ce+1,cf,cs,cb)
        | abs (d - e22) <= tol = (ce,cf+1,cs,cb)
        | abs (d - e23) <= tol = (ce,cf,cs+1,cb)
        | otherwise            = (ce,cf,cs,cb+1)
  in foldl step (0,0,0,0) ds

buildEdges :: [Point] -> Double -> Double -> [Edge]
buildEdges vs e2 eps =
  let tol = max eps (e2*1.0e-6)
      n   = length vs
      pairs = [ (i,j)
              | i <- [0..n-1], j <- [i+1..n-1]
              , let d2 = dist2 (vs !! i) (vs !! j)
              , abs (d2 - e2) <= tol
              ]
  in [ (vs !! i, vs !! j) | (i,j) <- pairs ]

validateEllipsoid :: Ellipsoid -> Either String ()
validateEllipsoid (Ellipsoid _ ax ay az)
  | any (<=0) [ax,ay,az] =
      Left $ "Полуоси эллипсоида должны быть > 0. Найдены: Ax="
          ++ show ax ++ ", Ay=" ++ show ay ++ ", Az=" ++ show az
  | otherwise = Right ()

validateCube
  :: [Point] -> Double -> Either String (Double, [Edge])
validateCube vs eps
  | length vs /= 8 =
      Left $ "Куб: нужно ровно 8 вершин, получено " ++ show (length vs)
  | length (nubBy samePoint vs) /= 8 =
      Left "Куб: есть совпадающие вершины."
  | otherwise =
      let n   = length vs
          ds2 = [ dist2 (vs !! i) (vs !! j)
                | i <- [0..n-1], j <- [i+1..n-1]
                ]
      in case minPositive ds2 of
           Nothing ->
             Left "Куб: не найдено ни одного положительного расстояния."
           Just e2 ->
             if e2 <= 0
             then Left "Куб: минимальное ненулевое расстояние не положительно."
             else
               let (ce,cf,cs,cb) = classifyDistances ds2 e2 eps
               in if ce == 12 && cf == 12 && cs == 4 && cb == 0
                  then
                    let edgeLen = sqrt e2
                        edges   = buildEdges vs e2 eps
                    in Right (edgeLen, edges)
                  else
                    Left $ "Эти точки не образуют куб. "
                        ++ "рёбра=" ++ show ce
                        ++ ", диагонали_грани=" ++ show cf
                        ++ ", диагонали_пространства=" ++ show cs
                        ++ ", прочие=" ++ show cb
  where
    samePoint p q = norm (p .-. q) < 1.0e-9

-- ===== Касания граней для куба =====

cubeExtentsAxisAligned
  :: [Point] -> Double -> Maybe (Double,Double,Double,Double,Double,Double)
cubeExtentsAxisAligned vs eps =
  let xs = map vx vs
      ys = map vy vs
      zs = map vz vs
      xmin = minimum xs
      xmax = maximum xs
      ymin = minimum ys
      ymax = maximum ys
      zmin = minimum zs
      zmax = maximum zs
      twoLevels mn mx cs = all (\v -> approx v mn (5*eps) || approx v mx (5*eps)) cs
  in if twoLevels xmin xmax xs
        && twoLevels ymin ymax ys
        && twoLevels zmin zmax zs
     then Just (xmin,xmax,ymin,ymax,zmin,zmax)
     else Nothing

axisAlignedFaceTangents
  :: Ellipsoid -> [Point] -> Double -> [Point]
axisAlignedFaceTangents (Ellipsoid (Vec3 cx cy cz) ax ay az) vs eps =
  case cubeExtentsAxisAligned vs eps of
    Nothing -> []
    Just (xmin,xmax,ymin,ymax,zmin,zmax) ->
      let ptsX =
            [ Vec3 xc cy cz
            | xc <- [xmin,xmax]
            , let nx = (xc - cx)/ax
            , let s  = 1 - nx*nx
            , abs s <= 5*eps
            , cy >= min ymin ymax - eps
            , cy <= max ymin ymax + eps
            , cz >= min zmin zmax - eps
            , cz <= max zmin zmax + eps
            ]
          ptsY =
            [ Vec3 cx yc cz
            | yc <- [ymin,ymax]
            , let ny = (yc - cy)/ay
            , let s  = 1 - ny*ny
            , abs s <= 5*eps
            , cx >= min xmin xmax - eps
            , cx <= max xmin xmax + eps
            , cz >= min zmin zmax - eps
            , cz <= max zmin zmax + eps
            ]
          ptsZ =
            [ Vec3 cx cy zc
            | zc <- [zmin,zmax]
            , let nz = (zc - cz)/az
            , let s  = 1 - nz*nz
            , abs s <= 5*eps
            , cx >= min xmin xmax - eps
            , cx <= max xmin xmax + eps
            , cy >= min ymin ymax - eps
            , cy <= max ymin ymax + eps
            ]
      in uniqPoints eps (ptsX ++ ptsY ++ ptsZ)

-- ===== Дедупликация точек =====

uniqByTol :: Double -> [Double] -> [Double]
uniqByTol tol = foldr add []
  where
    add x acc
      | any (\y -> abs (x-y) <= tol) acc = acc
      | otherwise                        = x:acc

uniqPoints :: Double -> [Point] -> [Point]
uniqPoints eps = foldr add []
  where
    add p acc
      | any (\q -> dist2 p q <= eps*eps) acc = acc
      | otherwise                            = p:acc

-- ===== Пересечение эллипсоид ∩ куб =====

intersectEllipsoidCube
  :: Ellipsoid -> [Point] -> [Edge] -> Double -> Double -> [Point]
intersectEllipsoidCube ell vs edges edgeLen eps =
  let edgePts =
        [ p
        | (a,b) <- edges
        , p     <- segmentEllipsoidIntersections ell a b eps
        ]
      vertexPts =
        [ v
        | v <- vs
        , onEllipsoid ell v eps
        ]
      facePts = axisAlignedFaceTangents ell vs eps
      allPts  = edgePts ++ vertexPts ++ facePts
      tol     = max eps (edgeLen * 1.0e-9)
  in uniqPoints tol allPts

-- ===== Ввод с консоли =====

readDouble :: String -> IO Double
readDouble prompt = do
  putStr prompt
  line <- getLine
  case readMaybe line of
    Just x  -> return x
    Nothing -> do
      putStrLn "Ошибка: нужно ввести число."
      readDouble prompt

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseVec3 :: String -> Maybe Vec3
parseVec3 s =
  let t = trim s
  in case (readMaybe t :: Maybe [Double]) of
       Just [x,y,z] -> Just (Vec3 x y z)
       _ ->
         case words t of
           [sx,sy,sz] ->
             case (readMaybe sx, readMaybe sy, readMaybe sz) of
               (Just x, Just y, Just z) -> Just (Vec3 x y z)
               _                         -> Nothing
           _ -> Nothing

readVec3 :: String -> IO Vec3
readVec3 prompt = do
  putStrLn prompt
  putStrLn "Введите 3 числа через пробел или список вида [x,y,z]:"
  putStr "> "
  line <- getLine
  case parseVec3 line of
    Just v  -> return v
    Nothing -> do
      putStrLn "Ошибка: ожидалось три числа (например: 1 2 3 или [1,2,3])."
      readVec3 prompt

readEllipsoid :: IO Ellipsoid
readEllipsoid = do
  putStrLn "=== Ввод эллипсоида ==="
  ctr <- readVec3 "Центр эллипсоида:"
  ax' <- readDouble "Полуось Ax > 0: "
  ay' <- readDouble "Полуось Ay > 0: "
  az' <- readDouble "Полуось Az > 0: "
  let ell = Ellipsoid ctr ax' ay' az'
  case validateEllipsoid ell of
    Left err -> do
      putStrLn $ "Ошибка эллипсоида: " ++ err
      readEllipsoid
    Right () -> do
      putStrLn "✓ Эллипсоид принят."
      return ell

readCube :: IO [Point]
readCube = do
  putStrLn "=== Ввод куба ==="
  let go i acc
        | i > 8     = return (reverse acc)
        | otherwise = do
            putStrLn $ "Вершина " ++ show i ++ ":"
            p <- readVec3 ""
            go (i+1) (p:acc)
  go 1 []

-- ===== Печать результата =====

printPoints :: [Point] -> IO ()
printPoints [] = putStrLn "Точек пересечения нет."
printPoints ps = do
  putStrLn $ "Найдено точек: " ++ show (length ps)
  mapM_ printPoint ps
  where
    printPoint (Vec3 x y z) =
      putStrLn $ "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"


main :: IO ()
main = do
  let eps = 1.0e-6
  putStrLn "=== Пересечение эллипсоида и куба (Haskell) ==="
  ell <- readEllipsoid
  vs  <- readCube
  case validateCube vs eps of
    Left err -> putStrLn $ "Ошибка куба: " ++ err
    Right (edgeLen, edges) -> do
      putStrLn $ "✓ Куб принят. Длина ребра ≈ " ++ show edgeLen
      putStrLn "Считаю пересечение..."
      let pts = intersectEllipsoidCube ell vs edges edgeLen eps
      putStrLn "Результат:"
      printPoints pts
      putStrLn "Готово."