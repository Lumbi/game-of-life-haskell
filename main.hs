import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT
import Data.Bits
import Data.Matrix as DM
import Data.Maybe
import Control.Monad
import Control.Concurrent.MVar

data GameState = GameState { gameTime::Int, gameCells::CellMatrix }

main :: IO ()
main = do
    (_, _) <- GLUT.getArgsAndInitialize
    _ <- GLUT.createWindow "Game of life"
    GLUT.windowSize $= Size 800 800
    gameStateVar <- newMVar $ GameState 0 (cells 0)
    GLUT.displayCallback $= display gameStateVar
    GLUT.addTimerCallback updateTimeout (update gameStateVar)
    GLUT.mouseCallback $= Just (mouseClick gameStateVar)
    GLUT.mainLoop

-- Color API

color4fromHex :: Int -> Color4 GLfloat
color4fromHex hexa =
    let r = fromIntegral (hexa `shiftR` 24 .&. 255) / 255
        g = fromIntegral (hexa `shiftR` 16 .&. 255) / 255
        b = fromIntegral (hexa `shiftR` 8 .&. 255) / 255
        a = fromIntegral (hexa .&. 255) / 255
    in Color4 r g b a

color3fromHex :: Int -> Color3 GLfloat
color3fromHex hexa =
    let r = fromIntegral (hexa `shiftR` 16 .&. 255) / 255
        g = fromIntegral (hexa `shiftR` 8 .&. 255) / 255
        b = fromIntegral (hexa .&. 255) / 255
    in Color3 r g b

-- Model API

addVertex :: GLfloat -> GLfloat -> GLfloat -> IO ()
addVertex x y z =
    vertex $ Vertex3 x y z

drawRectangle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawRectangle x y w h =
    renderPrimitive Quads $ do
        addVertex x y 0
        addVertex (x+w) y 0
        addVertex (x+w) (y+h) 0
        addVertex x (y+h) 0

drawSquare :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawSquare x y s = drawRectangle x y s s

drawUnitSquare :: GLfloat -> GLfloat -> IO ()
drawUnitSquare x y = drawSquare x y 1

drawCube :: GLfloat -> GLfloat -> GLfloat -> IO()
drawCube cx cy side =
    renderPrimitive Quads $ do
        -- top x-y
        addVertex (cx - halfside) (cy - halfside) halfside
        addVertex (cx + halfside) (cy - halfside) halfside
        addVertex (cx + halfside) (cy + halfside) halfside
        addVertex (cx - halfside) (cy + halfside) halfside

        -- bottom x-y
        addVertex (cx - halfside) (cy - halfside) (-halfside)
        addVertex (cx - halfside) (cy + halfside) (-halfside)
        addVertex (cx + halfside) (cy + halfside) (-halfside)
        addVertex (cx + halfside) (cy - halfside) (-halfside)

        -- top z-y
        addVertex halfside (cx - halfside) (cy - halfside)
        addVertex halfside (cx + halfside) (cy - halfside)
        addVertex halfside (cx + halfside) (cy + halfside)
        addVertex halfside (cx - halfside) (cy + halfside)

        -- bottom x-y
        addVertex (cx - halfside) (cy - halfside) (-halfside)
        addVertex (cx + halfside) (cy - halfside) (-halfside)
        addVertex (cx + halfside) (cy + halfside) (-halfside)
        addVertex (cx - halfside) (cy + halfside) (-halfside)

        where
            halfside = side / (2::GLfloat)

drawAxis :: IO ()
drawAxis =
    renderPrimitive Lines $ do
        color $ color3fromHex 0xFF0000
        addVertex (-999) 0 0
        addVertex 999 0 0

        color $ color3fromHex 0x00FF00
        addVertex 0 (-999) 0
        addVertex 0 999 0

        color $ color3fromHex 0x0000FF
        addVertex 0 0 (-999)
        addVertex 0 0 999

-- Rendering

display :: MVar GameState -> GLUT.DisplayCallback
display gameStateVar = do
    clearColor $= color4fromHex 0x2b2b2bff
    clear [ColorBuffer]
    loadIdentity
    scale 0.05 0.05 (0.05::GLfloat)
    GLUT.rotate 180 $ Vector3 1 0 (0::GLfloat)
    translate $ Vector3 (-20) (-20) (0::GLfloat)

    drawAxis

    gameState <- readMVar gameStateVar
    mapM_ drawCell $ gameCells gameState

    -- print $ gameCells gameState

    flush

    return ()

updateTimeout :: Int
updateTimeout = 80

update :: MVar GameState -> GLUT.TimerCallback
update gameStateVar = do
    gameState <- takeMVar gameStateVar
    let nextTime = gameTime gameState + 1
    let nextGen = nextGeneration $ gameCells gameState
    putMVar gameStateVar $ GameState nextTime nextGen
    GLUT.addTimerCallback updateTimeout (update gameStateVar)
    GLUT.postRedisplay Nothing
    -- putStrLn $ "Generation " ++ show nextTime
    return ()

mouseClick :: MVar GameState -> GLUT.MouseCallback
mouseClick gameStateVar GLUT.LeftButton GLUT.Down (GLUT.Position screenX screenY) = do
    gameState <- takeMVar gameStateVar
    let nextTime = gameTime gameState
    let nextGen = cellMatrix (\pos -> spawnCell (mouseX, mouseY) (gameCells gameState ! pos) pos)
    putMVar gameStateVar $ GameState nextTime nextGen
    GLUT.postRedisplay Nothing
    return ()
    where
        mouseX = floor $ (fromIntegral screenX ::Double) / 20.0
        mouseY = floor $ (fromIntegral screenY ::Double) / 20.0
mouseClick _ _ _ _ = return ()

spawnCell :: (Int, Int) -> Maybe Cell -> (Int, Int) -> Maybe Cell
spawnCell (mouseX, mouseY) Nothing (i, j) =
    if mouseX == i && mouseY == j
        then Just $ Cell i j
        else Nothing
spawnCell _ cell _ = cell

drawCell :: Maybe Cell -> IO ()
drawCell Nothing = return ()
drawCell (Just (Cell {_x = cellx , _y = celly})) = do
        color $ Color3 cellr cellg cellb
        drawUnitSquare glCellx glCelly
        where
            glCellx = fromIntegral cellx::GLfloat
            glCelly = fromIntegral celly::GLfloat
            cellr = glCellx / 10.0
            cellg = glCelly / 10.0
            cellb = 0.5

-- Game of life

data Cell = Cell { _x::Int, _y::Int } deriving Show

type CellMatrix = DM.Matrix (Maybe Cell)
cellMatrix :: ((Int, Int) -> Maybe Cell) -> CellMatrix
cellMatrix = DM.matrix 40 40

nextGeneration :: CellMatrix -> CellMatrix
nextGeneration currentGeneration = cellMatrix $ \(i,j) -> evaluateCell (i,j, currentGeneration)

evaluateCell :: (Int, Int, CellMatrix) -> Maybe Cell
evaluateCell (i,j, _cells)
    | aliveNeighboursCount < 2 = Nothing
    | aliveNeighboursCount > 3 = Nothing
    | isJust $ _cells ! (i,j) = Just (Cell i j)
    | aliveNeighboursCount == 3 = Just (Cell i j)
    | otherwise = Nothing
    where
        neighbourCells = map (neighbourCell (i, j) _cells) allCellPositions :: [Maybe Cell]
        aliveNeighboursCount = length $ filter isJust neighbourCells

data CellPosition = CellTop | CellLeft | CellBottom | CellRight | CellTopLeft | CellTopRight | CellBottomLeft | CellBottomRight
allCellPositions :: [CellPosition]
allCellPositions = [CellTop, CellLeft, CellBottom, CellRight, CellTopLeft, CellTopRight, CellBottomLeft, CellBottomRight]

neighbourCell :: (Int, Int) -> CellMatrix -> CellPosition -> Maybe Cell
neighbourCell (i, j) _cells pos = join $ _neighbourCell (i, j) _cells pos

_neighbourCell :: (Int, Int) -> CellMatrix -> CellPosition -> Maybe (Maybe Cell)
_neighbourCell (i, j) _cells CellTop = safeGet i (j-1) _cells
_neighbourCell (i, j) _cells CellLeft = safeGet (i-1) j _cells
_neighbourCell (i, j) _cells CellBottom = safeGet i (j+1) _cells
_neighbourCell (i, j) _cells CellRight = safeGet (i+1) j _cells
_neighbourCell (i, j) _cells CellTopLeft = safeGet (i-1) (j-1) _cells
_neighbourCell (i, j) _cells CellTopRight = safeGet (i+1) (j-1) _cells
_neighbourCell (i, j) _cells CellBottomLeft = safeGet (i-1) (j+1) _cells
_neighbourCell (i, j) _cells CellBottomRight = safeGet (i+1) (j+1) _cells

cells :: Int -> CellMatrix
cells 0 = cellMatrix $ \(i,j) ->
    let gliderAt = glider (i,j) in
    if gliderAt (0,0) ||
        gliderAt (3,6) ||
        gliderAt (12,7)
     then Just (Cell i j) else Nothing

cells time = nextGeneration $ cells (time-1)

glider :: (Int, Int) -> (Int, Int) -> Bool
glider (i, j) (dx,dy) =
    i == 2+dx && j == 1+dy ||
    i == 3+dx && j == 2+dy ||
    i == 1+dx && j == 3+dy ||
    i == 2+dx && j == 3+dy ||
    i == 3+dx && j == 3+dy
