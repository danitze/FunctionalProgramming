module Main where

import Graphics.UI.GLUT
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)

data Ball = Ball { position :: (GLfloat, GLfloat), velocity :: (GLfloat, GLfloat), radius :: GLfloat }

ball1 :: IORef Ball
ball1 = unsafePerformIO $ newIORef $ Ball (100, 100) (1, 1) 10

ball2 :: IORef Ball
ball2 = unsafePerformIO $ newIORef $ Ball (200, 200) (-1, -1) 10

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Ball Collision"
  displayCallback $= display
  idleCallback $= Just update
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  ballA <- get ball1
  ballB <- get ball2
  renderBall ballA
  renderBall ballB
  flush

renderBall :: Ball -> IO ()
renderBall (Ball (posX, posY) _ r) = preservingMatrix $ do
  translate $ Vector3 posX posY 0
  renderPrimitive Polygon $ mapM_ (\(ptX, ptY) -> vertex $ Vertex2 ptX ptY) (circlePoints r 10)

circlePoints :: GLfloat -> Int -> [(GLfloat, GLfloat)]
circlePoints r n = [ (r * cos (2 * pi * fromIntegral i / fromIntegral n), r * sin (2 * pi * fromIntegral i / fromIntegral n)) | i <- [0..n] ]

update :: IdleCallback
update = do
  modifyIORef ball1 moveBall
  modifyIORef ball2 moveBall
  checkCollision ball1 ball2
  postRedisplay Nothing

moveBall :: Ball -> Ball
moveBall (Ball (x, y) (vx, vy) r) = Ball (x + vx, y + vy) (vx, vy) r

-- Check and Handle Collision
checkCollision :: IORef Ball -> IORef Ball -> IO ()
checkCollision ballRef1 ballRef2 = do
  ballA <- get ballRef1
  ballB <- get ballRef2
  let Ball (x1, y1) _ r1 = ballA
      Ball (x2, y2) _ r2 = ballB
      dx = x1 - x2
      dy = y1 - y2
      dist = sqrt(dx*dx + dy*dy)
  when (dist < r1 + r2) $ do
    -- Adjust velocities for a simple elastic collision
    let Ball _ (vx1, vy1) _ = ballA
        Ball _ (vx2, vy2) _ = ballB
        newVx1 = vx2
        newVy1 = vy2
        newVx2 = vx1
        newVy2 = vy1
    writeIORef ballRef1 $ Ball (x1, y1) (newVx1, newVy1) r1
    writeIORef ballRef2 $ Ball (x2, y2) (newVx2, newVy2) r2
