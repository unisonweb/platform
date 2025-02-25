Compiling to an executable file with the @-O2@ optimization level is recommended.

For example: @ghc -o 'mandelbrot_iter' -O2 MandelbrotIter.lhs ; ./mandelbrot_iter@

> {-# LANGUAGE PackageImports #-}
> module Main where

> import Control.Arrow hiding (loop)
> import Control.Monad.IO.Class (MonadIO(..))
> import Control.Monad.Trans.Iter
> import "mtl" Control.Monad.Reader (ReaderT, runReaderT, asks)
> import Data.Complex
> import Graphics.HGL (runGraphics, Window, withPen,
>                      line, RGB (RGB), RedrawMode (DoubleBuffered), openWindowEx,
>                      drawInWindow, mkPen, Style (Solid))

Some fractals can be defined by infinite sequences of complex numbers. For example,
to render the <https://en.wikipedia.org/wiki/Mandelbrot_set Mandelbrot set>,
the following sequence is generated for each point @c@ in the complex plane:

@
z₀ = c

z₁ = z₀² + c

z₂ = z₁² + c

…
@

If, after some iterations, |z_i| ≥ 2, the point is not in the set. We
can compute if a point is not in the Mandelbrot set this way:

@
 escaped :: Complex Double -> Int
 escaped c = loop 0 0 where
   loop z n = if (magnitude z) >= 2 then n
                                    else loop (z*z + c) (n+1)
@

If @c@ is not in the Mandelbrot set, we get the number of iterations required to
prove that fact. But, if @c@ is in the mandelbrot set, 'escaped' will
run forever.

We can use the 'Iter' monad to delimit this effect. By applying
'delay' before the recursive call, we decompose the computation into
terminating steps.

> escaped :: Complex Double -> Iter Int
> escaped c = loop 0 0 where
>   loop z n = if (magnitude z) >= 2 then return n
>                                    else delay $ loop (z*z + c) (n+1)
>

If we draw each point on a canvas after it escapes, we can get a _negative_
image of the Mandelbrot set. Drawing pixels is a side-effect, so it
should happen inside the IO monad. Also, we want to have an
environment to store the size of the canvas, and the target window.

By using 'IterT', we can add all these behaviours to our non-terminating
computation.

> data Canvas = Canvas { width :: Int, height :: Int, window :: Window }
>
> type FractalM a = IterT (ReaderT Canvas IO) a

Any simple, non-terminating computation can be lifted into a richer environment.

> escaped' :: Complex Double -> IterT (ReaderT Canvas IO) Int
> escaped' = liftIter . escaped

Then, to draw a point, we can just retrieve the number of iterations until it
finishes, and draw it. The color will depend on the number of iterations.

> mandelbrotPoint :: (Int, Int) -> FractalM ()
> mandelbrotPoint p = do
>   c <- scale p
>   n <- escaped' c
>   let color =  if (even n) then RGB   0   0 255 -- Blue
>                            else RGB   0   0 127 -- Darker blue
>   drawPoint color p

The pixels on the screen don't match the region in the complex plane where the
fractal is; we need to map them first. The region we are interested in is
Im z = [-1,1], Re z = [-2,1].

> scale :: (Int, Int) -> FractalM (Complex Double)
> scale (xi,yi) = do
>   (w,h) <- asks $ (fromIntegral . width) &&& (fromIntegral . height)
>   let (x,y) = (fromIntegral xi, fromIntegral yi)
>   let im = (-y + h / 2     ) / (h/2)
>   let re = ( x - w * 2 / 3 ) / (h/2)
>   return $ re :+ im

Drawing a point is equivalent to drawing a line of length one.

> drawPoint :: RGB -> (Int,Int) -> FractalM ()
> drawPoint color (x,y) = do
>   w <- asks window
>   let point = line (x,y) (x+1, y+1)
>   liftIO $ drawInWindow w $ mkPen Solid 1 color (flip withPen point)

We may want to draw more than one point. However, if we just sequence the computations
monadically, the first point that is not a member of the set will block the whole
process. We need advance all the points at the same pace, by interleaving the
computations.

> drawMandelbrot :: FractalM ()
> drawMandelbrot = do
>   (w,h) <- asks $ width &&& height
>   let ps = [mandelbrotPoint (x,y) | x <- [0 .. (w-1)], y <- [0 .. (h-1)]]
>   interleave_ ps

To run this computation, we can just use @retract@, which will run indefinitely:

> runFractalM :: Canvas -> FractalM a -> IO a
> runFractalM canvas  = flip runReaderT canvas . retract

Or, we can trade non-termination for getting an incomplete result,
by cutting off after a certain number of steps.

> runFractalM' :: Integer -> Canvas -> FractalM a -> IO (Maybe a)
> runFractalM' n canvas  = flip runReaderT canvas . retract . cutoff n

Thanks to the 'IterT' transformer, we can separate timeout concerns from
computational concerns.

> main :: IO ()
> main = do
>   let windowWidth = 800
>   let windowHeight = 480
>   runGraphics $ do
>     w <- openWindowEx "Mandelbrot" Nothing (windowWidth, windowHeight) DoubleBuffered (Just 1)
>     let canvas = Canvas windowWidth windowHeight w
>     _ <- runFractalM' 100 canvas drawMandelbrot
>     putStrLn $ "Fin"

