import Prelude hiding (Either(..))
import Control.Applicative
import FRP.Elerea.Simple hiding (delay)
import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data Vector a = Vector a a

instance Functor Vector where
    fmap f (Vector x y) = Vector (f x) (f y)

instance Applicative Vector where
    pure a = Vector a a
    (Vector fx fy) <*> (Vector x y) = Vector (fx x) (fy y)

instance Num a => Num (Vector a) where
    a + b       = (+) <$> a <*> b
    a - b       = (-) <$> a <*> b
    a * b       = (*) <$> a <*> b
    negate      = fmap negate
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure.fromInteger

instance Fractional a => Fractional (Vector a) where
    a / b        = (/) <$> a <*> b
    recip        = fmap recip
    fromRational = pure.fromRational

toPair :: Vector a -> (a, a)
toPair (Vector x y) = (x,y)

vector :: (a, a) -> Vector a
vector (x,y) = (Vector x y)


-- | x * (abs x)
signedSquare :: (Floating a) => a -> a
signedSquare = (*) <*> abs

-- | (x / abs x) * sqrt (abs x)
absSqrt :: (Floating a, Ord a) => a -> a
absSqrt a
    | a >= 0 = sqrt a
    | otherwise = negate.sqrt $ negate a

-----

data MassState = MassState {
    mass :: Double,
    pos :: Vector Double,
    speed :: Vector Double
}

accelerate :: Vector Double -> Time -> MassState -> MassState
accelerate force t (MassState m p s) = MassState m p' s'
   where a  = force / (pure m)
         s' = s + (pure t * a)
         p' = p + pure t * s'

dragCoef :: Vector Double
dragCoef = Vector (-1) (-0.6)



step :: (Time, (Int, Int)) -> MassState -> MassState
step (dt, acc) state = accelerate force dt state
    where push = fmap fromIntegral $ vector acc
          --drag = dragCoef * (fmap absSqrt $ speed state)
          --drag = dragCoef * (speed state)
          drag = dragCoef * (fmap signedSquare $ speed state)
          force = push + drag

chipForm :: Form
chipForm = filled white $ polygon $ path [(0,-30), (15,15), (0,5), (-15,15)]

render :: (Int, Int) -> MassState -> Element
render (w, h) state =
    centeredCollage w h [move (toPair $ pos state) chipForm]

input :: SignalGen (Signal (Time, (Int, Int)))
input = (,) <~ delta' ~~ Keyboard.arrows
  where delta' = delay $ fps 60

main :: IO ()
main = run defaultConfig $ render <~ Window.dimensions ~~ stepper
    where stepper = foldp step state input
          state = MassState 100 (Vector 0 0) (Vector 0 0)
