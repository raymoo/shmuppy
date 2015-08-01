module Game.Bullet (Bullet(..), velBullet, straightBullet, Vector(..)) where

import Control.FRPNow
import Control.FRPNow.Gloss

import Control.Applicative ((<$>), (<*>), pure)


type Vector a = (a, a)


-- | Bullets
data Bullet =
  Bullet { _bulletPos :: Vector Float
         , _bulletR :: Float -- ^ Radius
         }
  deriving (Show, Eq)



-- | Takes a time behavior and a velocity behavior (in units per second) and produces
-- a bullet that travels with that velocity (Euler integration).
velBullet :: Behavior Time -> Behavior (Vector Float) -> Bullet
                  -> Behavior (Behavior Bullet)
velBullet bt bv (Bullet pos r) = remakeBullet <$> integrate bt bv
  where remakeBullet bPos = Bullet . (^+^ pos) <$> bPos <*> pure r


-- | Similar to 'velBullet', but with constant velocity.
straightBullet :: Behavior Time -> Vector Float -> Bullet -> Behavior (Behavior Bullet)
straightBullet bt v = velBullet bt (pure v)
