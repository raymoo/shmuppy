module Game.Player (Player(..), PlayerControl(..), player, playerShots) where

import Control.FRPNow
import Control.FRPNow.Gloss

import Game.Bullet
import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative ((<$>), (<*>))

import Data.Monoid


pBulletSize :: Float
pBulletSize = 20


pBulletV :: Vector Float
pBulletV = (0, 3000)


pBOffset :: Vector Float
pBOffset = (0, 30)


playerSpeed :: Float
playerSpeed = 600


-- | Square Root of 2, used in diagonal movement
sq2 :: Float
sq2 = sqrt 2


data Player =
  Player { _playerPos :: Vector Float
         , _playerShooting :: Bool
         }
  deriving (Show, Eq)


data PlayerControl = CUp
                   | CDown
                   | CLeft
                   | CRight
                   | CFocus
                   | CShoot
                   deriving (Show, Eq, Ord)


-- | Turns a set of controls and a top speed into a velocity
controlToVel :: Float -> Set PlayerControl -> Vector Float
controlToVel speed set
  | dydt == 0 = (speed * fromIntegral dxdt, 0)
  | dxdt == 0 = (0, speed * fromIntegral dydt)
  | otherwise = (diagSpeed * fromIntegral dxdt, diagSpeed * fromIntegral dydt)
  where diagSpeed = speed / sq2
        isUp = CUp `S.member` set
        isDown = CDown `S.member` set
        dydt :: Int
        dydt
          | isUp && isDown = 0
          | isUp = 1
          | isDown = -1
          | otherwise = 0
        isRight = CRight `S.member` set
        isLeft = CLeft `S.member` set
        dxdt :: Int
        dxdt
          | isRight && isLeft = 0
          | isRight = 1
          | isLeft = -1
          | otherwise = 0


-- | Takes a time behavior, controls behavior, and creates a player behavior.
player :: Behavior Time -> Behavior (Set PlayerControl) -> Vector Float
          -> Behavior (Behavior Player)
player bt bcs pos = (<*> bShot) . fmap Player <$> bbPos
  where bv = controlToVel playerSpeed <$> bcs
        bbPos = fmap (pos ^+^) <$> integrate bt bv
        bShot = (CShoot `S.member`) <$> bcs


-- | Player shots
playerShots :: Behavior Time -> Behavior Player -> EvStream (Behavior Bullet)
playerShots bt bp = snapshots (bp >>= playerShoot bt) (shotTimes <> omake)
  where goodTime = (< 0.5) . snd . properFraction . (*10) <$> bt
        shotTimes = edges $ (&&) . _playerShooting <$> bp <*> goodTime
        omake = edges $ _playerShooting <$> bp


playerShoot :: Behavior Time -> Player -> Behavior (Behavior Bullet)
playerShoot bt (Player pos _) = straightBullet bt pBulletV (Bullet (pos ^+^ pBOffset) pBulletSize)
