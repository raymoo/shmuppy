module Game (game, drawGame) where

import Game.Player
import Game.Bullet


import Control.FRPNow
import Control.FRPNow.Gloss

import Graphics.Gloss.Interface.Pure.Game hiding (Vector)

import Control.Applicative((<$>), (<*>), pure, liftA2)

import Control.Monad (filterM, sequence, join)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe


data Game =
  Game { _gamePlayer :: Player
       , _gameBullets :: [Bullet]
       }
  deriving (Show, Eq)


gameW :: Float
gameW = 600

gameH :: Float
gameH = 600

initPos :: Vector Float
initPos = (0,0)


-- | Takes a time behavior, an event stream of new bullets, and gives a behavior
-- for a list of bullets. Also takes an initial list of bullet behaviors.
{- old version
bullets :: Behavior Time -> EvStream (Behavior Bullet) -> Behavior (Behavior [Bullet])
bullets bt evbb = foldBs (pure []) addBullet evbb
  where addBullet bbullets newB = (:) <$> newB <*> bbullets
-}


bullets :: Behavior Time -> EvStream (Behavior Bullet) -> Behavior (Behavior [Bullet])
bullets bt evbb = join . fmap sequence <$> bulletBs bt evbb


inRectangle :: (Float, Float) -> (Float, Float) -> Bullet -> Bool
inRectangle (l,b) (r,t) (Bullet (x,y) rad) =
  x + rad >= l && x - rad <= r && y + rad >= b && y - rad <= t


bulletBs :: Behavior Time -> EvStream (Behavior Bullet) -> Behavior (Behavior [Behavior Bullet])
bulletBs bt evbb = foldBs (pure []) addBullet evbb
  where addBullet bbbullets newB = do
          (newB :) <$> bbbullets >>= clearGarbage


-- | Remove out of camera bullets
clearGarbage :: [Behavior Bullet] -> Behavior [Behavior Bullet]
clearGarbage bbs = filterM (inRectangle (screenLeft, screenBottom) (screenRight, screenTop) <$>) bbs
  where screenLeft = -gameW/2
        screenRight = gameW/2
        screenTop = gameH/2
        screenBottom = -gameH/2

game :: Behavior Time -> EvStream GEvent -> Behavior (Behavior Game)
game bt evStr = do
  newPlayer <- mkPlayer
  bulletB <- bullets bt (playerShots bt newPlayer)
  pure $ Game <$> newPlayer <*> bulletB
  where controls :: Behavior (Behavior (Set PlayerControl))
        controls = (mkControls <$>) <$> toKeysDown evStr
        mkPlayer :: Behavior (Behavior Player)
        mkPlayer = do
          bCont <- controls
          player bt bCont initPos


mkControls :: Set Key -> Set PlayerControl
mkControls set = S.fromList $
                 mapMaybe (\(k,v) -> if k `S.member` set then Just v else Nothing) inputMap
  where inputMap = [ (SpecialKey KeyUp, CUp)
                   , (SpecialKey KeyDown, CDown)
                   , (SpecialKey KeyLeft, CLeft)
                   , (SpecialKey KeyRight, CRight)
                   , (SpecialKey KeyShiftL, CFocus)
                   , (Char 'z', CShoot)
                   ] 


drawGame :: Game -> Picture
drawGame (Game p bs) = pictures (drawPlayer p : map drawBullet bs)


drawPlayer :: Player -> Picture
drawPlayer (Player (x,y) _) = translate x y . color yellow $ circleSolid 10


drawBullet :: Bullet -> Picture
drawBullet (Bullet (x,y) r) = translate x y . color red $ circleSolid r
