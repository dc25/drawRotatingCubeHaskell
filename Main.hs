{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom (mainWidget,tickLossy,foldDyn,leftmost)
import Data.Matrix (multStd2)
import Control.Monad(fmap,return)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

import Matrices
import View
import Action
import Model
import Update

main = mainWidget $ do 
    let initialOrientation =             identityMatrix 
                              `multStd2` zxRotationMatrix (3*pi/4) 
                              `multStd2` yzRotationMatrix (pi/4)
        dt = 0.1

    now <- liftIO getCurrentTime
    tick <- tickLossy dt now
    let advanceAction = fmap (const Animate) tick
    rec
        view model
        model <- foldDyn update (Model initialOrientation ) $ leftmost [advanceAction]
    return ()
