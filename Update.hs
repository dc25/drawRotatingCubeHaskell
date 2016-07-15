module Update(update) where

import Data.Matrix (Matrix,multStd2)

import Matrices
import Action
import Model
import View

applyRotation :: Matrix Float -> Matrix Float -> Matrix Float
applyRotation rotationMatrix  prevOrientation = 
    prevOrientation `multStd2` rotationMatrix

rotateModel rotationMatrix model = 
    model { orientation = applyRotation rotationMatrix $ orientation model }

-- | FRP style update function. Given action and model, return updated model.
update :: Action -> Model -> Model
update action model = 
    case action of
        Animate -> 
            let step = pi/20
            in rotateModel (zxRotationMatrix (-pi/20) ) model
 
