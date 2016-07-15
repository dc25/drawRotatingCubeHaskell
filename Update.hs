module Update(update) where

import Data.Matrix (Matrix,multStd2)

import Matrices
import Action
import Direction as D
import Rotation
import Model
import Cube
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
        NudgeCube direction -> 
            let step = pi/20
            in case direction of
                   D.Left ->  rotateModel (zxRotationMatrix (-step) ) model
                   D.Right -> rotateModel (zxRotationMatrix   step  ) model
                   D.Up ->    rotateModel (yzRotationMatrix (-step) ) model
                   D.Down ->  rotateModel (yzRotationMatrix   step  ) model
 
