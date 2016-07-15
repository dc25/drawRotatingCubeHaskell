module Action (Action(NudgeCube,Animate)) where

import Cube
import Rotation
import Direction

data Action = NudgeCube Direction | Animate
