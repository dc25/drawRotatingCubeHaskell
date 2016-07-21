{-# LANGUAGE RecursiveDo #-} 
import Reflex.Dom 
import Data.Map as DM (Map, lookup, insert, empty)
import Data.Matrix (Matrix, fromLists, toLists, multStd2)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

viewScale = 500
updateFrequency = 0.2
rotationStep = pi/10

data Color = Red | Green | Blue | Yellow | Orange | Purple | Black deriving (Show,Eq,Ord,Enum)

zRot :: Float -> Matrix Float
zRot rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  s,  0,  0 ]
                 ,[-s,  c,  0,  0 ]
                 ,[ 0,  0,  1,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

xRot :: Float -> Matrix Float
xRot rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ 1,  0,  0,  0 ]
                 ,[ 0,  c,  s,  0 ]
                 ,[ 0, -s,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

yRot :: Float -> Matrix Float
yRot rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  0, -s,  0 ]
                 ,[ 0,  1,  0,  0 ]
                 ,[ s,  0,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

translation :: (Float,Float,Float) -> Matrix Float
translation (x,y,z) =
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  0 ]
               ,[ x,  y,  z,  1 ]
               ]

scale :: Float -> Matrix Float
scale s =
    fromLists  [[ s,  0,  0,  0 ]
               ,[ 0,  s,  0,  0 ]
               ,[ 0,  0,  s,  0 ]
               ,[ 0,  0,  0,  1 ]
               ]

-- translate to (0,0,1) for perspective viewing
perspectivePrep :: Matrix Float
perspectivePrep = translation (0,0,1)

-- perspective transformation; 
perspective :: Matrix Float
perspective = 
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  1 ]
               ,[ 0,  0,  0,  0 ] ]

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

transformPoints :: Matrix Float -> Matrix Float -> [(Float,Float)]
transformPoints transform points = 
    let result4d = points `multStd2` transform
        result2d = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists result4d
    in result2d

showRectangle :: MonadWidget t m => Float -> Float -> Float -> Float -> Color -> Dynamic t (Matrix Float) -> m ()
showRectangle x0 y0 x1 y1 faceColor dFaceView = do
    let points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
        pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ") 
    dAttrs <- mapDyn (\fvk -> "fill" =: show faceColor  <> 
                              "points" =: pointsToString (transformPoints fvk points))  dFaceView
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return ()

showUnitSquare :: MonadWidget t m => Color -> Float -> Dynamic t (Matrix Float) -> m ()
showUnitSquare faceColor margin dFaceView = 
    showRectangle margin margin (1.0 - margin) (1.0 - margin) faceColor dFaceView

-- show colored square on top of black square for outline effect
showFace :: MonadWidget t m => Color -> Dynamic t (Matrix Float) -> m ()
showFace faceColor dFaceView = do  
    showUnitSquare Black 0 dFaceView
    showUnitSquare faceColor 0.03 dFaceView

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
    let cross [x0,y0,z0] [x1,y1,z1] = [y0*z1 - z0*y1
                                      ,z0*x1 - x0*z1
                                      ,x0*y1 - y0*x1 ]

        dot v0 v1 = sum $ zipWith (*) v0 v1

        vMinus = zipWith (-) 

        threeUntransformedPoints = fromLists [ [0,0,0,1]   -- lower left 
                                             , [1,0,0,1]   -- lower right 
                                             , [0,1,0,1] ] -- upper left 

        threeTransformedPoints = toLists $ threeUntransformedPoints `multStd2` modelTransform
        pt00 = take 3 $ head threeTransformedPoints 
        pt10 = take 3 $ threeTransformedPoints !! 1
        pt01 = take 3 $ threeTransformedPoints !! 2

        -- vector from lower right to lower left
        tVec_10_00 = pt10 `vMinus` pt00  

        -- vector from upper left to lower left
        tVec_01_00 = pt01 `vMinus` pt00  

        -- cross to get perpendicular pointing out from face.
        perpendicular = tVec_10_00 `cross` tVec_01_00  
        cameraToPlane = pt00 `vMinus` viewPoint

        -- Perpendicular points away from surface;
        -- Camera vector points towards surface
        -- Opposed vectors means that face will be visible.
    in cameraToPlane `dot` perpendicular < 0

faceView :: Matrix Float -> Matrix Float -> (Bool, Matrix Float)
faceView modelOrientation faceOrientation = 
    let modelTransform =            translation (-1/2,-1/2,1/2) -- unit square to origin + z offset
                         `multStd2` faceOrientation             -- orientation specific to each face
                         `multStd2` scale (1/2)                 -- shrink cube to fit in view.
                         `multStd2` modelOrientation            -- position the entire cube

        -- backface elimination
        isFacingCamera = facingCamera [0,0,-1] modelTransform
        
        -- scale up to svg box scale
        viewScaleMatrix = scale viewScale

        -- move to center of svg box
        viewTranslation = translation (viewScale/2, viewScale/2, 0)

        -- combine to get single transform from 2d face to 2d display
        viewTransform =            modelTransform
                        `multStd2` perspectivePrep
                        `multStd2` perspective
                        `multStd2` viewScaleMatrix
                        `multStd2` viewTranslation

    in (isFacingCamera, viewTransform)

updateFaceViews :: Matrix Float -> Map Color (Matrix Float) -> (Color, Matrix Float) -> Map Color (Matrix Float)
updateFaceViews modelOrientation prevCollection (faceColor, faceOrientation) = 
    let (isVisible, newFaceView) = faceView modelOrientation faceOrientation
    in  if isVisible 
        then insert faceColor newFaceView prevCollection
        else prevCollection

faceViews :: Matrix Float -> Map Color (Matrix Float)
faceViews modelOrientation  =
    foldl (updateFaceViews modelOrientation) empty 
          [ (Purple , xRot (0.0) )  
          , (Yellow , xRot (pi/2) )  
          , (Red    , yRot (pi/2) )  
          , (Green  , xRot (-pi/2) )  
          , (Blue   , yRot (-pi/2) )  
          , (Orange , xRot (pi) )
          ]

viewModel :: MonadWidget t m => Dynamic t (Matrix Float) -> m ()
viewModel modelOrientation = do
    faceMap <- mapDyn faceViews modelOrientation
    listWithKey faceMap showFace
    return ()

view :: MonadWidget t m => Dynamic t (Matrix Float) -> m ()
view modelOrientation = do
    el "h1" $ text "Rotating Cube"
    (_,_) <- elDynAttrNS' svgNamespace "svg" 
               (constDyn $  "width" =: show viewScale
                         <> "height" =: show viewScale
               ) $ viewModel modelOrientation
    return ()

main = mainWidget $ do 
    let initialOrientation = xRot (pi/4) `multStd2` zRot (atan(1/sqrt(2)))

        update _ modelOrientation = 
            modelOrientation `multStd2` (yRot (rotationStep) ) 

    tick <- tickLossy  updateFrequency =<< liftIO getCurrentTime
    rec
        view modelOrientation
        modelOrientation <- foldDyn update initialOrientation tick
    return ()
