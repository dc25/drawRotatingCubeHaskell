{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom 
import Data.Map as DM (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl1, foldl, scanl, head)
import Data.Maybe (Maybe(Just))
import Data.Matrix (Matrix, fromLists, toLists, multStd2)
import Data.Monoid ((<>))
import Control.Monad(fmap,return,(>>=),(=<<))
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

data Action = Animate
data Color = Red | Green | Blue | Yellow | Orange | Purple | Black deriving (Show,Eq,Ord,Enum)

xyRotationMatrix :: Float -> Matrix Float
xyRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  s,  0,  0 ]
                 ,[-s,  c,  0,  0 ]
                 ,[ 0,  0,  1,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

yzRotationMatrix :: Float -> Matrix Float
yzRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ 1,  0,  0,  0 ]
                 ,[ 0,  c,  s,  0 ]
                 ,[ 0, -s,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

zxRotationMatrix :: Float -> Matrix Float
zxRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  0,  s,  0 ]
                 ,[ 0,  1,  0,  0 ]
                 ,[-s,  0,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

translationMatrix :: (Float,Float,Float) -> Matrix Float
translationMatrix (x,y,z) =
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  0 ]
               ,[ x,  y,  z,  1 ]
               ]

scaleMatrix :: Float -> Matrix Float
scaleMatrix s =
    fromLists  [[ s,  0,  0,  0 ]
               ,[ 0,  s,  0,  0 ]
               ,[ 0,  0,  s,  0 ]
               ,[ 0,  0,  0,  1 ]
               ]

-- translate to (0,0,1) for perspective viewing
perspectivePrepMatrix :: Matrix Float
perspectivePrepMatrix = translationMatrix (0,0,1)

-- perspective transformation 
perspectiveMatrix :: Matrix Float
perspectiveMatrix = 
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  1 ]
               ,[ 0,  0,  0,  0 ] ]

viewScale = 500

type ViewKitCollection = Map Color (Matrix Float)

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

transformPoints :: Matrix Float -> Matrix Float -> [(Float,Float)]
transformPoints transform points = 
    let result4d = points `multStd2` transform
        result2d = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists result4d
    in result2d

pointsToString :: [(Float,Float)] -> String
pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ") 
showFacetRectangle :: MonadWidget t m => Float -> Float -> Float -> Float -> Color -> Dynamic t (Matrix Float) -> m ()
showFacetRectangle x0 y0 x1 y1 faceColor dFaceViewKit = do
    let points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
    dAttrs <- mapDyn (\fvk -> "fill" =: show faceColor  <> 
                              "points" =: pointsToString (transformPoints fvk points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return ()

showFacetSquare :: MonadWidget t m => Color -> Float -> Dynamic t (Matrix Float) -> m ()
showFacetSquare faceColor margin dFaceViewKit = do
    let x0 = margin
        y0 = margin
        x1 = 1.0 - margin
        y1 = 1.0 - margin
    showFacetRectangle x0 y0 x1 y1 faceColor dFaceViewKit

showFace :: MonadWidget t m => Color -> Dynamic t (Matrix Float) -> m ()
showFace faceColor dFaceViewKit = do  
    showFacetSquare Black 0 dFaceViewKit
    showFacetSquare faceColor 0.05 dFaceViewKit

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
    let cross [x0,y0,z0] [x1,y1,z1] = [y0*z1 - z0*y1
                                      ,z0*x1 - x0*z1
                                      ,x0*y1 - y0*x1 ]

        dot v0 v1 = sum $ zipWith (*) v0 v1

        vMinus = zipWith (-) 

        threeUntransformedPoints = fromLists [ [0,0,0,1]   -- lower left corner of original face
                                             , [1,0,0,1]   -- lower right corner of original face
                                             , [0,1,0,1] ] -- upper left corner of original face

        threeTransformedPoints = toLists $ threeUntransformedPoints `multStd2` modelTransform
        pt00 = take 3 $ head threeTransformedPoints 
        pt10 = take 3 $ threeTransformedPoints !! 1
        pt01 = take 3 $ threeTransformedPoints !! 2

        tVec_10_00 = pt10 `vMinus` pt00  -- vector from lower right to lower left
        tVec_01_00 = pt01 `vMinus` pt00  -- vector from upper left to lower left

        perpendicular = tVec_10_00 `cross` tVec_01_00  -- cross to get perpendicular pointing out from face.
        cameraToPlane = pt00 `vMinus` viewPoint

        -- perpendicular always points out from surface of cube.
        -- camera vector points in to surface of cube.
        -- For face to be visible, camera vector and perpendicular 
        -- should be opposed to each other. 
    in cameraToPlane `dot` perpendicular < 0

viewTransformation :: Matrix Float -> Color -> (Matrix Float, Bool)
viewTransformation orientation faceColor = 
    let trans2d = -1/2  -- translate center of 1x1 square face to origin.
        trans2dMatrix = translationMatrix (trans2d,trans2d,0)

        offsetMatrix = translationMatrix (0,0,1/2)

        -- Rotate face into position .  
        assemblies = fromList
                        [ ( Purple
                          ,  []
                          )  

                        , ( Yellow
                          ,  [ yzRotationMatrix (pi/2) ]
                          )  

                        , ( Red
                          ,  [ yzRotationMatrix (pi/2)
                             , xyRotationMatrix (pi/2) ]
                          )  

                        , ( Green
                          ,  [ yzRotationMatrix (pi/2)
                             , xyRotationMatrix pi ]
                          )  

                        , ( Blue
                          ,  [ yzRotationMatrix (pi/2)
                             , xyRotationMatrix (-pi/2) ]
                          )  

                        , ( Orange
                          , [ yzRotationMatrix pi ]
                          )
                        ]

        Just assembleMatricies = DM.lookup faceColor assemblies 

        -- scale down to fit in camera space
        scale3dMatrix = scaleMatrix (1/2)

        modelTransformations = [ trans2dMatrix 
                               , offsetMatrix
                               ] ++ 
                               assembleMatricies ++ -- may be 0,1 or 2 matricies
                               [ scale3dMatrix,
                                 orientation
                               ]

        -- combine to single transform from 2d to 3d
        modelTransform =  foldl1 multStd2 modelTransformations

        -- backface elimination
        isFacingCamera = facingCamera [0,0,-1] modelTransform
    in (modelTransform, isFacingCamera)

viewKit :: Matrix Float -> Color -> (Bool, Matrix Float)
viewKit orientation faceColor = 
    let (modelTransform, isFacingCamera) 
            = viewTransformation orientation faceColor 

        -- scale up to svg box scale
        viewScaleMatrix = scaleMatrix viewScale

        -- move to center of svg box
        viewTranslationMatrix = translationMatrix (viewScale/2, viewScale/2, 0)

        -- combine to get single transform from 2d face to 2d display
        viewTransform =            modelTransform
                        `multStd2` perspectivePrepMatrix
                        `multStd2` perspectiveMatrix
                        `multStd2` viewScaleMatrix
                        `multStd2` viewTranslationMatrix

    in (isFacingCamera, viewTransform)

kitmapUpdate :: Matrix Float -> ViewKitCollection -> Color -> ViewKitCollection
kitmapUpdate orientation prevMap faceColor = 
    let (isVisible, newViewKit) 
            = viewKit orientation faceColor 
    in  if isVisible 
        then insert faceColor newViewKit prevMap
        else prevMap

topView :: Matrix Float -> ViewKitCollection
topView orientation  =
    foldl (kitmapUpdate orientation ) empty [Red, Green, Blue, Yellow, Orange, Purple]

viewModel :: MonadWidget t m => Dynamic t (Matrix Float) -> m ()
viewModel orientation = do
    topMap <- mapDyn topView orientation
    listWithKey topMap showFace
    return ()

view :: MonadWidget t m => Dynamic t (Matrix Float) -> m (Event t Action)
view orientation = 
    el "div" $ do
        (_,ev) <-    elDynAttrNS' svgNamespace "svg" 
                       (constDyn $  "width" =: show viewScale
                                 <> "height" =: show viewScale
                                 ) $ viewModel orientation
        return never

rotateModel rotationMatrix orientation = 
    orientation `multStd2` rotationMatrix 

update :: Action -> Matrix Float -> Matrix Float
update _ orientation = 
    rotateModel (zxRotationMatrix (-pi/20) ) orientation

main = mainWidget $ do 
    let initialOrientation =             zxRotationMatrix (3*pi/4) 
                              `multStd2` yzRotationMatrix (pi/4)
        dt = 0.1

    tick <- tickLossy dt =<< liftIO getCurrentTime
    let advanceAction = fmap (const Animate) tick
    rec
        view orientation
        orientation <- foldDyn update initialOrientation $ leftmost [advanceAction]
    return ()
