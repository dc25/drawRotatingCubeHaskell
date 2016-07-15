module View (view) where

import Reflex.Dom ( MonadWidget ,Dynamic ,Event ,EventName(Click) ,attachWith ,button ,constDyn ,current ,domEvent ,el ,elAttr ,elDynAttrNS' ,leftmost ,listWithKey ,mapDyn ,switch ,never ,(=:) ,(&))

import Data.Map as DM (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, scanl,head)
import Data.Maybe (Maybe(Just))
import Data.Matrix (Matrix, fromLists, toLists, multStd2)
import Data.Monoid ((<>))
import Control.Monad(sequence,fmap,return,(>>=),(=<<))

import Matrices
import Action
import Color
import Model

viewScale = 500

data FaceViewKit = FaceViewKit { transform :: Matrix Float }

type ViewKitCollection = Map Color FaceViewKit

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

transformPoints :: Matrix Float -> Matrix Float -> [(Float,Float)]
transformPoints transform points = 
    let result4d = points `multStd2` transform
        result2d = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists result4d
    in result2d

pointsToString :: [(Float,Float)] -> String
pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ") 
showFacetRectangle :: MonadWidget t m => Float -> Float -> Float -> Float -> Color -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetRectangle x0 y0 x1 y1 faceColor dFaceViewKit = do
    let points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
    dAttrs <- mapDyn (\fvk -> "fill" =: show faceColor  <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return dFaceViewKit

showFacetSquare :: MonadWidget t m => Int -> Int -> Color -> Float -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetSquare x y faceColor margin dFaceViewKit = do
    let x0 = fromIntegral x + margin
        y0 = fromIntegral y + margin
        x1 = x0 + 1 - 2 * margin
        y1 = y0 + 1 - 2 * margin
    showFacetRectangle x0 y0 x1 y1 faceColor dFaceViewKit

showFace :: MonadWidget t m => Color -> Dynamic t FaceViewKit -> m (Event t Action)
showFace faceColor dFaceViewKit = do  
    showFacetSquare 0 0 Black 0 dFaceViewKit
    showFacetSquare 0 0 faceColor 0.05 dFaceViewKit
    return never

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

viewTransformation :: Model -> Color -> (Matrix Float, Bool)
viewTransformation model@(Model orientation ) faceColor = 
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
        modelTransform =  foldl multStd2 identityMatrix modelTransformations

        -- backface elimination
        isFacingCamera = facingCamera [0,0,-1] modelTransform
    in (modelTransform, isFacingCamera)

viewKit :: Model -> Color -> (Bool, FaceViewKit)
viewKit model@(Model orientation ) faceColor = 
    let (modelTransform, isFacingCamera) 
            = viewTransformation model faceColor 

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

    in (isFacingCamera, FaceViewKit viewTransform)

kitmapUpdate :: Model -> ViewKitCollection -> Color -> ViewKitCollection
kitmapUpdate model prevMap faceColor = 
    let (isVisible, newViewKit) 
            = viewKit model faceColor 
    in  if isVisible 
        then insert faceColor newViewKit prevMap
        else prevMap

topView :: Model -> ViewKitCollection
topView model  =
    foldl (kitmapUpdate model ) empty [Red, Green, Blue, Yellow, Orange, Purple]

viewModel :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
viewModel model = do
    topMap <- mapDyn topView model

    listWithKey topMap showFace

    return never

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = 
    el "div" $ do
        (_,ev) <-    elDynAttrNS' svgNamespace "svg" 
                       (constDyn $  "width" =: show viewScale
                                 <> "height" =: show viewScale
                                 ) $ viewModel model
        return never

