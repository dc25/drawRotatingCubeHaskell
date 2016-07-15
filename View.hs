module View (view, insideFacesCamera) where

import Reflex.Dom ( MonadWidget ,Dynamic ,Event ,EventName(Click) ,attachWith ,button ,constDyn ,current ,domEvent ,el ,elAttr ,elDynAttrNS' ,leftmost ,listWithKey ,mapDyn ,switch ,never ,(=:) ,(&))

import Data.Map as DM (Map, lookup, insert, empty, fromList, elems)
import Data.List (foldl, scanl,head)
import Data.Maybe (Maybe(Just))
import Data.Matrix (Matrix, fromLists, toLists, multStd2)
import Data.Monoid ((<>))
import Control.Monad(sequence,fmap,return,(>>=),(=<<))

import Matrices
import Rotation
import Direction as D
import Action
import Color
import Cube
import Model

viewScale = 500

data FaceViewKit = FaceViewKit { face :: Facet
                               , transform :: Matrix Float
                               }

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

showFacetRectangle :: MonadWidget t m => Float -> Float -> Float -> Float -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetRectangle x0 y0 x1 y1 dFaceViewKit = do
    let points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
    dAttrs <- mapDyn (\fvk -> "fill" =: (show.color.face) fvk  <> 
                              "points" =: pointsToString (transformPoints (transform fvk) points))  dFaceViewKit
    (el,_) <- elDynAttrNS' svgNamespace "polygon" dAttrs $ return ()
    return dFaceViewKit

showFacetSquare :: MonadWidget t m => Int -> Int -> Float -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacetSquare x y margin dFaceViewKit = do
    let x0 = fromIntegral x + margin
        y0 = fromIntegral y + margin
        x1 = x0 + 1 - 2 * margin
        y1 = y0 + 1 - 2 * margin
    showFacetRectangle x0 y0 x1 y1 dFaceViewKit

changeViewKitColor :: Color -> FaceViewKit -> FaceViewKit
changeViewKitColor newColor prevViewKit = 
    let prevFace = face prevViewKit
        newFace = prevFace {color=newColor}
    in prevViewKit {face = newFace}

showFacet :: MonadWidget t m => Int -> Int -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showFacet x y dFaceViewKit = do
    showFacetSquare x y 0 =<< mapDyn (changeViewKitColor Black) dFaceViewKit 
    showFacetSquare x y 0.05 dFaceViewKit
    return dFaceViewKit

advance :: MonadWidget t m => (Facet -> Facet) -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
advance adv dFaceViewKit = do
    let updateViewKit advancer prevViewKit = prevViewKit { face = advancer $ face prevViewKit }
    mapDyn (updateViewKit adv) dFaceViewKit

showAndAdvance :: MonadWidget t m => Int -> Int -> (Facet -> Facet) -> Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showAndAdvance x y adv dFaceViewKit = do
    showFacet x y dFaceViewKit
    advance adv dFaceViewKit

showFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showFace lowerLeft = do  
    left <-       showAndAdvance 0 0 east lowerLeft   -- lower left
    upperLeft <-  showAndAdvance 0 1 east left        -- left
    upper <-      showAndAdvance 0 2 east upperLeft   -- upper left
    upperRight <- showAndAdvance 1 2 east upper       -- upper
    right <-      showAndAdvance 2 2 east upperRight  -- upper right
    lowerRight <- showAndAdvance 2 1 east right       -- right
    lower <-      showAndAdvance 2 0 east lowerRight  -- lower right
    center <-     showAndAdvance 1 0 south lower      -- lower
    _ <-          showFacet      1 1       center          

    return never

showInside :: MonadWidget t m => Dynamic t FaceViewKit -> m (Dynamic t FaceViewKit)
showInside dFaceViewKit = showFacetRectangle 0 0 3 3 =<< mapDyn (changeViewKitColor Maroon) dFaceViewKit 

showUpperMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showUpperMiddleFace upperRight = do  
    upper <-       showAndAdvance 2 2 south upperRight  
    upperLeft <-  showAndAdvance 1 2 west upper      
                    >>= showFacet 0 2                 

    return $ never


showMiddleMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showMiddleMiddleFace upperRight = do  
    right <-      advance east upperRight 
    _     <-      showAndAdvance 2 1 south right 
              >>= showFacet 1 1            

    left <-       advance (south.west.south) upperRight
              >>= showFacet 0 1            

    return never

showLowerMiddleFace :: MonadWidget t m => Dynamic t FaceViewKit -> m (Event t Action)
showLowerMiddleFace lowerLeft = do  
    lower <-       showAndAdvance 0 0 south lowerLeft  
    lowerRight <-  showAndAdvance 1 0 west lower      
                    >>= showFacet 2 0                 

    return never

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
    let cross [x0,y0,z0] [x1,y1,z1] = [y0*z1 - z0*y1
                                      ,z0*x1 - x0*z1
                                      ,x0*y1 - y0*x1 ]

        dot v0 v1 = sum $ zipWith (*) v0 v1

        vMinus = zipWith (-) 

        threeUntransformedPoints = fromLists [ [0,0,0,1]   -- lower left corner of original face
                                             , [3,0,0,1]   -- lower right corner of original face
                                             , [0,3,0,1] ] -- upper left corner of original face

        threeTransformedPoints = toLists $ threeUntransformedPoints `multStd2` modelTransform
        pt00 = take 3 $ head threeTransformedPoints 
        pt30 = take 3 $ threeTransformedPoints !! 1
        pt03 = take 3 $ threeTransformedPoints !! 2

        tVec_30_00 = pt30 `vMinus` pt00  -- vector from lower right to lower left
        tVec_03_00 = pt03 `vMinus` pt00  -- vector from upper left to lower left

        perpendicular = tVec_30_00 `cross` tVec_03_00  -- cross to get perpendicular pointing out from face.
        cameraToPlane = pt00 `vMinus` viewPoint

        -- perpendicular always points out from surface of cube.
        -- camera vector points in to surface of cube.
        -- For face to be visible, camera vector and perpendicular 
        -- should be opposed to each other. 
    in cameraToPlane `dot` perpendicular < 0

viewTransformation :: Model -> Facet -> Float -> (Matrix Float, Bool)
viewTransformation model@(Model topFace orientation ) viewCenterFacet offset = 
    let faceColor = color viewCenterFacet 
        topColor = color topFace

        scale2dMatrix = scaleMatrix (1/3) -- scale from 3x3 square face to 1x1 square face.
        trans2d = -1/2  -- translate center of 1x1 square face to origin.
        trans2dMatrix = translationMatrix (trans2d,trans2d,0)


        -- If face A is the face being rendered and face B is the face that
        -- cooresponds to face A if the top face were Purple, then this map
        -- contains the number of turns to get the frame for face A
        -- to match the frame for face B.
        -- This is necessary because lowerLeft is determined "as if" the
        -- Purple face is at the top of the model ( as it initially is ).
        
        turns = fromList [( (Purple, Yellow), 0)
                         ,( (Purple, Red),    0)
                         ,( (Purple, Purple), 0)
                         ,( (Purple, Green),  0)
                         ,( (Purple, Blue),   0)
                         ,( (Purple, Orange), 0)

                         ,( (Blue,   Blue),   0)
                         ,( (Green,  Green),  0)
                         ,( (Red,    Red),    0)
                         ,( (Yellow, Yellow), 0)
                         ,( (Orange, Orange), 0)

                         ,( (Blue,   Yellow), 1)
                         ,( (Green,  Blue),   1)
                         ,( (Red,    Green),  1)
                         ,( (Yellow, Red),    1)

                         ,( (Blue,   Red),    2)
                         ,( (Green,  Yellow), 2)
                         ,( (Red,    Blue),   2)
                         ,( (Yellow, Green),  2)

                         ,( (Blue,   Green),  3)
                         ,( (Green,  Red),    3)
                         ,( (Red,    Yellow), 3)
                         ,( (Yellow, Blue),   3)

                         ,( (Green,  Purple), 0)
                         ,( (Blue,   Purple), 1)
                         ,( (Yellow, Purple), 2)
                         ,( (Red,    Purple), 3)

                         ,( (Yellow, Orange), 0)
                         ,( (Blue,   Orange), 1)
                         ,( (Green,  Orange), 2)
                         ,( (Red,    Orange), 3) 

                         ,( (Orange, Yellow), 2)
                         ,( (Orange, Red),    2)
                         ,( (Orange, Green),  2)
                         ,( (Orange, Blue),   2) 
                         
                         ,( (Orange, Purple), 0) ]

        Just turnCount = DM.lookup (topColor, faceColor) turns 
        turnMatrix = xyRotationMatrix (fromIntegral turnCount * pi / 2)
        offsetMatrix = translationMatrix (0,0,offset)

        -- Rotate face into position .  Rotations and inverses specified.
        assemblies = fromList
                        [ ( Purple
                          , (  []
                            ,  []
                            )
                          )  

                        , ( Yellow
                          , ([ yzRotationMatrix (pi/2) ]
                            ,[ yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Red
                          , ([ yzRotationMatrix (pi/2)
                             , xyRotationMatrix (pi/2) ]
                            ,[ xyRotationMatrix (-pi/2)
                             , yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Green
                          , ([ yzRotationMatrix (pi/2)
                             , xyRotationMatrix pi ]
                            ,[ xyRotationMatrix (-pi)
                             , yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Blue
                          , ([ yzRotationMatrix (pi/2)
                             , xyRotationMatrix (-pi/2) ]
                            ,[ xyRotationMatrix (pi/2)
                             , yzRotationMatrix (-pi/2) ]
                            )
                          )  

                        , ( Orange
                          , ([ yzRotationMatrix pi ]
                            ,[ yzRotationMatrix (-pi) ]
                            )
                          )
                        ]

        Just (assembleMatricies,_) = DM.lookup faceColor assemblies 
        Just (postTwist,preTwist) = DM.lookup topColor assemblies 

        -- scale down to fit in camera space
        scale3dMatrix = scaleMatrix (1/2)

        modelTransformations = [ scale2dMatrix
                               , trans2dMatrix 
                               , turnMatrix 
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

insideFacesCamera :: Model -> Facet -> Bool
insideFacesCamera model facet = 
    let (_,isFacingCamera) = viewTransformation model facet (1.0/6.0)
    in isFacingCamera

viewKit :: Model -> Facet -> Float -> (Bool, FaceViewKit)
viewKit model@(Model topFace orientation ) viewFacet offset = 
    let viewCenterFacet = (south.south) viewFacet 
        (modelTransform, isFacingCamera) 
            = viewTransformation model viewCenterFacet offset

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

    in (isFacingCamera, FaceViewKit viewFacet viewTransform)

kitmapUpdate :: Model -> Float -> ViewKitCollection -> Facet -> ViewKitCollection
kitmapUpdate model offset prevMap lowerLeft = 
    let (isVisible, newViewKit) 
            = viewKit model lowerLeft offset
    in  if isVisible 
        then insert ((color.south.south) lowerLeft) newViewKit prevMap
        else prevMap

topView :: Model -> ViewKitCollection
topView model@(Model center _ )  =
    foldl (kitmapUpdate model (1.0/2.0)) empty [getLowerLeft center]

bottomView :: Model -> ViewKitCollection
bottomView model@(Model center _ )  =
    foldl (kitmapUpdate model  0.5) empty [(west.south.west.west.south.west.getLowerLeft) center]

upperRights :: Model -> [Facet]
upperRights model@(Model center _ )   =
    let upperRight = (north.getLowerLeft) center
        advancers = [ west.west.south
                    , west.west.south
                    , west.west.south
                    ]
    in scanl (&) upperRight advancers  -- get upper right corners of all faces

lowerLefts :: Model -> [Facet]
lowerLefts model@(Model center _ )   =
    let lowerLeft = (west.south.west.getLowerLeft) center
        advancers = [ west.west.south
                    , west.west.south
                    , west.west.south
                    ]
    in scanl (&) lowerLeft advancers  -- get lower left corners of all faces

upperMiddleView :: Model -> ViewKitCollection
upperMiddleView model@(Model center _ )   =
    foldl (kitmapUpdate model 0.5) empty $ upperRights model

middleMiddleView :: Model -> ViewKitCollection
middleMiddleView model@(Model center _ )   =
    foldl (kitmapUpdate model 0.5) empty $ upperRights model

lowerMiddleView :: Model -> ViewKitCollection
lowerMiddleView model@(Model center _ )  =
    foldl (kitmapUpdate model  0.5) empty $ lowerLefts model

getLowerLeft :: Facet -> Facet
getLowerLeft centerFace =
    let centerFaceColor = color centerFace
        westFaceColor = (color.south.north.west) centerFace

        leftDirs = fromList [ ((Purple,  Blue),   west)
                            , ((Purple,  Yellow), north)
                            , ((Purple,  Red),    east)
                            , ((Purple,  Green),  south)
 
                            , ((Yellow,  Blue),   west)
                            , ((Yellow,  Orange), north)
                            , ((Yellow,  Red ),   east)
                            , ((Yellow,  Purple), south)
 
                            , ((Red,     Yellow), west)
                            , ((Red,     Orange), north)
                            , ((Red,     Green ), east)
                            , ((Red,     Purple), south)
 
                            , ((Green,   Red),    west)
                            , ((Green,   Orange), north)
                            , ((Green,   Blue ),  east)
                            , ((Green,   Purple), south)
 
                            , ((Blue,    Green),  west)
                            , ((Blue,    Orange), north)
                            , ((Blue,    Yellow ),east)
                            , ((Blue,    Purple), south)
 
                            , ((Orange,  Blue),   west)
                            , ((Orange,  Green),  north)
                            , ((Orange,  Red ),   east)
                            , ((Orange,  Yellow), south)
                            ]

        Just goLeft = DM.lookup (centerFaceColor, westFaceColor) leftDirs
    in (west.goLeft) centerFace

viewModel :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
viewModel model = do
    bottomMap <-                    mapDyn bottomView model
    lowerMiddleMap <-               mapDyn lowerMiddleView model
    middleMiddleMap <-              mapDyn middleMiddleView model
    upperMiddleMap <-               mapDyn upperMiddleView model
    topMap <-                       mapDyn topView model

    listWithKey bottomMap $ const showFace
    listWithKey lowerMiddleMap $ const showLowerMiddleFace
    listWithKey middleMiddleMap $ const showMiddleMiddleFace
    listWithKey upperMiddleMap $ const showUpperMiddleFace
    listWithKey topMap $ const showFace

    return never

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = 
    el "div" $ do
        (_,ev) <-    elDynAttrNS' svgNamespace "svg" 
                       (constDyn $  "width" =: show viewScale
                                 <> "height" =: show viewScale
                                 ) $ viewModel model
        return never

