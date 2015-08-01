{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Composition((.:))
import Tic.HudPictureInput
import Wrench.MonadGame
import Wrench.SpriteIdentifier
import Wrench.MouseGrabMode
import Wrench.RenderBlockMode
import Wrench.WindowSize
import Wrench.Time
import Tic.UnitType
import Wrench.Keysym(Keysym(Space))
import Wrench.Color
import Wrench.BitmapFont.RenderResult
import System.Random.Shuffle(shuffleM)
import Control.Monad.Random
import Wrench.Event
import Wrench.Picture
import Wrench.MouseButtonMovement
import ClassyPrelude hiding(head)
import Wrench.Rectangle
import Control.Monad.State.Strict(MonadState,execStateT)
import Tic.ViewportCoord
import Tic.GeoCoord
import Tic.CoordTransform
import Tic.LocationSelectionResult
import Tic.Level
import Tic.Score
import Tic.Location
import Wrench.RenderPositionMode
import Tic.GameState
import Control.Lens((^.),(^..),filtered,folding,ix,(^?!),use,(%=),(<~),(+=),_head,(^?),(&),view,from,Iso',has,makePrisms,(%~))
import Data.List(tail,head)
import Linear.V2

mapImageId :: SpriteIdentifier
mapImageId = "map"

fitMap :: (Num a,Fractional a,Ord a) => OuterRectangle a -> InnerRectangle a -> Rectangle a
fitMap = fitRectAspectPreserving

mapPicture :: Num a => Rectangle a -> Picture a b
mapPicture fitRect = (fitRect ^. rectLeftTop) `pictureTranslated` pictureSpriteResampled mapImageId RenderPositionTopLeft (fitRect ^. rectDimensions)

type MouseCoord = V2 UnitType
type MapRectangle = Rectangle UnitType

{-
toImageCoord :: MapRectangle -> ViewportCoord UnitType -> Maybe (ImageCoord UnitType)
toImageCoord image mouse
  | pointInRectangle mouse (image ^. viewportToVector) = Just (mouse - (image ^. rectLeftTop))
  | otherwise = Nothing
-}

levelDatabase :: [[Location UnitType]]
levelDatabase = [
    [ Location "Hannover" (GeoCoord 52.518611 13.408333)
    , Location "Osnabrück" (GeoCoord 52.278889 8.043056)
    ]
  , [ Location "Belm" (GeoCoord 52.3 8.133333)
    , Location "Mali" (GeoCoord 17.0 4.366667)
    ]
  ]

chooseLocationSequence :: MonadRandom m => Level -> m [Location UnitType]
chooseLocationSequence level = do
  shuffled <- shuffleM (levelDatabase ^?! ix (level ^. _Level))
  return (take (locationsPerLevel level) shuffled)

minScoreForLevel :: Level -> Score
minScoreForLevel _ = 100000

timerDurationForLevel :: Level -> TimeDelta
timerDurationForLevel _ = fromSeconds 10

locationsPerLevel :: Level -> Int
locationsPerLevel = const 2

newtype DistanceKilometers a = DistanceKilometers a

$(makePrisms ''DistanceKilometers)

distanceAndTimeToScore :: DistanceKilometers UnitType -> TimeDelta -> Score
distanceAndTimeToScore distance _ =
  let
    maxDist = 5000
    diffi = 5
    distanceUnpack = distance ^. _DistanceKilometers
    distScore = ((1-min distanceUnpack maxDist/maxDist)**diffi)*maxDist
  in
    floor distScore

geoLocationDistance :: GeoCoord UnitType -> GeoCoord UnitType -> DistanceKilometers UnitType
geoLocationDistance = view (from _DistanceKilometers) .: distanceHaversine

pixelDistance :: V2 UnitType -> V2 UnitType -> UnitType
pixelDistance = error "pixel distance not implemented"

timeLeft :: (Monad m,MonadGame m,MonadState GameState m) => m TimeDelta
timeLeft = do
  timerInited <- use gsTimerInited
  currentLevel <- use gsCurrentLevel
  let
    levelEnd = timerInited `plusDuration` timerDurationForLevel currentLevel
  currentTicks <- gcurrentTicks
  return (levelEnd `tickDelta` currentTicks)

type RectangleSize a = V2 a

data Alignment = AlignLeftOrTop
               | AlignCenter
               | AlignRightOrBottom

alignRectWithSize :: Integral a => Rectangle a -> RectangleSize a -> (Alignment,Alignment) -> Rectangle a
alignRectWithSize outer size (ax,ay) =
  let
    alignDim left _ _ AlignLeftOrTop = left
    alignDim left right sz AlignCenter = left + ((right - left) `div` 2) - (sz `div` 2)
    alignDim _ right sz AlignRightOrBottom = right - sz
    origin = V2 (alignDim (outer ^. rectLeft) (outer ^. rectRight) (size ^. _x) ax) (alignDim (outer ^. rectTop) (outer ^. rectBottom) (size ^. _y) ay)
  in
    rectFromOriginAndDim origin size

statusText :: (HasGameState s,MonadState s m) => m Text
statusText = do
  currentLevel <- use gsCurrentLevel
  currentScore <- use gsLevelScore
  let totalScore = minScoreForLevel currentLevel
  return $ "L" <> pack (show currentLevel) <> " " <> pack (show currentScore) <> "/" <> pack (show totalScore) <> " "

locationText :: (HasGameState s,Functor m,MonadState s m) => Location a -> m Text
locationText location =
  return $ " " <> location ^. locName

currentPercent :: (HasGameState s,MonadGame m,MonadState s m) => m UnitType
currentPercent = do
  timerInited <- use gsTimerInited
  currentLevel <- use gsCurrentLevel
  currentTicks <- gcurrentTicks
  return $ toSeconds (currentTicks `tickDelta` timerInited) / toSeconds (timerDurationForLevel currentLevel)

currentPercentCapped :: (HasGameState s,MonadGame m,MonadState s m) => m UnitType
currentPercentCapped = do
  p <- currentPercent
  return (min 1 p)

timedOut :: (HasGameState s,MonadGame m,MonadState s m) => m Bool
timedOut = do
  p <- currentPercent
  return (p >= 1)

localRenderText :: Num unit => MonadGame m => Text -> m (RenderResult unit float)
localRenderText = grenderText "djvu" (-1)

-- | Generate HUD for location selection
hudPicture :: (HasGameState s,Monad m,MonadState s m,MonadGame m,Functor m) => HudPictureInput UnitType -> m (Picture Int UnitType)
hudPicture texts = do
  viewport <- originRectangle <$> gviewportSize
  let
    hudRect = rectFromOriginAndDim (V2 0 0) (V2 (viewport ^. rectWidth) (viewport ^. rectHeight `div` 8))
    localRenderText = grenderText "djvu" (-1)
  leftText <- forM (texts ^. hpiLeftText) localRenderText 
  centerText <- forM (texts ^. hpiCenterText) localRenderText
  rightText <- forM (texts ^. hpiRightText) localRenderText
  barSize <- view rectDimensions <$> glookupImageRectangleUnsafe "ui_bar"
  let
    barRect = alignRectWithSize hudRect barSize (AlignCenter,AlignCenter)
    transformPicture (xa) = foldMap (\t -> (alignRectWithSize barRect (t ^. bfrrSize) (xa,AlignCenter) ^. rectLeftTop) `pictureTranslated` (t ^. bfrrPicture))
    leftTextPicture = transformPicture AlignLeftOrTop leftText
    centerTextPicture = transformPicture AlignCenter centerText
    rightTextPicture = transformPicture AlignRightOrBottom rightText
    gameStateBarPicture = (barRect ^. rectLeftTop) `pictureTranslated` pictureSpriteTopLeft "ui_bar"
    gamePercentBar = foldMap (\percent -> (barRect ^. rectLeftTop) `pictureTranslated` pictureSpriteResampled "ui_percent_bar" RenderPositionTopLeft ((barRect ^. rectDimensions) & _x %~ (\w -> floor (fromIntegral w * percent)))) (texts ^. hpiPercentFilled)
  return (gameStateBarPicture <> gamePercentBar <> leftTextPicture <> centerTextPicture <> rightTextPicture)

-- | Let the user click the map or do nothing
selectLocation :: (Num a,Fractional a,Floating a,Eq a) => (HasGameState s,MonadGame m,MonadState s m,Monad m,Functor m) => Location a -> m (LocationSelectionResult a)
selectLocation location = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  viewport <- originRectangle <$> gviewportSize
  mapImage <- glookupImageRectangleUnsafe mapImageId
  let
    floatViewport = fromIntegral <$> viewport
    floatMapImage = fromIntegral <$> mapImage
    fitRect = floor <$> (fitMap floatViewport floatMapImage :: Rectangle UnitType) :: Rectangle Int
    lastClick = (events ^.. traverse . _MouseButton . filtered ((== ButtonDown) . (^. mouseButtonMovement)) . mousePosition . folding (^. from viewportToVector . viewportCoordToImageCoord fitRect)) ^? _head
  case lastClick of
    Just clickPosition ->
      return (LocationClicked ((fromIntegral <$> clickPosition) ^. imageCoordToGeoCoord (fromIntegral <$> (fitRect ^. rectDimensions))))
    Nothing -> do
      timeout <- timedOut
      if timeout
        then return LocationTimedOut
        else do
          stText <- statusText
          locText <- locationText location
          curPercent <- currentPercentCapped
          let hudPicInput = HudPictureInput {_hpiLeftText=Just locText,_hpiCenterText=Nothing,_hpiRightText=Just stText,_hpiPercentFilled=Just curPercent}
          hudPic <- hudPicture hudPicInput 
          grender ((mapPicture fitRect :: Picture Int Double) <> (hudPic :: Picture Int Double))
          selectLocation location

mouseButtonClicked :: Traversable t => t Event -> Bool
mouseButtonClicked events = isJust ((events ^.. traverse . _MouseButton . mouseButtonMovement . filtered (== ButtonDown)) ^? _head)

spaceKeyPressed :: Traversable t => t Event -> Bool
--spaceKeyPressed events = has (events ^.. traverse . _Keyboard . keySym . filtered (== Space))
spaceKeyPressed = has (traverse . _Keyboard . keySym . filtered (== Space))

-- | Show a confirmation of the last score (if the user clicked something, anyway)
confirmScore :: forall s m.(MonadIO m,HasGameState s,Monad m,MonadGame m,Functor m,MonadState s m) => Location UnitType -> Maybe (GeoCoord UnitType) -> Score -> Maybe (DistanceKilometers UnitType) -> Maybe TimeDelta -> m ()
confirmScore correctLocation clickedLocation score maybeDistance maybeTime = do
  events <- gpollEvents
  gupdateTicks 1.0
  gupdateKeydowns events
  unless (spaceKeyPressed events) $ do
    viewport <- (fmap fromIntegral . originRectangle) <$> gviewportSize
    mapImage <- fmap fromIntegral <$> glookupImageRectangleUnsafe mapImageId
    let
      fitRect = fitMap viewport mapImage
      mousePinId = "ui_mouse_pin"
      correctPinId = "ui_correct_pin"
    mousePinImage <- fmap fromIntegral <$> glookupImageRectangleUnsafe mousePinId
    correctPinImage <- fmap fromIntegral <$> glookupImageRectangleUnsafe correctPinId
    let
      geoCoordToViewportCoord :: Iso' (GeoCoord UnitType) (ViewportCoord UnitType)
      geoCoordToViewportCoord = geoCoordToImageCoord (fitRect ^. rectDimensions) . imageCoordToViewportCoord (fitRect ^. rectLeftTop)
      correctLocationPixels :: ViewportCoord UnitType
      correctLocationPixels = correctLocation ^. locCoord . geoCoordToViewportCoord

      correctPinPicture = ((correctLocationPixels ^. viewportToVector) - V2 (correctPinImage ^. rectDimensions . _x / 2) (correctPinImage ^. rectDimensions . _y)) `pictureTranslated` pictureSpriteTopLeft correctPinId
      clickedLocationPixels :: Maybe (ViewportCoord UnitType)
      clickedLocationPixels = (^. geoCoordToViewportCoord) <$> clickedLocation
      clickedPinPicture = foldMap (\clickedLocation' -> ((clickedLocation' ^. viewportToVector) - V2 (mousePinImage ^. rectDimensions . _x / 2) (mousePinImage ^. rectDimensions . _y)) `pictureTranslated` pictureSpriteTopLeft mousePinId) clickedLocationPixels
    hudPic <- hudPicture HudPictureInput{_hpiLeftText=Nothing,_hpiCenterText=Just "Press space to continue",_hpiRightText=Nothing,_hpiPercentFilled=Nothing}
    grender (first floor (mapPicture fitRect) <> first floor clickedPinPicture <> first floor correctPinPicture <> hudPic)
  
data GameoverConfirmResult = WantsAnotherRound
                           | WantsToQuit

-- | Show a gameover dialog with the end result. Ask for another game
confirmGameover :: m GameoverConfirmResult
confirmGameover = error "confirmGameover not implemented yet"

-- | Calculate distance, increase current level score and total score accordingly. Return distance/score
increaseScore :: (HasGameState s,MonadState s m) => LocationSelectionResult UnitType -> Location UnitType -> m (Maybe (DistanceKilometers UnitType),Score)
increaseScore locationSelection location =
  case locationSelection of
    LocationClicked p -> do
      let
        coordDistanceKm = geoLocationDistance p ((^. toRadians) <$> location ^. locCoord)
        score = distanceAndTimeToScore coordDistanceKm (error "time not necessary for score yet")
      gsTotalScore += score
      gsLevelScore += score
      return (Just coordDistanceKm,score)
    LocationTimedOut ->
      return (Nothing,0)
                             

-- | Determine if the current level score is high enough to advance to the next level
determineLevelSuccess :: m Bool
determineLevelSuccess = error "determineLevelSuccess not implemented"

-- | Choose new locations, reset level score                       
advanceLevel :: m ()
advanceLevel = error "advanceLevel not implemented"

-- | Reset level, reset locations, score etc.
resetGame :: m ()
resetGame = error "resetGame not implemented"

-- | Confirm gameover, perform resetting action of needed
handleGameover :: (MonadGame m,HasGameState s,MonadState s m,Monad m,MonadIO m,Functor m) => m ()
handleGameover = do
  gameoverResult <- confirmGameover
  case gameoverResult of
    WantsAnotherRound -> do
      resetGame
      mainLoop
    WantsToQuit -> return ()

popNextLocation :: (HasGameState s,Monad m,Functor m,MonadState s m) => m (Location UnitType)
popNextLocation = do
  -- TODO: Lens here instead of head?
  result <- head <$> use gsLocationSequence
  gsLocationSequence %= tail
  return result
  
packShow :: Show a => a -> Text
packShow = pack . show

--mainLoop :: (Monad m,Applicative m,Functor m,MonadIO m,MonadGame m,MonadState GameState m,MonadRandom m) => m ()
mainLoop :: (MonadGame m,HasGameState s,MonadState s m,Monad m,Functor m,MonadIO m) => m ()
mainLoop = do
  currentLocation <- popNextLocation
  putStrLn $ "Asking to click for location: " <> packShow currentLocation
  locationSelection <- selectLocation currentLocation
  putStrLn $ "Location selection done, increasing score: " <> packShow locationSelection
  (distance,score) <- increaseScore locationSelection currentLocation
  putStrLn "Confirming score"
  confirmScore currentLocation (locationSelection ^? _LocationClicked) score distance Nothing
  remainingLocationsInLevel <- use gsLocationSequence
  if null remainingLocationsInLevel
  then do
    putStrLn "No more locations in level, determining success"
    levelSuccessful <- determineLevelSuccess
    if levelSuccessful
    then do
      putStrLn "Level is succesful, advancing to next level"
      gsTimerInited <~ gcurrentTicks
      advanceLevel
      mainLoop
    else do
      putStrLn "Level is not succesful, going to gameover handling"
      handleGameover
  else do
    putStrLn "There are locations left in this level, continuing"
    gsTimerInited <~ gcurrentTicks
    mainLoop

main :: IO ()
main =
  runGame "media" "tic 0.1" DynamicWindowSize MouseGrabNo (Just colorsBlack) (RenderAndWait 60) $ do
    initialLocations <- chooseLocationSequence (Level 0)
    currentTicks <- gcurrentTicks
    let
      initialGameState = GameState {
            _gsCurrentLevel = Level 0
          , _gsTimerInited = currentTicks
          , _gsLocationSequence = initialLocations
          , _gsTotalScore = 0
          , _gsLevelScore = 0
          }
    _ <- execStateT mainLoop initialGameState
    return ()
