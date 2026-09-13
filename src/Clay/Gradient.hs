{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}
module Clay.Gradient
(
-- * Color ramp type.

  Ramp

-- * Linear gradients.

, linearGradient
, hGradient
, vGradient

-- * Radial gradients.

, Radial
, circle, ellipse
, circular, elliptical

, Extend
, closestSide, closestCorner, farthestSide, farthestCorner

, radialGradient

-- * Repeating gradients.

, repeatingLinearGradient
, hRepeatingGradient
, vRepeatingGradient
, repeatingRadialGradient

)
where

import Clay.Color
import Clay.Common
import Clay.Property
import Clay.Size
import Clay.Background

type Ramp = [(Color, Size Percentage)]

-------------------------------------------------------------------------------

linearGradient :: Direction -> Ramp -> BackgroundImage
linearGradient d xs = other $ Value $
  let Value v = "linear-gradient(" <> value d <> "," <> ramp xs <> ")"
   in browsers <> v

hGradient, vGradient :: Color -> Color -> BackgroundImage

-- The sides are the ones the gradient runs toward, so that the first
-- color is at the left/top as before.  They read as the opposite of
-- what they were because 'straight' used to take the side the gradient
-- started from.
hGradient = shortcut (linearGradient (straight sideRight ))
vGradient = shortcut (linearGradient (straight sideBottom))

-------------------------------------------------------------------------------

repeatingLinearGradient :: Direction -> Ramp -> BackgroundImage
repeatingLinearGradient d xs = other $ Value $
  let Value v = "repeating-linear-gradient(" <> value d <> "," <> ramp xs <> ")"
   in browsers <> v

hRepeatingGradient, vRepeatingGradient :: Color -> Color -> BackgroundImage

hRepeatingGradient = shortcut (repeatingLinearGradient (straight sideRight ))
vRepeatingGradient = shortcut (repeatingLinearGradient (straight sideBottom))

-------------------------------------------------------------------------------

newtype Radial = Radial Value
  deriving (Val, Other)

circle :: Extend -> Radial
circle ext = Radial ("circle " <> value ext)

ellipse :: Extend -> Radial
ellipse ext = Radial ("ellipse " <> value ext)

circular :: Size LengthUnit -> Radial
circular radius = Radial (value (radius, radius))

elliptical :: Size a -> Size a -> Radial
elliptical radx rady = Radial (value (radx, rady))

newtype Extend = Extend Value
  deriving (Val, Other)

closestSide, closestCorner, farthestSide, farthestCorner :: Extend

closestSide    = Extend "closest-side"
closestCorner  = Extend "closest-corner"
farthestSide   = Extend "farthest-side"
farthestCorner = Extend "farthest-corner"

-------------------------------------------------------------------------------

radialGradient :: Loc l => l -> Radial -> Ramp -> BackgroundImage
radialGradient d r xs = other $ Value $
  let Value v = "radial-gradient(" <> value [value d, value r, ramp xs] <> ")"
   in browsers <> v

repeatingRadialGradient :: Loc l => l -> Radial -> Ramp -> BackgroundImage
repeatingRadialGradient d r xs = other $ Value $
  let Value v = "repeating-radial-gradient(" <> value [value d, value r, ramp xs] <> ")"
   in browsers <> v

-------------------------------------------------------------------------------

ramp :: Ramp -> Value
ramp xs = value (map (\(a, b) -> value (value a, value b)) xs)

shortcut :: (Ramp -> BackgroundImage) -> Color -> Color -> BackgroundImage
shortcut g f t = g [(f, (pct 0)), (t, (pct 100))]

