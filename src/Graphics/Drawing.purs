-- | This module defines a type `Drawing` for creating vector graphics.

module Graphics.Drawing
  ( Point
  , Shape, path, closed, rectangle, circle, arc
  , FillStyle, fillColor
  , OutlineStyle, outlineColor, lineWidth
  , Shadow, shadowOffset, shadowBlur, shadowColor, shadow
  , Drawing, filled, outlined, clipped, scale, translate, rotate, text
  , everywhere
  , render
  , module Color
  , module Font
  ) where

import Prelude

import Color (Color, cssStringHSLA)
import Color (Color) as Color
import Control.Alt ((<|>))
import Data.Foldable (class Foldable, for_)
import Data.List (List(..), singleton, (:), fromFoldable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas as Canvas
import Graphics.Drawing.Font (Font, fontString)
import Graphics.Drawing.Font (Font) as Font
import Math (pi)

-- | A `Point` consists of `x` and `y` coordinates.
type Point = { x :: Number, y :: Number }

-- | A single shape.
data Shape
  -- | A path is a list of points joined by line segments
  = Path Boolean (List Point)
  -- | A rectangle consisting of the numbers left, top, width and height
  | Rectangle Canvas.Rectangle
  -- | A circular arc consisting of the numbers center-x, center-y, start angle, end angle and radius
  | Arc Canvas.Arc
  -- | A composite shape
  | Composite (List Shape)
  
derive instance eqShape :: Eq Shape

instance semigroupShape :: Semigroup Shape where
  append (Composite ds) d = Composite (ds <> singleton d)
  append d (Composite ds) = Composite (d : ds)
  append d1 d2 = Composite (Cons d1 (Cons d2 Nil))

instance monoidShape :: Monoid Shape where
  mempty = Composite mempty

-- | Create a path.
path :: forall f. (Foldable f) => f Point -> Shape
path = Path false <<< fromFoldable

-- | Create a _closed_ path.
closed :: forall f. (Foldable f) => f Point -> Shape
closed = Path true <<< fromFoldable

-- | Create a rectangle from the left, top, width and height parameters.
rectangle :: Number -> Number -> Number -> Number -> Shape
rectangle x y width height = Rectangle { x, y, width, height }

-- | Create a circle from the center-x, center-y and radius parameters.
circle :: Number -> Number -> Number -> Shape
circle x y = arc x y 0.0 (pi * 2.0)

-- | Create a circular arc from the center-x, center-y, start angle, end angle
-- | and radius parameters.
arc :: Number -> Number -> Number -> Number -> Number -> Shape
arc x y start end radius = Arc { x, y, start, end, radius }

-- | Encapsulates fill color etc.
newtype FillStyle = FillStyle
  { color :: Maybe Color
  }

instance semigroupFillStyle :: Semigroup FillStyle where
  append (FillStyle f1) (FillStyle f2) = FillStyle { color: f1.color <|> f2.color }

instance monoidFillStyle :: Monoid FillStyle where
  mempty = FillStyle { color: Nothing }

derive instance eqFillStyle :: Eq FillStyle

-- | Set the fill color.
fillColor :: Color -> FillStyle
fillColor c = FillStyle { color: Just c }

-- | Encapsulates outline color etc.
newtype OutlineStyle = OutlineStyle
  { color :: Maybe Color
  , lineWidth :: Maybe Number
  }

-- | Set the outline color.
outlineColor :: Color -> OutlineStyle
outlineColor c = OutlineStyle { color: Just c, lineWidth: Nothing }

-- | Set the line width.
lineWidth :: Number -> OutlineStyle
lineWidth c = OutlineStyle { color: Nothing, lineWidth: Just c }

instance semigroupOutlineStyle :: Semigroup OutlineStyle where
  append (OutlineStyle f1) (OutlineStyle f2) = OutlineStyle { color:     f1.color     <|> f2.color
                                                            , lineWidth: f1.lineWidth <|> f2.lineWidth
                                                            }

instance monoidOutlineStyle :: Monoid OutlineStyle where
  mempty = OutlineStyle { color: Nothing
                        , lineWidth: Nothing
                        }

derive instance eqOutlineStyle :: Eq OutlineStyle

-- | Encapsulates shadow settings etc.
newtype Shadow = Shadow
  { color  :: Maybe Color
  , blur   :: Maybe Number
  , offset :: Maybe { x :: Number, y :: Number }
  }

derive instance eqShadow :: Eq Shadow

-- | Set the shadow color.
shadowColor :: Color -> Shadow
shadowColor c = Shadow { color: Just c, blur: Nothing, offset: Nothing }

-- | Set the shadow blur.
shadowBlur :: Number -> Shadow
shadowBlur b = Shadow { color: Nothing, blur: Just b, offset: Nothing }

-- | Set the shadow blur.
shadowOffset :: Number -> Number -> Shadow
shadowOffset x y = Shadow { color: Nothing, blur: Nothing, offset: Just { x: x, y: y } }

instance semigroupShadow :: Semigroup Shadow where
  append (Shadow s1) (Shadow s2) = Shadow { color:  s1.color   <|> s2.color
                                          , blur:   s1.blur    <|> s2.blur
                                          , offset: s1.offset  <|> s2.offset
                                          }

instance monoidShadow :: Monoid Shadow where
  mempty = Shadow { color: Nothing
                  , blur: Nothing
                  , offset: Nothing
                  }

-- | A vector `Drawing`.
data Drawing
  = Fill Shape FillStyle
  | Outline Shape OutlineStyle
  | Text Font Number Number FillStyle String
  | Many (List Drawing)
  | Scale { scaleX :: Number, scaleY :: Number } Drawing
  | Translate { translateX :: Number, translateY :: Number } Drawing
  | Rotate Number Drawing
  | Clipped Shape Drawing
  | WithShadow Shadow Drawing

instance semigroupDrawing :: Semigroup Drawing where
  append (Many ds) d = Many (ds <> singleton d)
  append d (Many ds) = Many (d : ds)
  append d1 d2 = Many (Cons d1 (Cons d2 Nil))

instance monoidDrawing :: Monoid Drawing where
  mempty = Many mempty

derive instance eqDrawing :: Eq Drawing

-- | Fill a `Shape`.
filled :: FillStyle -> Shape -> Drawing
filled = flip Fill

-- | Draw the outline of a `Shape`.
outlined :: OutlineStyle -> Shape -> Drawing
outlined = flip Outline

-- | Clip a `Drawing` to a `Shape`.
clipped :: Shape -> Drawing -> Drawing
clipped = Clipped

-- | Apply a `Shadow` to a `Drawing`.
shadow :: Shadow -> Drawing -> Drawing
shadow = WithShadow

-- | Apply a scale transformation by providing the x and y scale factors.
scale :: Number -> Number -> Drawing -> Drawing
scale sx sy = Scale { scaleX: sx, scaleY: sy }

-- | Apply a translation by providing the x and y distances.
translate :: Number -> Number -> Drawing -> Drawing
translate tx ty = Translate { translateX: tx, translateY: ty }

-- | Apply a rotation by providing the angle.
rotate :: Number -> Drawing -> Drawing
rotate = Rotate

-- | Render some text.
text :: Font -> Number -> Number -> FillStyle -> String -> Drawing
text = Text

-- | Modify a `Drawing` by applying a transformation to every subdrawing.
everywhere :: (Drawing -> Drawing) -> Drawing -> Drawing
everywhere f = go
  where
  go (Many ds) = f (Many (map go ds))
  go (Scale s d) = f (Scale s (go d))
  go (Translate t d) = f (Translate t (go d))
  go (Rotate r d) = f (Rotate r (go d))
  go (Clipped s d) = f (Clipped s (go d))
  go (WithShadow s d) = f (WithShadow s (go d))
  go other = f other

-- | Render a `Drawing` to a canvas.
render :: Canvas.Context2D -> Drawing -> Effect Unit
render ctx = go
  where
  go (Fill sh style) = void $ Canvas.withContext ctx do
    applyFillStyle style
    Canvas.fillPath ctx $
      renderShape sh
  go (Outline sh style) = void $ Canvas.withContext ctx do
    applyOutlineStyle style
    Canvas.strokePath ctx $
      renderShape sh
  go (Many ds) = for_ ds go
  go (Scale s d) = void $ Canvas.withContext ctx do
    _ <- Canvas.scale ctx s
    go d
  go (Translate t d) = void $ Canvas.withContext ctx do
    _ <- Canvas.translate ctx t
    go d
  go (Rotate r d) = void $ Canvas.withContext ctx do
    _ <- Canvas.rotate ctx r
    go d
  go (Clipped sh d) = void $ Canvas.withContext ctx do
    renderShape sh
    _ <- Canvas.clip ctx
    go d
  go (WithShadow sh d) = void $ Canvas.withContext ctx do
    applyShadow sh
    go d
  go (Text font x y style s) = void $ Canvas.withContext ctx do
    _ <- Canvas.setFont ctx (fontString font)
    applyFillStyle style
    Canvas.fillText ctx s x y

  applyShadow :: Shadow -> Effect Unit
  applyShadow (Shadow s) = do
    for_ s.color \color -> Canvas.setShadowColor ctx (cssStringHSLA color)
    for_ s.blur \blur -> Canvas.setShadowBlur ctx blur
    for_ s.offset \offset -> do
      _ <- Canvas.setShadowOffsetX ctx offset.x
      Canvas.setShadowOffsetY ctx offset.y

  applyFillStyle :: FillStyle -> Effect Unit
  applyFillStyle (FillStyle fs) = do
    for_ fs.color $ \color -> Canvas.setFillStyle ctx (cssStringHSLA color)

  applyOutlineStyle :: OutlineStyle -> Effect Unit
  applyOutlineStyle (OutlineStyle fs) = do
    for_ fs.color $ \color -> Canvas.setStrokeStyle ctx (cssStringHSLA color)
    for_ fs.lineWidth $ \width -> Canvas.setLineWidth ctx width

  renderShape :: Shape -> Effect Unit
  renderShape (Path _ Nil) = pure unit
  renderShape (Path cl (Cons p rest)) = do
    _ <- Canvas.moveTo ctx p.x p.y
    for_ rest \pt -> Canvas.lineTo ctx pt.x pt.y
    when cl $ void $ Canvas.closePath ctx
  renderShape (Rectangle r) = void $ Canvas.rect ctx r
  renderShape (Arc a) = void $ Canvas.arc ctx a
  renderShape (Composite ds) = for_ ds renderShape
