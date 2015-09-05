-- | This module defines a type `Drawing` for creating vector graphics.

module Graphics.Drawing
  ( Point
  , Shape, path, closed, rectangle, circle
  , FillStyle, fillColor
  , OutlineStyle, outlineColor, lineWidth
  , Shadow, shadowOffset, shadowBlur, shadowColor, shadow
  , Drawing, filled, outlined, clipped, scale, translate, rotate, text
  , above, beside, shapeSize, drawingSize
  , everywhere
  , render
  , module Color
  , module Graphics.Drawing.Font
  ) where

import Prelude (class Eq, class Semigroup, Unit, void, when, bind, unit, pure,
                map, flip, (*), ($), (==), (&&), (<>), (<<<))

import Color
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Data.Foldable (class Foldable, for_)
import Data.List (List(..), singleton, (:), fromFoldable)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Monoid (class Monoid, mempty)
import Graphics.Canvas as Canvas
import Graphics.Drawing.Font (Font(), fontString)
import Math (pi)

-- | A `Point` consists of `x` and `y` coordinates.
type Point = { x :: Number, y :: Number }

-- | A single shape.
data Shape
  -- | A path is a list of points joined by line segments
  = Path Boolean (List Point)
  -- | A rectangle consisting of width and height
  | Rectangle { w :: Number, h :: Number }
  -- | A circle consisting of its radius
  | Circle { r :: Number }

instance eqShape :: Eq Shape where
  eq (Path a b) (Path a' b') = a == a'
                            && map _.x b == map _.x b'
                            && map _.y b == map _.y b'
  eq (Rectangle a) (Rectangle a') = a.w == a'.w
                                 && a.h == a'.h
  eq (Circle a) (Circle a') = a.r == a'.r
  eq _ _ = false

-- | Create a path.
path :: forall f. (Foldable f) => f Point -> Shape
path = Path false <<< fromFoldable

-- | Create a _closed_ path.
closed :: forall f. (Foldable f) => f Point -> Shape
closed = Path true <<< fromFoldable

-- | Create a rectangle from the width and height parameters.
rectangle :: Number -> Number -> Shape
rectangle w h = Rectangle { w: w, h: h }

-- | Create a circle from its radius.
circle :: Number -> Shape
circle r = Circle { r: r }

-- | Get the width and height of a Shape
shapeSize :: Shape -> { w :: Number, h :: Number }
shapeSize (Path _ points) =
  { w: foldr max 0.0 $ map _.x points, h: foldr max 0.0 $ map _.y points }
shapeSize (Rectangle a) = a
shapeSize (Circle a) = { w: 2.0 * a.r, h: 2.0 * a.r }

-- | Encapsulates fill color etc.
newtype FillStyle = FillStyle
  { color :: Maybe Color
  }

instance semigroupFillStyle :: Semigroup FillStyle where
  append (FillStyle f1) (FillStyle f2) = FillStyle { color: f1.color <|> f2.color }

instance monoidFillStyle :: Monoid FillStyle where
  mempty = FillStyle { color: Nothing }

instance eqFillStyle :: Eq FillStyle where
  eq (FillStyle a) (FillStyle a') = a.color == a'.color

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

instance eqOutlineStyle :: Eq OutlineStyle where
  eq (OutlineStyle a) (OutlineStyle a') = a.color == a'.color
                                       && a.lineWidth == a'.lineWidth

-- | Encapsulates shadow settings etc.
newtype Shadow = Shadow
  { color  :: Maybe Color
  , blur   :: Maybe Number
  , offset :: Maybe { x :: Number, y :: Number }
  }

instance eqShadow :: Eq Shadow where
  eq (Shadow a) (Shadow a') = a.color == a'.color
                           && a.blur  == a'.blur
                           && maybe (isNothing a'.offset)
                                    (\o -> maybe false
                                                 (\o' -> o.x == o'.x && o.y == o'.y)
                                                 a'.offset)
                                    a.offset

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

instance eqDrawing :: Eq Drawing where
  eq (Fill a b) (Fill a' b') = a == a'
                            && b == b'
  eq (Outline a b) (Outline a' b') = a == a'
                                  && b == b'
  eq (Text a b c d e) (Text a' b' c' d' e') = a == a'
                                           && b == b'
                                           && c == c'
                                           && d == d'
                                           && e == e'
  eq (Many a) (Many a') = a == a'
  eq (Scale a b) (Scale a' b') = a.scaleX == a'.scaleX
                              && a.scaleY == a'.scaleY
                              && b == b'
  eq (Translate a b) (Translate a' b') = a.translateX == a'.translateX
                                      && a.translateY == a'.translateY
                                      && b == b'
  eq (Rotate a b) (Rotate a' b') = a == a'
                                && b == b'
  eq (Clipped a b) (Clipped a' b') = a == a'
                                  && b == b'
  eq (WithShadow a b) (WithShadow a' b') = a == a'
                                        && b == b'
  eq _ _ = false

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

-- | Get the size of a Drawing.
drawingSize :: Drawing -> { w:: Number, h:: Number }
drawingSize (Fill shape _) = shapeSize shape
drawingSize (Outline shape (OutlineStyle oStyle)) = shapeSize shape -- TODO
drawingSize (Text font x y fillStyle string) = { w: 0.0, h: 0.0} -- TODO
drawingSize (Many ds) =
  let sizes = map drawingSize ds
  in { w: foldr max 0.0 $ map _.w sizes, h: foldr max 0.0 $ map _.h sizes }
drawingSize (Scale s d) =
  let sz = drawingSize d
  in { w: sz.w * s.scaleX, h: sz.h * s.scaleY }
drawingSize (Translate t d) =
  let sz = drawingSize d
  in { w: sz.w + t.translateX, h: sz.h + t.translateY }
drawingSize (Rotate angle d) = drawingSize d -- TODO
drawingSize (Clipped shape d) = drawingSize d -- TODO
drawingSize (WithShadow shadow d) = drawingSize d -- TODO

-- | Place a Drawing above another.
above :: Drawing -> Drawing -> Drawing
above a b =
  let sa = drawingSize a
      sb = drawingSize b
      c = (sa.w + sb.w) / 2.0
  in if sa.w > sb.w
     then Many (Cons (translate (c - sb.w) sa.h b) (Cons a Nil))
     else Many (Cons (translate 0.0 sa.h b) (Cons (translate (c - sa.w) 0.0 a) Nil))

import Debug.Trace

-- | Place a Drawing beside another.
beside :: Drawing -> Drawing -> Drawing
beside a b =
  let sa = drawingSize a
      sb = drawingSize b
      c = (sa.h + sb.h) / 2.0
  in if sa.h > sb.h
     then Many (Cons (translate sa.w (c - sb.h) b) (Cons a Nil))
     else Many (Cons (translate sa.w 0.0 b) (Cons (translate 0.0 (c - sa.h) a) Nil))

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
render :: forall eff. Canvas.Context2D -> Drawing -> Eff (canvas :: Canvas.CANVAS | eff) Unit
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
    Canvas.scale s ctx
    go d
  go (Translate t d) = void $ Canvas.withContext ctx do
    Canvas.translate t ctx
    go d
  go (Rotate r d) = void $ Canvas.withContext ctx do
    Canvas.rotate r ctx
    go d
  go (Clipped sh d) = void $ Canvas.withContext ctx do
    renderShape sh
    Canvas.clip ctx
    go d
  go (WithShadow sh d) = void $ Canvas.withContext ctx do
    applyShadow sh
    go d
  go (Text font x y style s) = void $ Canvas.withContext ctx do
    Canvas.setFont (fontString font) ctx
    applyFillStyle style
    Canvas.fillText ctx s x y

  applyShadow :: Shadow -> Eff (canvas :: Canvas.CANVAS | eff) Unit
  applyShadow (Shadow s) = do
    for_ s.color \color -> Canvas.setShadowColor (cssStringHSLA color) ctx
    for_ s.blur \blur -> Canvas.setShadowBlur blur ctx
    for_ s.offset \offset -> do
      Canvas.setShadowOffsetX offset.x ctx
      Canvas.setShadowOffsetY offset.y ctx

  applyFillStyle :: FillStyle -> Eff (canvas :: Canvas.CANVAS | eff) Unit
  applyFillStyle (FillStyle fs) = do
    for_ fs.color $ \color -> Canvas.setFillStyle (cssStringHSLA color) ctx

  applyOutlineStyle :: OutlineStyle -> Eff (canvas :: Canvas.CANVAS | eff) Unit
  applyOutlineStyle (OutlineStyle fs) = do
    for_ fs.color $ \color -> Canvas.setStrokeStyle (cssStringHSLA color) ctx
    for_ fs.lineWidth $ \width -> Canvas.setLineWidth width ctx

  renderShape :: Shape -> Eff (canvas :: Canvas.CANVAS | eff) Unit
  renderShape (Path _ Nil) = pure unit
  renderShape (Path cl (Cons p rest)) = do
    Canvas.moveTo ctx p.x p.y
    for_ rest \p -> Canvas.lineTo ctx p.x p.y
    when cl $ void $ Canvas.closePath ctx
  renderShape (Rectangle r) = void $ Canvas.rect ctx { x: 0.0, y: 0.0, w: r.w, h: r.h }
  renderShape (Circle c) = void $ Canvas.arc ctx { x: c.r, y: c.r, r: c.r, start: 0.0, end: pi * 2.0 }
