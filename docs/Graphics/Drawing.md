## Module Graphics.Drawing

This module defines a type `Drawing` for creating vector graphics.

#### `Point`

``` purescript
type Point = { x :: Number, y :: Number }
```

A `Point` consists of `x` and `y` coordinates.

#### `Shape`

``` purescript
data Shape
```

A single shape.

##### Instances
``` purescript
Eq Shape
```

#### `path`

``` purescript
path :: forall f. Foldable f => f Point -> Shape
```

Create a path.

#### `closed`

``` purescript
closed :: forall f. Foldable f => f Point -> Shape
```

Create a _closed_ path.

#### `rectangle`

``` purescript
rectangle :: Number -> Number -> Shape
```

Create a rectangle from the width and height parameters.

#### `circle`

``` purescript
circle :: Number -> Shape
```

Create a circle from its radius.

#### `FillStyle`

``` purescript
newtype FillStyle
```

Encapsulates fill color etc.

##### Instances
``` purescript
Semigroup FillStyle
Monoid FillStyle
Eq FillStyle
```

#### `fillColor`

``` purescript
fillColor :: Color -> FillStyle
```

Set the fill color.

#### `OutlineStyle`

``` purescript
newtype OutlineStyle
```

Encapsulates outline color etc.

##### Instances
``` purescript
Semigroup OutlineStyle
Monoid OutlineStyle
Eq OutlineStyle
```

#### `outlineColor`

``` purescript
outlineColor :: Color -> OutlineStyle
```

Set the outline color.

#### `lineWidth`

``` purescript
lineWidth :: Number -> OutlineStyle
```

Set the line width.

#### `Shadow`

``` purescript
newtype Shadow
```

Encapsulates shadow settings etc.

##### Instances
``` purescript
Eq Shadow
Semigroup Shadow
Monoid Shadow
```

#### `shadowColor`

``` purescript
shadowColor :: Color -> Shadow
```

Set the shadow color.

#### `shadowBlur`

``` purescript
shadowBlur :: Number -> Shadow
```

Set the shadow blur.

#### `shadowOffset`

``` purescript
shadowOffset :: Number -> Number -> Shadow
```

Set the shadow blur.

#### `Drawing`

``` purescript
data Drawing
```

A vector `Drawing`.

##### Instances
``` purescript
Semigroup Drawing
Monoid Drawing
Eq Drawing
```

#### `filled`

``` purescript
filled :: FillStyle -> Shape -> Drawing
```

Fill a `Shape`.

#### `outlined`

``` purescript
outlined :: OutlineStyle -> Shape -> Drawing
```

Draw the outline of a `Shape`.

#### `clipped`

``` purescript
clipped :: Shape -> Drawing -> Drawing
```

Clip a `Drawing` to a `Shape`.

#### `shadow`

``` purescript
shadow :: Shadow -> Drawing -> Drawing
```

Apply a `Shadow` to a `Drawing`.

#### `scale`

``` purescript
scale :: Number -> Number -> Drawing -> Drawing
```

Apply a scale transformation by providing the x and y scale factors.

#### `translate`

``` purescript
translate :: Number -> Number -> Drawing -> Drawing
```

Apply a translation by providing the x and y distances.

#### `rotate`

``` purescript
rotate :: Number -> Drawing -> Drawing
```

Apply a rotation by providing the angle.

#### `text`

``` purescript
text :: Font -> Number -> Number -> FillStyle -> String -> Drawing
```

Render some text.

#### `above`

``` purescript
above :: Drawing -> Drawing -> Drawing
```

Place a Drawing above another.

#### `beside`

``` purescript
beside :: Drawing -> Drawing -> Drawing
```

Place a Drawing beside another.

#### `everywhere`

``` purescript
everywhere :: (Drawing -> Drawing) -> Drawing -> Drawing
```

Modify a `Drawing` by applying a transformation to every subdrawing.

#### `render`

``` purescript
render :: forall eff. Context2D -> Drawing -> Eff (canvas :: CANVAS | eff) Unit
```

Render a `Drawing` to a canvas.


### Re-exported from Color:

#### `ColorSpace`

``` purescript
data ColorSpace
  = RGB
  | HSL
  | LCh
  | Lab
```

Definition of a color space.

* `RGB`: red, green, blue
* `HSL`: hue, saturation, lightness
* `LCh`: Lightness, chroma, hue
* `Lab`: Lightness, a, b

#### `Color`

``` purescript
data Color
```

The representation of a color.

Note:
- The `Eq` instance compares two `Color`s by comparing their (integer) RGB
  values. This is different from comparing the HSL values (for example,
  HSL has many different representations of black (arbitrary hue and
  saturation values).
- Colors outside the sRGB gamut which cannot be displayed on a typical
  computer screen can not be represented by `Color`.


##### Instances
``` purescript
Show Color
Eq Color
```

#### `xyz`

``` purescript
xyz :: Number -> Number -> Number -> Color
```

Create a `Color` from XYZ coordinates in the CIE 1931 color space. Note
that a `Color` always represents a color in the sRGB gamut (colors that
can be represented on a typical computer screen) while the XYZ color space
is bigger. This function will tend to create fully saturated colors at the
edge of the sRGB gamut if the coordinates lie outside the sRGB range.

See:
- https://en.wikipedia.org/wiki/CIE_1931_color_space
- https://en.wikipedia.org/wiki/SRGB

#### `white`

``` purescript
white :: Color
```

Pure white.

#### `toXYZ`

``` purescript
toXYZ :: Color -> { x :: Number, y :: Number, z :: Number }
```

Get XYZ coordinates according to the CIE 1931 color space.

See:
- https://en.wikipedia.org/wiki/CIE_1931_color_space
- https://en.wikipedia.org/wiki/SRGB

#### `toRGBA'`

``` purescript
toRGBA' :: Color -> { r :: Number, g :: Number, b :: Number, a :: Number }
```

Convert a `Color` to its red, green, blue and alpha values. All values
are numbers in the range from 0.0 to 1.0.

#### `toRGBA`

``` purescript
toRGBA :: Color -> { r :: Int, g :: Int, b :: Int, a :: Number }
```

Convert a `Color` to its red, green, blue and alpha values. The RGB values
are integers in the range from 0 to 255. The alpha channel is a number
between 0.0 and 1.0.

#### `toLab`

``` purescript
toLab :: Color -> { l :: Number, a :: Number, b :: Number }
```

Get L, a and b coordinates according to the Lab color space.

See: https://en.wikipedia.org/wiki/Lab_color_space

#### `toLCh`

``` purescript
toLCh :: Color -> { l :: Number, c :: Number, h :: Number }
```

Get L, C and h coordinates according to the CIE LCh color space.

See: https://en.wikipedia.org/wiki/Lab_color_space

#### `toHexString`

``` purescript
toHexString :: Color -> String
```

Return a hexadecimal representation of the color in the form `#rrggbb`,
where `rr`, `gg` and `bb` refer to hexadecimal digits corresponding to
the RGB channel values between `00` and `ff`. The alpha channel is not
represented.

#### `toHSLA`

``` purescript
toHSLA :: Color -> { h :: Number, s :: Number, l :: Number, a :: Number }
```

Convert a `Color` to its hue, saturation, lightness and alpha values. See
`hsla` for the ranges of each channel.

#### `toGray`

``` purescript
toGray :: Color -> Color
```

Convert a color to a gray tone with the same perceived luminance (see
`luminance`).

#### `textColor`

``` purescript
textColor :: Color -> Color
```

Return a readable foreground text color (either `black` or `white`) for a
given background color.

#### `saturate`

``` purescript
saturate :: Number -> Color -> Color
```

Increase the saturation of a color by adding a certain amount (number
between -1.0 and 1.0) to the saturation channel. If the number is
negative, the color is desaturated.

#### `rotateHue`

``` purescript
rotateHue :: Number -> Color -> Color
```

Rotate the hue of a `Color` by a certain angle (in degrees).

#### `rgba'`

``` purescript
rgba' :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from RGB and alpha values between 0.0 and 1.0.

#### `rgba`

``` purescript
rgba :: Int -> Int -> Int -> Number -> Color
```

Create a `Color` from integer RGB values between 0 and 255 and a floating
point alpha value between 0.0 and 1.0.

#### `rgb'`

``` purescript
rgb' :: Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 1.0.

#### `rgb`

``` purescript
rgb :: Int -> Int -> Int -> Color
```

Create a `Color` from integer RGB values between 0 and 255.

#### `mix`

``` purescript
mix :: ColorSpace -> Color -> Color -> Number -> Color
```

Mix two colors by linearly interpolating between them in the specified
color space. For the HSL colorspace, the shortest path is chosen along the
circle of hue values.

#### `luminance`

``` purescript
luminance :: Color -> Number
```

The relative brightness of a color (normalized to 0.0 for darkest black
and 1.0 for lightest white), according to the WCAG definition.

See: https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef

#### `lighten`

``` purescript
lighten :: Number -> Color -> Color
```

Lighten a color by adding a certain amount (number between -1.0 and 1.0)
to the lightness channel. If the number is negative, the color is
darkened.

#### `lch`

``` purescript
lch :: Number -> Number -> Number -> Color
```

Create a `Color` from lightness, chroma and hue coordinates in the CIE LCh
color space. This is a cylindrical transform of the Lab color space.
Note: See documentation for `xyz`. The same restrictions apply here.

See: https://en.wikipedia.org/wiki/Lab_color_space

#### `lab`

``` purescript
lab :: Number -> Number -> Number -> Color
```

Create a `Color` from L, a and b coordinates coordinates in the Lab color
space.
Note: See documentation for `xyz`. The same restrictions apply here.

See: https://en.wikipedia.org/wiki/Lab_color_space

#### `isReadable`

``` purescript
isReadable :: Color -> Color -> Boolean
```

Determine whether text of one color is readable on a background of a
different color (see `contrast`). This function is symmetric in both
arguments.

``` purs
isReadable c1 c2 = contrast c1 c2 > 4.5
```

#### `isLight`

``` purescript
isLight :: Color -> Boolean
```

Determine whether a color is perceived as a light color.

``` purs
isLight c = brightness c > 0.5
```

#### `hsla`

``` purescript
hsla :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from hue, saturation, lightness and alpha values. The
hue is given in degrees, as a `Number` between 0.0 and 360.0. Saturation,
lightness and alpha are numbers between 0.0 and 1.0.

#### `hsl`

``` purescript
hsl :: Number -> Number -> Number -> Color
```

Create a `Color` from hue, saturation and lightness values. The hue is
given in degrees, as a `Number` between 0.0 and 360.0. Both saturation and
lightness are numbers between 0.0 and 1.0.

#### `graytone`

``` purescript
graytone :: Number -> Color
```

Create a gray tone from a lightness values (0.0 is black, 1.0 is white).

#### `fromInt`

``` purescript
fromInt :: Int -> Color
```

Converts an integer to a color (RGB representation). `0` is black and
`0xffffff` is white. Values outside this range will be clamped.

This function is useful if you want to hard-code Hex values. For example:
``` purs
red = fromInt 0xff0000
```

#### `fromHexString`

``` purescript
fromHexString :: String -> Maybe Color
```

Parse a hexadecimal RGB code of the form `#rgb` or `#rrggbb`. The `#`
character is required. Each hexadecimal digit is of the form `[0-9a-fA-F]`
(case insensitive). Returns `Nothing` if the string is in a wrong format.

#### `distance`

``` purescript
distance :: Color -> Color -> Number
```

Compute the perceived 'distance' between two colors according to the CIE76
delta-E standard. A distance below ~2.3 is not noticable.

See: https://en.wikipedia.org/wiki/Color_difference

#### `desaturate`

``` purescript
desaturate :: Number -> Color -> Color
```

Decrease the saturation of a color by subtracting a certain amount (number
between -1.0 and 1.0) from the saturation channel. If the number is
negative, the color is saturated.

#### `darken`

``` purescript
darken :: Number -> Color -> Color
```

Darken a color by subtracting a certain amount (number between -1.0 and
1.0) from the lightness channel. If the number is negative, the color is
lightened.

#### `cssStringRGBA`

``` purescript
cssStringRGBA :: Color -> String
```

A CSS representation of the color in the form `rgb(..)` or `rgba(...)`.

#### `cssStringHSLA`

``` purescript
cssStringHSLA :: Color -> String
```

A CSS representation of the color in the form `hsl(..)` or `hsla(...)`.

#### `contrast`

``` purescript
contrast :: Color -> Color -> Number
```

The contrast ratio of two colors. A minimum contrast ratio of 4.5 is
recommended to ensure that text is readable on a colored background. The
contrast ratio is symmetric on both arguments:
`contrast c1 c2 == contrast c2 c1`.

See http://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef

#### `complementary`

``` purescript
complementary :: Color -> Color
```

Get the complementary color (hue rotated by 180°).

#### `brightness`

``` purescript
brightness :: Color -> Number
```

The percieved brightness of the color (A number between 0.0 and 1.0).

See: https://www.w3.org/TR/AERT#color-contrast

#### `black`

``` purescript
black :: Color
```

Pure black.

### Re-exported from Graphics.Drawing.Font:

#### `Font`

``` purescript
data Font
```

Fonts.

##### Instances
``` purescript
Eq Font
```

#### `fontString`

``` purescript
fontString :: Font -> String
```

Turn a `Font` into a `String` which can be used with `Graphics.Canvas.setFont`.

