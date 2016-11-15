## Module Graphics.Drawing.Color

This module defines preset colors, and functions for creating colors.

#### `Color`

``` purescript
data Color
```

Colors.

##### Instances
``` purescript
instance eqColor :: Eq Color
```

#### `rgb`

``` purescript
rgb :: Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 255.0.

#### `hsl`

``` purescript
hsl :: Number -> Number -> Number -> Color
```

Create a `Color` from hue (0.0-360.0), saturation (0.0-1) and lightness (0.0-1) values.

#### `lighten`

``` purescript
lighten :: Number -> Color -> Color
```

Lighten a color by the specified amount between 0 and 1.

#### `darken`

``` purescript
darken :: Number -> Color -> Color
```

Darken a color by the specified amount between 0 and 1.

#### `colorString`

``` purescript
colorString :: Color -> String
```

Render a color as a HTML color string.

#### `white`

``` purescript
white :: Color
```

#### `silver`

``` purescript
silver :: Color
```

#### `gray`

``` purescript
gray :: Color
```

#### `black`

``` purescript
black :: Color
```

#### `red`

``` purescript
red :: Color
```

#### `maroon`

``` purescript
maroon :: Color
```

#### `yellow`

``` purescript
yellow :: Color
```

#### `olive`

``` purescript
olive :: Color
```

#### `lime`

``` purescript
lime :: Color
```

#### `green`

``` purescript
green :: Color
```

#### `aqua`

``` purescript
aqua :: Color
```

#### `teal`

``` purescript
teal :: Color
```

#### `blue`

``` purescript
blue :: Color
```

#### `navy`

``` purescript
navy :: Color
```

#### `fuchsia`

``` purescript
fuchsia :: Color
```

#### `purple`

``` purescript
purple :: Color
```


