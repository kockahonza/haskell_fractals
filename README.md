# Fractals
A program written in haskell that renders fractal images.
It is a stack project to run install [stack](https://docs.haskellstack.org/en/stable/README) and run `stack run` in the root directory of this git repository.

## Some explanation of the code structure
There are 3 new types defined, `Fractal`, `Func` and `Coloring`
This is done the function that defines a fractal image can be easily rendered with different ways of coloring it.
### `Fractal`
This defines a fractal which can then be rendered.
It has 5 parts:
- name (String) which will be used to name the output file.
- xRange and yRange ((a, a, a)) which describe the range in which the fractal should be rendered.
- func (Func a b) that defines a mapping of x, y values to some output
- coloring (Coloring b) which then maps the output to a color.
### `Func`
This is a type synonym for (a -> a -> b).
### `Coloring`
This is a type synonym for (b -> PixelRGB8).
PixelRGB8 is the type which represents a color of a pixel in the Graphics library I am using.

## Defined Fractals
There are two defined functions which will generate a `Func` based on some arguments.
And there are also two defined functions which will generate a `Coloring`.
### Funcs
#### `getJuliaNumFunc`
This expects one complex number `c` and one integer `n`.
It then returns the result of doing `z = z^2 + c` `n` times where the first `z` is `c`.
#### `getJuliaStepsFunc`
This expects one complex number `c` and one real number `m`.
It then returns an integer describing how many steps described in `getJuliaNumFunc` are necessary for the result to reach `m`.
### Colorings
#### `getRedLinColoring`
This expects a number `m` and it will then return a `Coloring` which upon receiving a number will return a shade of red that corresponds to the ratio of the number and `m`.
It should also be said that the shades loop thanks to how the graphics library works.
#### `getColorListColoring`
This expects a list of PixelRGB8 `colors` and returns a `Coloring` taking integers.
It the returns the nth color where n is the remainder of dividing the input by the length of `colors`.

heyoo
awd
d
