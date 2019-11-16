# Fractals
A program written in haskell that renders fractal images.
It is a stack project to run install [stack](https://docs.haskellstack.org/en/stable/README) and run `stack run` in the root directory of this git repository.

## Some explanation of te code
There are 3 new types defined, `Fractal`, `Func` and `Coloring`
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
