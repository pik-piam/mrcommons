# calcSeed

Calculates Seed demand

## Usage

``` r
calcSeed(
  cellular = FALSE,
  cells = "lpjcell",
  products = "kall",
  irrigation = FALSE,
  attributes = "all"
)
```

## Arguments

- cellular:

  cellular or regional level

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- products:

  kcr or also kall, which includes seeds for eggs and fish

- irrigation:

  if TRUE, distinguishes irrigated and non-irrigated crops

- attributes:

  in dm, wm, ge, nr, p, k

## Value

List of magpie object with results and weight on country or cellular
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Seed")
} # }
```
