# Print Method for htna Path/Pattern Objects

Shared print method for objects returned by
[`extract_meta_paths()`](https://sonsoles.me/htna/reference/extract_meta_paths.md)
(class `htna_meta_paths`). Dispatches on the `htna_paths` parent class.

## Usage

``` r
# S3 method for class 'htna_paths'
print(x, n = 10L, ...)
```

## Arguments

- x:

  An `htna_paths` object.

- n:

  Maximum number of rows to print. Default `10`.

- ...:

  Unused.

## Value

The input object, invisibly.
