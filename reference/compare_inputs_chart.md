# Generate heat map of changed values between two run inputs.

Can be a very busy chart if not filtered down. Consider using a filter
on the dataframe before piping into `compare_input_chart`.

## Usage

``` r
compare_inputs_chart(.data)
```

## Arguments

- .data:

  Dataframe origination from the compare_inputs() function

## See also

[`compare_inputs()`](https://framverse.github.io/framrsquared/reference/compare_inputs.md)

## Examples

``` r
if (FALSE) fram_db |> compare_inputs(c(100, 101)) |> compare_inputs_chart() # \dontrun{}
```
