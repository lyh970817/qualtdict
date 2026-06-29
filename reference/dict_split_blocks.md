# Split a Variable Dictionary by Survey Block

Create block-specific views of a Variable Dictionary without changing
the return type of
[`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

## Usage

``` r
dict_split_blocks(dict)
```

## Arguments

- dict:

  A Variable Dictionary returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

## Value

A named list of Variable Dictionaries, one per Survey Block.

## Examples

``` r
dict <- data.frame(
  response_column_id = c("QID1", "QID2"),
  variable_name = c("q1", "q2"),
  block = c("Block A", "Block B")
)
class(dict) <- c("qualtdict", class(dict))

dict_split_blocks(dict)
#> $`Block A`
#>   response_column_id variable_name   block
#> 1               QID1            q1 Block A
#> 
#> $`Block B`
#>   response_column_id variable_name   block
#> 2               QID2            q2 Block B
#> 
```
