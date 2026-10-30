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

  A Variable Dictionary: a `qualtdict` object (a tibble with class
  `qualtdict`) as returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

## Value

A named list of Variable Dictionaries, one per Survey Block. When rows
have no Survey Block assignment, the list includes `"..unassigned"`.

## Examples

``` r
dict_blocks <- dict_split_blocks(glad_example$dictionary)
names(dict_blocks)
#> [1] "..unassigned" "Demographics" "GAD7"         "PHQ9"        
dict_blocks$GAD7
#> # A tibble: 32 × 14
#>    response_column_id row_source qid          question_name variable_name block
#>    <chr>              <chr>      <chr>        <chr>         <chr>         <chr>
#>  1 QID127303430_1     question   QID127303430 HEAD          HEAD          GAD7 
#>  2 QID127303430_1     question   QID127303430 HEAD          HEAD          GAD7 
#>  3 QID127303430_1     question   QID127303430 HEAD          HEAD          GAD7 
#>  4 QID127303430_1     question   QID127303430 HEAD          HEAD          GAD7 
#>  5 QID127303430_2     question   QID127303430 HEAD          HEAD.1        GAD7 
#>  6 QID127303430_2     question   QID127303430 HEAD          HEAD.1        GAD7 
#>  7 QID127303430_2     question   QID127303430 HEAD          HEAD.1        GAD7 
#>  8 QID127303430_2     question   QID127303430 HEAD          HEAD.1        GAD7 
#>  9 QID127303430_3     question   QID127303430 HEAD          HEAD.2        GAD7 
#> 10 QID127303430_3     question   QID127303430 HEAD          HEAD.2        GAD7 
#> # ℹ 22 more rows
#> # ℹ 8 more variables: question <chr>, item <chr>, level <chr>, label <chr>,
#> #   type <chr>, selector <chr>, sub_selector <chr>, content_type <chr>
```
