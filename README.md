
# eas

## Overview

Package eas for [Einstein Aging
Study](https://einsteinmed.org/departments/neurology/clinical-research-program/eas/)

The EAS is a longitudinal study of community-residing individuals, aged
70 and older, in the Bronx, New York, a racially and ethnically diverse
urban setting. In May 2017, the EAS added the neuropsychological battery
of the Uniform Data Set (UDSNB 3.0) to the in-person assessment battery.
Below is the list of UDSNB 3.0 tests, the corresponding variables and
item numbers on the UDSNB 3.0 form. [Click here for the UDSNB3.0
form.](https://files.alz.washington.edu/documentation/uds3-ivp-c2.pdf)

  - Montreal Cognitive Assessment(MoCA)
    
    `mocascore`(1f)

  - Craft Story 21 Recall(Immediate)
    
    `verbatimi`(3a), `paraphrasei`(3b)

  - Benson Complex Figure Copy
    
    `bensonscorei`(4a)

  - Number Span Test: Forward
    
    `numspancorf`(5a)

  - Number Span Test: Background
    
    `numspancorb`(6a)

  - Category Fluency
    
    `animals60sec`(7a), `vegetables60sec`(7b)

  - Trail Making Test
    
    `tr_a1`(8a), `tr_b1`(8b)

  - Craft Story 21 Recall(Delayed)
    
    `verbatimd`(9a), `paraphrased`(9b)

  - Benson Complex Figure Recall
    
    `bensonscored`(10a)

  - Multilingual Naming Test (MINT)
    
    `minttotal`(11a)

  - Verbal Fluency: Phonemic Test
    
    `fwords60sec`(12a), `lwords60sec`(12d), `flword`(12g)

Current package provides a tool to calculate demographically adjusted
z-scores and impairment indicators for UDS3 neuropsychological tests.
You can specify if you want to use the norms from Einstein Aging
Study(EAS) or National Alzheimer’s Coordinating Center(NACC).

  - EAS norms

obtained from 225 cognitively normal older adults in EAS, use
`?norms_coef_eas` for the documentation.

  - NACC norms

obtaied from comparable data of 5,031 participants in the NACC database,
use `?norms_coef_nacc` for the documentation.

## Installation

``` r
install.packages("devtools")
devtools::install_github("JiyueQin/eas")
```

## Usage

``` r
library(eas)
library(tidyverse)
# here is a sample hypothetical dataset used to calculate z-scores. You should prepare your dataset in this standard format.
head(sample_dat)
```

    ## # A tibble: 6 x 21
    ##   female   age educyrs black_race mocascore verbatimi paraphrasei verbatimd
    ##    <dbl> <dbl>   <dbl>      <dbl>     <dbl>     <dbl>       <dbl>     <dbl>
    ## 1      1  74.4      19          0        18        21          17        26
    ## 2      0  82.2      18          0        17        12          19        29
    ## 3      1  84.2      16          0        25        21          22        16
    ## 4      1  84.3      14          1        26        28          20        12
    ## 5      0  76.5      20          0        17        16          23         9
    ##   paraphrased bensonscorei bensonscored numspancorf numspancorb minttotal
    ##         <dbl>        <dbl>        <dbl>       <dbl>       <dbl>     <dbl>
    ## 1          15           15           11          10           8        29
    ## 2          10           16           10           4           5        28
    ## 3          15           13            9           9           6        27
    ## 4          14           16           11           9           7        28
    ## 5          15           13           10          13           8        28
    ##   fwords60sec lwords60sec flword animals60sec vegetables60sec tr_a1 tr_b1
    ##         <dbl>       <dbl>  <dbl>        <dbl>           <dbl> <dbl> <dbl>
    ## 1          12          18     29           28               7    39   105
    ## 2          14          11     42           19              14    58    69
    ## 3          12           9     30           10              20    37    51
    ## 4           9          19     11           16              13    43    77
    ## 5          15           7     29           22               4    33   139
    ## # ... with 1 more row

``` r
str(sample_dat)
```

    ## tibble[,21] [20 x 21] (S3: tbl_df/tbl/data.frame)
    ##  $ female         : num [1:20] 1 0 1 1 0 1 1 1 0 1 ...
    ##  $ age            : num [1:20] 74.4 82.2 84.2 84.3 76.5 ...
    ##  $ educyrs        : num [1:20] 19 18 16 14 20 6 19 15 12 18 ...
    ##  $ black_race     : num [1:20] 0 0 0 1 0 1 0 0 1 0 ...
    ##  $ mocascore      : num [1:20] 18 17 25 26 17 25 27 26 25 23 ...
    ##  $ verbatimi      : num [1:20] 21 12 21 28 16 19 29 17 18 19 ...
    ##  $ paraphrasei    : num [1:20] 17 19 22 20 23 22 16 18 12 9 ...
    ##  $ verbatimd      : num [1:20] 26 29 16 12 9 21 17 16 3 19 ...
    ##  $ paraphrased    : num [1:20] 15 10 15 14 15 4 19 10 21 17 ...
    ##  $ bensonscorei   : num [1:20] 15 16 13 16 13 14 16 14 15 14 ...
    ##  $ bensonscored   : num [1:20] 11 10 9 11 10 8 8 10 9 12 ...
    ##  $ numspancorf    : num [1:20] 10 4 9 9 13 7 7 11 8 5 ...
    ##  $ numspancorb    : num [1:20] 8 5 6 7 8 8 7 3 10 6 ...
    ##  $ minttotal      : num [1:20] 29 28 27 28 28 25 32 32 32 28 ...
    ##  $ fwords60sec    : num [1:20] 12 14 12 9 15 12 17 11 10 14 ...
    ##  $ lwords60sec    : num [1:20] 18 11 9 19 7 14 14 12 13 13 ...
    ##  $ flword         : num [1:20] 29 42 30 11 29 20 27 27 36 25 ...
    ##  $ animals60sec   : num [1:20] 28 19 10 16 22 6 14 17 13 25 ...
    ##  $ vegetables60sec: num [1:20] 7 14 20 13 4 12 9 12 5 10 ...
    ##  $ tr_a1          : num [1:20] 39 58 37 43 33 44 51 28 51 49 ...
    ##  $ tr_b1          : num [1:20] 105 69 51 77 139 184 61 63 141 86 ...

``` r
# only include three test variables for easier display
sample_dat_small = sample_dat %>% select(female:black_race, minttotal, tr_a1, tr_b1)

# calculate z-scores and the impairment indicators for tr_a1 and tr_b1 with NACC norms and 1.5 SD to define impairment.
uds_z(sample_dat_small, c('tr_a1','tr_b1'), norms = 'nacc', impair_sd = 1.5)
```

    ## # A tibble: 20 x 11
    ##   female   age educyrs black_race minttotal tr_a1 tr_b1 z_tr_a1
    ##    <dbl> <dbl>   <dbl>      <dbl>     <dbl> <dbl> <dbl>   <dbl>
    ## 1      1  74.4      19          0        29    39   105 -0.796 
    ## 2      0  82.2      18          0        28    58    69 -1.75  
    ## 3      1  84.2      16          0        27    37    51 -0.0257
    ## 4      1  84.3      14          1        28    43    77  0.431 
    ## 5      0  76.5      20          0        28    33   139 -0.212 
    ##   impair_1.5sd_tr_a1 z_tr_b1 impair_1.5sd_tr_b1
    ##                <dbl>   <dbl>              <dbl>
    ## 1                  0  -0.786                  0
    ## 2                  1   0.511                  0
    ## 3                  0   1.09                   0
    ## 4                  0   1.61                   0
    ## 5                  0  -1.45                   0
    ## # ... with 15 more rows

``` r
# calculate the z-score and the impaiment indicator for minttotal with EAS norms and 1SD to define impairment
# Also output mean and sd estimates in addition to the z-scores and the impairment indicators.
uds_z(sample_dat_small, 'minttotal', norms = 'eas', impair_sd = 1, out_mean_sd  = T)
```

    ## # A tibble: 20 x 11
    ##   female   age educyrs black_race minttotal tr_a1 tr_b1 sd_minttotal
    ##    <dbl> <dbl>   <dbl>      <dbl>     <dbl> <dbl> <dbl>        <dbl>
    ## 1      1  74.4      19          0        29    39   105         3.90
    ## 2      0  82.2      18          0        28    58    69         3.90
    ## 3      1  84.2      16          0        27    37    51         3.90
    ## 4      1  84.3      14          1        28    43    77         3.90
    ## 5      0  76.5      20          0        28    33   139         3.90
    ##   mean_minttotal z_minttotal impair_1sd_minttotal
    ##            <dbl>       <dbl>                <dbl>
    ## 1           29.7     -0.172                     0
    ## 2           28.2     -0.0473                    0
    ## 3           27.8     -0.199                     0
    ## 4           26.0      0.524                     0
    ## 5           29.4     -0.350                     0
    ## # ... with 15 more rows
