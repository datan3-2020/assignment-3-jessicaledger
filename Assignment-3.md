Statistical assignment 3
================
Jessica Ledger – 660013603
11/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)
library(dplyr)
library(reshape2)
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.
# create a vector with the file names and paths
files <- dir(
             # Select the folder where the files are stored.
             "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab/",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)
# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w1/a_indresp.tab"
    ## [2] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w2/b_indresp.tab"
    ## [3] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w3/c_indresp.tab"
    ## [4] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w4/d_indresp.tab"
    ## [5] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w5/e_indresp.tab"
    ## [6] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w6/f_indresp.tab"
    ## [7] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w7/g_indresp.tab"
    ## [8] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w8/h_indresp.tab"
    ## [9] "/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab//ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")
for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
   pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>%
   separate(variable, into = c("wave","variable"), sep = "_", extra = "merge") %>%
   pivot_wider(names_from = variable, values_from = value)
Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # … with 584,224 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were
correct.

``` r
# Have to create an extra variable for sex as otherwise mutate produces NA in the sex_dv colomn

Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex = case_when(sex_dv == 1 ~ 'Male', sex_dv == 2 ~ 'Female') ) %>%
        mutate (vote6 = ifelse(vote6 > 0, vote6, NA)) %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(vote6))
      
    
Long %>% count(sex)
```

    ## # A tibble: 2 x 2
    ##   sex         n
    ##   <chr>   <int>
    ## 1 Female 112254
    ## 2 Male    88629

``` r
Long %>% count(vote6)
```

    ## # A tibble: 4 x 2
    ##   vote6     n
    ##   <int> <int>
    ## 1     1 21660
    ## 2     2 70952
    ## 3     3 56133
    ## 4     4 52138

## Calculate mean political interest by sex and wave (10 points)

Political interest (vote6) is an ordinal variable, but we will treat it
as interval and calculate mean political interest for men and women in
each wave.

``` r
meanVote6 <- Long %>% 
  group_by(sex, wave) %>%
  summarise(MeanPolInt = mean(vote6, na.rm = TRUE))
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   sex [2]
    ##    sex    wave  MeanPolInt
    ##    <chr>  <chr>      <dbl>
    ##  1 Female a           2.84
    ##  2 Female b           2.82
    ##  3 Female c           2.87
    ##  4 Female d           2.89
    ##  5 Female e           2.87
    ##  6 Female f           2.81
    ##  7 Female g           2.73
    ##  8 Male   a           2.53
    ##  9 Male   b           2.51
    ## 10 Male   c           2.54
    ## 11 Male   d           2.55
    ## 12 Male   e           2.51
    ## 13 Male   f           2.47
    ## 14 Male   g           2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
Wide <- meanVote6 %>%
  pivot_wider(names_from = wave, values_from = MeanPolInt) %>%
filter(!is.na(sex))
Wide
```

    ## # A tibble: 2 x 8
    ## # Groups:   sex [2]
    ##   sex        a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 Male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

Men on average are slightly more interested in politics than women. Both
men and women’s interest in politics appears to increase from wave 1-4
then decreased after this.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
all7NA <- all7 %>%
  na.omit()

Long2 <- Long %>%
        group_by(pidp) %>%
        mutate(wave.no = length(wave)) %>%
        filter(wave.no == 7) %>%
        filter(!is.na(vote6)) %>%
        filter(!is.na(sex_dv))
Long2
```

    ## # A tibble: 103,740 x 8
    ## # Groups:   pidp [14,820]
    ##        pidp wave  memorig sex_dv age_dv vote6 sex    wave.no
    ##       <int> <chr>   <int>  <int>  <int> <int> <chr>    <int>
    ##  1 68004087 a           1      1     59     2 Male         7
    ##  2 68004087 b           1      1     60     2 Male         7
    ##  3 68004087 c           1      1     61     2 Male         7
    ##  4 68004087 d           1      1     62     1 Male         7
    ##  5 68004087 e           1      1     63     2 Male         7
    ##  6 68004087 f           1      1     64     2 Male         7
    ##  7 68004087 g           1      1     65     2 Male         7
    ##  8 68006127 a           1      2     39     4 Female       7
    ##  9 68006127 b           1      2     40     4 Female       7
    ## 10 68006127 c           1      2     41     4 Female       7
    ## # … with 103,730 more rows

2.  Calculate Delta for each person in the data set.

<!-- end list -->

``` r
Delta <- Long2 %>%
        group_by(pidp) %>%
        summarise(Delta = sum(abs(vote6[wave == "a"] - vote6[wave == "b"]), abs(vote6[wave == "b"] - vote6[wave == "c"]), abs(vote6[wave == "c"] - vote6[wave == "d"]), abs(vote6[wave == "d"] - vote6[wave == "e"]), abs(vote6[wave == "e"] - vote6[wave == "f"]), abs(vote6[wave == "f"] - vote6[wave == "g"])))

Delta
```

    ## # A tibble: 14,820 x 2
    ##        pidp Delta
    ##       <int> <int>
    ##  1 68004087     2
    ##  2 68006127     0
    ##  3 68006807     1
    ##  4 68008847     6
    ##  5 68009527     2
    ##  6 68010887     6
    ##  7 68020407     4
    ##  8 68025847     8
    ##  9 68029927     5
    ## 10 68031967     4
    ## # … with 14,810 more rows

``` r
Table <- Long2 %>%
        left_join(Delta, by = "pidp") 
Table
```

    ## # A tibble: 103,740 x 9
    ## # Groups:   pidp [14,820]
    ##        pidp wave  memorig sex_dv age_dv vote6 sex    wave.no Delta
    ##       <int> <chr>   <int>  <int>  <int> <int> <chr>    <int> <int>
    ##  1 68004087 a           1      1     59     2 Male         7     2
    ##  2 68004087 b           1      1     60     2 Male         7     2
    ##  3 68004087 c           1      1     61     2 Male         7     2
    ##  4 68004087 d           1      1     62     1 Male         7     2
    ##  5 68004087 e           1      1     63     2 Male         7     2
    ##  6 68004087 f           1      1     64     2 Male         7     2
    ##  7 68004087 g           1      1     65     2 Male         7     2
    ##  8 68006127 a           1      2     39     4 Female       7     0
    ##  9 68006127 b           1      2     40     4 Female       7     0
    ## 10 68006127 c           1      2     41     4 Female       7     0
    ## # … with 103,730 more rows

3.  Calculate mean Delta for men and women.

<!-- end list -->

``` r
MeanDel <- Table %>%
        group_by(sex) %>%
        summarise(MeanDel =
                          mean(Delta, na.rm = TRUE))
MeanDel
```

    ## # A tibble: 2 x 2
    ##   sex    MeanDel
    ##   <chr>    <dbl>
    ## 1 Female    2.49
    ## 2 Male      2.53

4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.

<!-- end list -->

``` r
Delta1 <- Table %>%
        group_by(age_dv) %>%
        summarise(AvgDelA =
                          mean(Delta[wave == "a"]))
Delta1
```

    ## # A tibble: 87 x 2
    ##    age_dv AvgDelA
    ##     <int>   <dbl>
    ##  1     15    2   
    ##  2     16    2.78
    ##  3     17    2.55
    ##  4     18    2.83
    ##  5     19    2.82
    ##  6     20    2.39
    ##  7     21    3.08
    ##  8     22    2.59
    ##  9     23    2.42
    ## 10     24    2.15
    ## # … with 77 more rows

4.  GRAPH

<!-- end list -->

``` r
scatter.smooth(Delta1,
               xlab = "Age (at wave 1)",
               ylab = "Mean Delta",
               main = "Change in Political Interest with Age")
```

![](Assignment-3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
?scatter.smooth
```

5.  Write a short interpretation of your findings.

The above graph indicates that levels of political interest (Delta) are
less stable between the ages 15-40 and 60-100. Political interest tends
to become more stable between 40-60. It must be noted that this is from
ages from the first wave, we are now 5 years on. Furthermore there are
some outliers but this may be due to there being less people in the
older age sections (80-100)
