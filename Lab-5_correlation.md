Geog4/6300: Lab 5
================

Name: Emma Benedict

## Correlation and ANOVA

**Overview** In this lab, you’ll work with county level election and
census data to answer two questions: (1) how did association between the
presidential vote (measured by vote for the Republican candidate) and
educational attainment change between 2008 and 2016 and (2) Were there
regional differences in the vote for the Republican candidate across
years? To do so, you’ll need to prepare a dataset for analysis and run
chi-square, ANOVA, and post-hoc (TukeyHSD) tests.

Our dataset is drawn from the American Community Survey and [this Github
repository](https://github.com/tonmcg/US_County_Level_Election_Results_08-20)
giving the votes by county in 2008, 2012, and 2016. The ACS data are the
pooled five-year samples for 2006-10, 2010-14, and 2014-18 respectively.
Let’s load the data from the lab folder:

``` r
election_data<-read_csv("data/elections0816_demog_pct.csv")
```

    ## Rows: 9332 Columns: 61
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (5): gisjn_cty, county, state, region, division
    ## dbl (56): fips_code, year, total, dem, gop, oth, totpop, wht_pop, afam_pop, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The raw population counts and rates (percentages) are both included in
this dataset. You can see a description of the variables in the
setup/census_vars.csv variables.

### Part 1: Correlation analysis

In this section, you’ll be measuring the correlation between the gop_pct
(% voting for the Republican) variable and the five variables showing
*highest* level of educational attainment as a rate:

*ed_lesshs_pct: % of population with less than a high school diploma
*ed_hsdiploma_pct: % of population with a high school diploma/GED
*ed_lessba_pct: % of population with some college or an associate’s
degree *ed_ba_pct: % of population with a bachelor’s degree
\*ed_graddeg_pct: % of population with a professional/graduate degree

**Question 1** *To start with, you will need to subset the data to two
separate data frames, one for the 2008 election results and one for
2016. Create these two data frames using the `year_txt` variable and
select just the region variable (Census region) as well as our variables
of interest (GOP voting and educational attainment). Call the heads of
both using kable when you are done.*

``` r
elec_2008 <- election_data%>%
  filter(year == "2008")%>%
  select(region, gop_pct, ed_lesshs_pct:ed_graddeg_pct)
elec_2008_counties <- election_data%>%
  filter(year == "2008")%>%
  select(year, region, county, gop_pct, ed_lesshs_pct:ed_graddeg_pct)

elec_2016 <- election_data%>%
  filter(year == "2016")%>%
  select(region, gop_pct, ed_lesshs_pct:ed_graddeg_pct)
elec_2016_counties <- election_data%>%
  filter(year == "2016")%>%
  select(year, region, county, gop_pct, ed_lesshs_pct:ed_graddeg_pct)

kable(head(elec_2008))
```

| region | gop_pct | ed_lesshs_pct | ed_hsdiploma_pct | ed_lessba_pct | ed_ba_pct | ed_graddeg_pct |
|:---|---:|---:|---:|---:|---:|---:|
| Midwest Region | 45.96622 | 9.449469 | 38.37580 | 33.51972 | 18.655008 | 6.011934 |
| South Region | 87.02229 | 18.523949 | 30.00978 | 28.98338 | 22.482893 | 5.865103 |
| South Region | 72.32305 | 24.474929 | 35.68355 | 30.18575 | 9.655775 | 2.758488 |
| South Region | 46.96133 | 47.198599 | 29.06453 | 16.13307 | 7.603802 | 1.938469 |
| West Region | 72.03456 | 11.863922 | 34.04803 | 34.81990 | 19.268153 | 6.918239 |
| Midwest Region | 66.55443 | 13.123309 | 38.61616 | 31.94820 | 16.312331 | 3.111712 |

``` r
kable(head(elec_2016))
```

| region | gop_pct | ed_lesshs_pct | ed_hsdiploma_pct | ed_lessba_pct | ed_ba_pct | ed_graddeg_pct |
|:---|---:|---:|---:|---:|---:|---:|
| Midwest Region | 60.17220 | 6.069211 | 33.34469 | 39.09587 | 14.663789 | 6.826442 |
| South Region | 87.67020 | 17.846154 | 33.27473 | 29.40659 | 14.505494 | 4.967033 |
| South Region | 82.78220 | 19.135500 | 37.09921 | 32.19416 | 7.168586 | 4.402544 |
| South Region | 44.50377 | 36.136206 | 27.58860 | 25.49389 | 7.812965 | 2.968331 |
| West Region | 76.48915 | 5.578801 | 28.00558 | 43.29149 | 14.365411 | 8.758717 |
| Midwest Region | 77.27273 | 9.970385 | 36.01185 | 34.66930 | 14.037512 | 5.310958 |

**Question 2** *Next you should assess the normality of the variables of
interest. Use two of the three measures discussed in class (histogram,
QQ plot, and/or Shapiro-Wilk test) to do so for the 2016 data. Based on
your results, is a parametric or non-parametric versiob if correlation
more appropriate?*

``` r
shapiro.test(elec_2016$gop_pct)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_2016$gop_pct
    ## W = 0.95145, p-value < 2.2e-16

``` r
shapiro.test(elec_2016$ed_lesshs_pct)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_2016$ed_lesshs_pct
    ## W = 0.93472, p-value < 2.2e-16

``` r
shapiro.test(elec_2016$ed_hsdiploma_pct)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_2016$ed_hsdiploma_pct
    ## W = 0.99337, p-value = 1.006e-10

``` r
shapiro.test(elec_2016$ed_lessba_pct)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_2016$ed_lessba_pct
    ## W = 0.99848, p-value = 0.00516

``` r
shapiro.test(elec_2016$ed_ba_pct)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_2016$ed_ba_pct
    ## W = 0.94163, p-value < 2.2e-16

``` r
shapiro.test(elec_2016$ed_graddeg_pct)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_2016$ed_graddeg_pct
    ## W = 0.81124, p-value < 2.2e-16

``` r
ggplot(elec_2016,aes(x=gop_pct))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(elec_2016,aes(x=ed_lesshs_pct))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggplot(elec_2016,aes(x=ed_hsdiploma_pct))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
ggplot(elec_2016,aes(x=ed_lessba_pct))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
ggplot(elec_2016,aes(x=ed_ba_pct))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
ggplot(elec_2016,aes(x=ed_graddeg_pct))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

``` r
ggplot(elec_2016, aes(sample=gop_pct))+
  stat_qq()+
  stat_qq_line()
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-7.png)<!-- -->

``` r
ggplot(elec_2016, aes(sample=ed_lesshs_pct))+
  stat_qq()+
  stat_qq_line()
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-8.png)<!-- -->

``` r
ggplot(elec_2016, aes(sample=ed_hsdiploma_pct))+
  stat_qq()+
  stat_qq_line()
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-9.png)<!-- -->

``` r
ggplot(elec_2016, aes(sample=ed_lessba_pct))+
  stat_qq()+
  stat_qq_line()
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-10.png)<!-- -->

``` r
ggplot(elec_2016, aes(sample=ed_ba_pct))+
  stat_qq()+
  stat_qq_line()
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-11.png)<!-- -->

``` r
ggplot(elec_2016, aes(sample=ed_graddeg_pct))+
  stat_qq()+
  stat_qq_line()
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-3-12.png)<!-- -->

{Based on the analysis of the variables I did using both the
Shapiro-Wilk Test and histograms, I think that a non-parametric
correlation is more appropriate. This is because all of the variables
are not normally distributed. You can see this first when looking at the
Shapiro-Wilk Test with the gop_pct having a p-value of 2.2e-16,
ed_lesshs_pct having a p-value of 2.2e-16, ed_hsdiploma_pct having a
p-value of 1.006e-10, ed_lessba_pct having a p-value of 0.00516 (not as
non normal as the others but still significant), ed_ba_pct having a
p-value of 2.2e-16, and ed_graddeg_pct having a p-value of 2.2e-16. All
of these are signficant as they are less than 0.05 so I concluded that
they were non-normal and should use the non-parametric correlation.
These results are further proven by the histograms were all of them are
skewed, except the ed_lessba_pct which looks slightly normal, but we saw
from the Shapiro-Wilk test that its p-value is less than 0.05 making it
significant.}

**Question 3** *Now you can assess correlation in each year. Use the
rcorr function from the Hmisc package to analyze the correlation of our
variables of interest in 2008 and 2016 using the Pearson or Spearman’s
test (depending on normality). Save the results of each function to an
object and then call it in your code so the correlation is visible in
this knitted lab document.*

``` r
elec_2008_1 <- elec_2008 %>% select(gop_pct, ed_lesshs_pct:ed_graddeg_pct)
elec_2016_1 <- elec_2016 %>% select(gop_pct, ed_lesshs_pct:ed_graddeg_pct)

elec_2008_cor <- rcorr(as.matrix(elec_2008_1), type = "spearman")
elec_2016_cor <- rcorr(as.matrix(elec_2016_1), type = "spearman")

elec_2008_cor
```

    ##                  gop_pct ed_lesshs_pct ed_hsdiploma_pct ed_lessba_pct ed_ba_pct
    ## gop_pct             1.00          0.16             0.16          0.03     -0.23
    ## ed_lesshs_pct       0.16          1.00             0.22         -0.59     -0.71
    ## ed_hsdiploma_pct    0.16          0.22             1.00         -0.39     -0.66
    ## ed_lessba_pct       0.03         -0.59            -0.39          1.00      0.35
    ## ed_ba_pct          -0.23         -0.71            -0.66          0.35      1.00
    ## ed_graddeg_pct     -0.21         -0.49            -0.58          0.24      0.79
    ##                  ed_graddeg_pct
    ## gop_pct                   -0.21
    ## ed_lesshs_pct             -0.49
    ## ed_hsdiploma_pct          -0.58
    ## ed_lessba_pct              0.24
    ## ed_ba_pct                  0.79
    ## ed_graddeg_pct             1.00
    ## 
    ## n= 3111 
    ## 
    ## 
    ## P
    ##                  gop_pct ed_lesshs_pct ed_hsdiploma_pct ed_lessba_pct ed_ba_pct
    ## gop_pct                  0.0000        0.0000           0.0596        0.0000   
    ## ed_lesshs_pct    0.0000                0.0000           0.0000        0.0000   
    ## ed_hsdiploma_pct 0.0000  0.0000                         0.0000        0.0000   
    ## ed_lessba_pct    0.0596  0.0000        0.0000                         0.0000   
    ## ed_ba_pct        0.0000  0.0000        0.0000           0.0000                 
    ## ed_graddeg_pct   0.0000  0.0000        0.0000           0.0000        0.0000   
    ##                  ed_graddeg_pct
    ## gop_pct          0.0000        
    ## ed_lesshs_pct    0.0000        
    ## ed_hsdiploma_pct 0.0000        
    ## ed_lessba_pct    0.0000        
    ## ed_ba_pct        0.0000        
    ## ed_graddeg_pct

``` r
elec_2016_cor
```

    ##                  gop_pct ed_lesshs_pct ed_hsdiploma_pct ed_lessba_pct ed_ba_pct
    ## gop_pct             1.00          0.18             0.40          0.07     -0.35
    ## ed_lesshs_pct       0.18          1.00             0.32         -0.46     -0.72
    ## ed_hsdiploma_pct    0.40          0.32             1.00         -0.35     -0.72
    ## ed_lessba_pct       0.07         -0.46            -0.35          1.00      0.24
    ## ed_ba_pct          -0.35         -0.72            -0.72          0.24      1.00
    ## ed_graddeg_pct     -0.45         -0.53            -0.62          0.03      0.73
    ##                  ed_graddeg_pct
    ## gop_pct                   -0.45
    ## ed_lesshs_pct             -0.53
    ## ed_hsdiploma_pct          -0.62
    ## ed_lessba_pct              0.03
    ## ed_ba_pct                  0.73
    ## ed_graddeg_pct             1.00
    ## 
    ## n= 3110 
    ## 
    ## 
    ## P
    ##                  gop_pct ed_lesshs_pct ed_hsdiploma_pct ed_lessba_pct ed_ba_pct
    ## gop_pct                  0.0000        0.0000           0.0002        0.0000   
    ## ed_lesshs_pct    0.0000                0.0000           0.0000        0.0000   
    ## ed_hsdiploma_pct 0.0000  0.0000                         0.0000        0.0000   
    ## ed_lessba_pct    0.0002  0.0000        0.0000                         0.0000   
    ## ed_ba_pct        0.0000  0.0000        0.0000           0.0000                 
    ## ed_graddeg_pct   0.0000  0.0000        0.0000           0.1444        0.0000   
    ##                  ed_graddeg_pct
    ## gop_pct          0.0000        
    ## ed_lesshs_pct    0.0000        
    ## ed_hsdiploma_pct 0.0000        
    ## ed_lessba_pct    0.1444        
    ## ed_ba_pct        0.0000        
    ## ed_graddeg_pct

**Question 4** *Use the corrplot package to create a graphic of the two
correlation matrices you created in question 3 using whatever styling
parameters (shapes, color scheme, sorting, etc.) you feel are best.*

``` r
elec_2008_matrix <- cor(elec_2008_1, method = "spearman")
elec_2016_matrix <- cor(elec_2016_1, method = "spearman")

corrplot(elec_2008_matrix, addCoef.col = 'black')
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
corrplot(elec_2016_matrix, addCoef.col = 'black')
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
elec_2008_matrix
```

    ##                      gop_pct ed_lesshs_pct ed_hsdiploma_pct ed_lessba_pct
    ## gop_pct           1.00000000     0.1621063        0.1630068    0.03378034
    ## ed_lesshs_pct     0.16210629     1.0000000        0.2213300   -0.58572680
    ## ed_hsdiploma_pct  0.16300681     0.2213300        1.0000000   -0.39234954
    ## ed_lessba_pct     0.03378034    -0.5857268       -0.3923495    1.00000000
    ## ed_ba_pct        -0.23194978    -0.7060353       -0.6610256    0.35421771
    ## ed_graddeg_pct   -0.20690147    -0.4943848       -0.5820720    0.23802731
    ##                   ed_ba_pct ed_graddeg_pct
    ## gop_pct          -0.2319498     -0.2069015
    ## ed_lesshs_pct    -0.7060353     -0.4943848
    ## ed_hsdiploma_pct -0.6610256     -0.5820720
    ## ed_lessba_pct     0.3542177      0.2380273
    ## ed_ba_pct         1.0000000      0.7890165
    ## ed_graddeg_pct    0.7890165      1.0000000

``` r
elec_2016_matrix
```

    ##                      gop_pct ed_lesshs_pct ed_hsdiploma_pct ed_lessba_pct
    ## gop_pct           1.00000000     0.1779525        0.3988263    0.06733654
    ## ed_lesshs_pct     0.17795251     1.0000000        0.3188720   -0.45906783
    ## ed_hsdiploma_pct  0.39882631     0.3188720        1.0000000   -0.34693468
    ## ed_lessba_pct     0.06733654    -0.4590678       -0.3469347    1.00000000
    ## ed_ba_pct        -0.35082720    -0.7233179       -0.7161040    0.24153134
    ## ed_graddeg_pct   -0.44582065    -0.5335489       -0.6228966    0.02617753
    ##                   ed_ba_pct ed_graddeg_pct
    ## gop_pct          -0.3508272    -0.44582065
    ## ed_lesshs_pct    -0.7233179    -0.53354893
    ## ed_hsdiploma_pct -0.7161040    -0.62289661
    ## ed_lessba_pct     0.2415313     0.02617753
    ## ed_ba_pct         1.0000000     0.72547178
    ## ed_graddeg_pct    0.7254718     1.00000000

**Question 5** *Based on results from your analysis in questions 3 and
4, evaluate the correlation between the GOP vote and the educational
attainment variables in both 2008 and 2016. focusing on direction,
magnitude and significance. What differences do you see within and
between those two years?*

{Beginning with the 2008 election, you can see an overall that there is
a positive correlation between GOP votes and a lower level of eduation
received and a negative correclation between GOP votes and a higher
level of education received. With ed_lesshs_pct there is a positive
correlation of 0.16 with GOP votes, ed_hsdiploma_pct there is also a
positive correlation of 0.16 with GOP votes, ed_lessba_pct has a
positive correlation of 0.03 with GOP votes. These educations are the
only positive correlations with GOP votes. Moving towards higher levels
of education we see a negative correlation. So ed_ba_pct has a negative
correlation of -0.23 with GOP votes and ed_graddeg_pct has a negative
correlation of -0.21 with GOP votes. Looking at this together we see the
trend that lower levels are education have a higher correlation with the
percentage of GOP votes. Whereas higher levels of education have a
negative correlation. Moving on to the election in 2016, we see the same
pattern of lower levels of education having a positive correlation with
gop_pct and higher levels having a negative correlation with gop_pct.
However in this election, the magnitude of the correlations are stronger
than they were in 2008. With ed_lesshs_pct there is a positive
correlation of 0.18 with GOP votes, ed_hsdiploma_pct there is a positive
correlation of 0.40 with GOP votes, ed_lessba_pct has a positive
correlation of 0.07 with GOP votes. These educations are the only
positive correlations with GOP votes. Moving towards higher levels of
education we see a negative correlation. So ed_ba_pct has a negative
correlation of -0.35 with GOP votes and ed_graddeg_pct has a negative
correlation of -0.45 with GOP votes. So between the two years the
correlation between lower levels of education and gop_pct became more
positive and the correlation between higher levels of education and
gop_pct became more negative. Despite this the overall trend of the
positive correlation between lower education and gop_pct and the
negative correlation between higher education and gop_pct stayed the
same between the elections of 2008 and 2016. In terms of significance,
we can get the p-values we use the rcorr function. Looking at this all
of the variables, except for ed_lessba_pct with a p-value of 0.0596, for
2008 have p-values less than 0.05 so we conclude that these results are
significant. The rest of the vairbales had a p-value of 0 which is less
than 0.05. Next, all of the 2016 variables have a p-value less than
0.05. All of the p-values there were 0 except ed_lessba_pct at 0.0002
which are all less than 0.05 and significant. As a result, we conclude
that all variables ,except for ed_lessba_pct in 2008, are significant.}

**Question 6** *How did the percentage of the vote for the GOP candidate
change *across counties* from 2008 to 2016. Join the two data frames for
these years and calculate the difference in the GOP vote across these
two elections. Also include the education variables from the 2016 data
so you can assess correlation in the next question. Call the head of the
resulting data frame using `kable` when you are done.*

``` r
elec_total <- merge(elec_2008_counties, elec_2016_counties, by = "county", all = TRUE)%>%
  rename(gop_pct_2008 = gop_pct.x, gop_pct_2016 = gop_pct.y, ed_lesshs_pct_2016 = ed_lesshs_pct.y, ed_hsdiploma_pct_2016 = ed_hsdiploma_pct.y, ed_lessba_pct_2016 = ed_lessba_pct.y, ed_ba_pct_2016 = ed_ba_pct.y, ed_graddeg_pct_2016 = ed_graddeg_pct.y, region = region.y, year_2008 = year.x, year_2016 = year.y)%>%
  mutate(diff_2016_2008 = gop_pct_2016 - gop_pct_2008)%>%
  select(-region.x, -ed_lesshs_pct.x:-ed_graddeg_pct.x, -county, -year_2008, -year_2016, -region)

elec_total_filter <- elec_total%>%
  select(-gop_pct_2008, -gop_pct_2016)

kable(head(elec_total))
```

| gop_pct_2008 | gop_pct_2016 | ed_lesshs_pct_2016 | ed_hsdiploma_pct_2016 | ed_lessba_pct_2016 | ed_ba_pct_2016 | ed_graddeg_pct_2016 | diff_2016_2008 |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 56.94028 | 62.86833 | 19.845992 | 34.64590 | 29.61731 | 11.369735 | 4.521059 | 5.928055 |
| 71.98907 | 77.26210 | 21.832125 | 41.17748 | 23.61819 | 9.506076 | 3.866131 | 5.273037 |
| 50.13762 | 54.47160 | 18.728544 | 38.80059 | 23.92456 | 11.099809 | 7.446493 | 4.333979 |
| 52.02172 | 47.93161 | 4.860101 | 22.98264 | 34.39484 | 25.267696 | 12.494720 | -4.090106 |
| 69.32735 | 80.66359 | 21.512695 | 38.74273 | 23.49342 | 7.769960 | 8.481187 | 11.336234 |
| 69.32735 | 65.33653 | 5.768479 | 43.09738 | 35.68635 | 11.615174 | 3.832616 | -3.990829 |

**Question 7** *Using `cor` and `corrplot`, create a visualization of
the correlation between the *change* in GOP vote between 2008 and 2016
(created in question 6) and your education variables for the 2016 data.
Include the text labels using the `addCoef.col` parameter as shown in
the class script. How would you interpret the results shown in this
graph? How is the question that it answers story different (if at all)
from the question answered by the graph in question 4?*

``` r
elec_diff_matrix <- cor(elec_total_filter, method = "spearman")

corrplot(elec_diff_matrix, order = "hclust", addCoef.col = 'black')
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

{The story from this correlation plot basically backs up the correlation
plots that we see in the previous question. When calculating the rates,
there tends to be a positive increase in the gop_pct, but some counties
did decrease in the gop_pct. When you look at the correlation plot we
can see that there is a similar trend in this as their was in question
4. In this, we see that there is a positive correlation between the
diff_2016_2008 in the lower levels of education, such as
ed_lesshs_pct_2016 having a positive correlation of 0.15,
ed_hsdiploma_pct_2016 having a positive correlation of 0.33, and
ed_lessba_pct_2016 having a positive correlation of 0.02. Similarly the
higher levels of education are negatively correlated with ed_ba_pct_2016
having a negative correlation of -0.31 and ed_graddeg_pct_2016 have a
negative correlation of -0.34. As we can see looking at this graph, it
essentially tells the same story as the previous correlation plots with
lower levels of education being positively correlated and higher levels
of education being negatively correlated.}

**Challenge question** The election data we used for this project also
includes median income. In 2012, what was the correlation between median
income and the GOP vote share for the whole country? Then pick two
regions and calculate the correlation between these variables in each
one. Present your results and explain what they tell you about the
relationship between these variables country wide and within each of the
regions you chose focusing on the direction and magnitude of the
correlation.

``` r
#Regions are South and Midwest
elec_country <- election_data%>%
  filter(year == "2012")%>%
  select(gop, totpop_ind)

shapiro.test(elec_country$gop) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_country$gop
    ## W = 0.40808, p-value < 2.2e-16

``` r
shapiro.test(elec_country$totpop_ind)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_country$totpop_ind
    ## W = 0.95946, p-value < 2.2e-16

``` r
elec_2012_matrix <- cor(elec_country, method = "spearman")

corrplot(elec_2012_matrix, addCoef.col = 'black')
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
elec_region_south <- election_data%>%
  filter(year == "2012")%>%
  filter(region %in% c("South Region"))%>%
  select(gop, totpop_ind)

elec_region_midwest <- election_data%>%
  filter(year == "2012")%>%
  filter(region %in% c("Midwest Region"))%>%
  select(gop, totpop_ind)

shapiro.test(elec_region_south$gop) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_region_south$gop
    ## W = 0.42729, p-value < 2.2e-16

``` r
shapiro.test(elec_region_south$totpop_ind)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_region_south$totpop_ind
    ## W = 0.951, p-value < 2.2e-16

``` r
shapiro.test(elec_region_midwest$gop) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_region_midwest$gop
    ## W = 0.39596, p-value < 2.2e-16

``` r
shapiro.test(elec_region_midwest$totpop_ind)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  elec_region_midwest$totpop_ind
    ## W = 0.96232, p-value = 7.225e-16

``` r
elec_2012_south_matrix <- cor(elec_region_south, method = "spearman")
elec_2012_midwest_matrix <- cor(elec_region_midwest, method = "spearman")

corrplot(elec_2012_south_matrix, addCoef.col = 'white')
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
corrplot(elec_2012_midwest_matrix, addCoef.col = 'white', order = "AOE")
```

![](Lab-5_correlation_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

{For these correlations they are quite similar in the whole country as
well as in the two selected regions (South and Midwest). In each of
these plots there is a positive correlation between gop vote share and
median income. Nationwide there is a positive correlation of 0.56
between the gop vote share and the totpop_ind. Looking at the South
Region in particular there is also a positive correlation, this one is
0.57, between the gop vote share and the totpop_ind. Finally, there is
also a positive correlation of 0.55 between gop and totpop_ind for the
Midwest region. The correlation in all of these is pretty uniform in
both the country and the regions I selected.}

**Disclosure of assistance:** *Besides class materials, what other
sources of assistance did you use while completing this lab? These can
include input from classmates, relevant material identified through web
searches (e.g., Stack Overflow), or assistance from ChatGPT or other AI
tools. How did these sources support your own learning in completing
this lab?*

{For this lab, I used the scripts from in class and some online
assistance (Stack Overflow and Geeks for Geeks).}

**Lab reflection:** *How do you feel about the work you did on this lab?
Was it easy, moderate, or hard? What were the biggest things you learned
by completing it?*

{I enjoyed this lab and thought the information was very interesting. I
thought it was moderate and a good way to solidify what we did in class.
The biggest things I learned was making the correlation matrices and
then how to customize the correlation plots more.}
