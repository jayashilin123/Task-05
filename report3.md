Statistical tests
================

One sample Wilcoxon Test
------------------------

Wilcoxon's rank sum test (also known as the unpaired Wilcoxon rank sum test or the Mann-Whitney U test) It is the test for ordinal or continuous data. In contrast to Student's t-test, does not require the data to be normally distributed. This test too can be used for paired or unpaired data.

Dataset shows different method for identifying protiens and the accuracy.

``` r
load("rdas/protStruct.rda")
head(protStruct)
```

    ##     Protein Method Correct
    ## 1 Ubiquitin  CFAVG   0.467
    ## 2 Ubiquitin    GOR   0.645
    ## 3 Ubiquitin    PHD   0.868
    ## 4   DeoxyHb  CFAVG   0.472
    ## 5   DeoxyHb    GOR   0.844
    ## 6   DeoxyHb    PHD   0.879

``` r
str(protStruct)
```

    ## 'data.frame':    12 obs. of  3 variables:
    ##  $ Protein: Factor w/ 4 levels "DeoxyHb","Prealbumin",..: 4 4 4 1 1 1 3 3 3 2 ...
    ##  $ Method : Factor w/ 3 levels "CFAVG","GOR",..: 1 2 3 1 2 3 1 2 3 1 ...
    ##  $ Correct: num  0.467 0.645 0.868 0.472 0.844 0.879 0.405 0.604 0.787 0.449 ...

Test against a specific hypothesis, mean =0.5

``` r
wilcox.test(protStruct$Correct,mu=0.5) 
```

    ## 
    ##  Wilcoxon signed rank test
    ## 
    ## data:  protStruct$Correct
    ## V = 68, p-value = 0.021
    ## alternative hypothesis: true location is not equal to 0.5

The results of this test concur with the results of the t-test that the central measure (mean or median) of the data differs significantly from 0.5.

Two sample Wilcoxon Rank Sum Tests
----------------------------------

Load the data

Dataset includes info on effectivesness of 2 painkillers as rated by patients

``` r
load("rdas/medrank.rda")
medrank
```

    ##         Drug Score Rank
    ## 1  Oxycodone     1  1.0
    ## 2  Oxycodone     2  2.5
    ## 3  Oxycodone     2  2.5
    ## 4  Oxycodone     3  4.0
    ## 5  Oxycodone     4  6.0
    ## 6  Oxycodone     4  6.0
    ## 7  Oxycodone     5  9.0
    ## 8  Oxycodone     5  9.0
    ## 9  Oxycodone     6 13.0
    ## 10 Oxycodone     6 13.0
    ## 11 Oxycodone     8 21.5
    ## 12 Oxycodone     8 21.5
    ## 13 Ibuprofen     4  6.0
    ## 14 Ibuprofen     5  9.0
    ## 15 Ibuprofen     6 13.0
    ## 16 Ibuprofen     6 13.0
    ## 17 Ibuprofen     6 13.0
    ## 18 Ibuprofen     7 17.5
    ## 19 Ibuprofen     7 17.5
    ## 20 Ibuprofen     7 17.5
    ## 21 Ibuprofen     7 17.5
    ## 22 Ibuprofen     8 21.5
    ## 23 Ibuprofen     8 21.5
    ## 24 Ibuprofen     9 24.0

``` r
group_by(medrank, Drug) %>%
  summarise(
    count = n(),
    medi_score = median(Score, na.rm = TRUE),
    IQR_score = IQR(Score, na.rm = TRUE),
    medi_rank = median(Rank, na.rm = TRUE),
    IQR_rank = IQR(Rank, na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 6
    ##   Drug      count medi_score IQR_score medi_rank IQR_rank
    ##   <fct>     <int>      <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 Ibuprofen    12        7        1.25      17.5     5.5 
    ## 2 Oxycodone    12        4.5      3.25       7.5     9.38

``` r
oxycodone_rank <- medrank$Rank[medrank$Drug=="Oxycodone"]
ibuprofen_rank <- medrank$Rank[medrank$Drug=="Ibuprofen"]
```

Test the 2 drugs

``` r
relation <- wilcox.test(oxycodone_rank, ibuprofen_rank)
```

    ## Warning in wilcox.test.default(oxycodone_rank, ibuprofen_rank): cannot
    ## compute exact p-value with ties

``` r
relation
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  oxycodone_rank and ibuprofen_rank
    ## W = 31, p-value = 0.0181
    ## alternative hypothesis: true location shift is not equal to 0

No difference between the median of 2 groups

Unpaired Wilcoxon Rank Sum Tests
--------------------------------

Load the data

``` r
load("rdas/my_data.rda")
head(my_data)
```

    ##   group weight
    ## 1 Woman   38.9
    ## 2 Woman   61.2
    ## 3 Woman   73.3
    ## 4 Woman   21.8
    ## 5 Woman   63.4
    ## 6 Woman   64.6

``` r
group_by(my_data, group) %>%
  summarise(
    count = n(),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )
```

    ## # A tibble: 2 x 4
    ##   group count median   IQR
    ##   <fct> <int>  <dbl> <dbl>
    ## 1 Man       9   67.3  10.9
    ## 2 Woman     9   48.8  15

``` r
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Weight", xlab = "Groups")
```

![](report3_files/figure-markdown_github/unnamed-chunk-12-1.png)

### Compute two-samples Wilcoxon test - Method 1

``` r
women_weight <- my_data$weight[my_data$group=="Woman"]
men_weight <- my_data$weight[my_data$group=="Man"]
res <- wilcox.test(women_weight, men_weight)
```

    ## Warning in wilcox.test.default(women_weight, men_weight): cannot compute
    ## exact p-value with ties

``` r
res
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  women_weight and men_weight
    ## W = 15, p-value = 0.02712
    ## alternative hypothesis: true location shift is not equal to 0

### Compute two-samples Wilcoxon test - Method 2

``` r
res <- wilcox.test(weight ~ group, data = my_data,
                   exact = FALSE)
res
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  weight by group
    ## W = 66, p-value = 0.02712
    ## alternative hypothesis: true location shift is not equal to 0

To test whether the median men's weight is less than the median women's weight:

``` r
wilcox.test(weight ~ group, data = my_data, 
            exact = FALSE, alternative = "less")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  weight by group
    ## W = 66, p-value = 0.9892
    ## alternative hypothesis: true location shift is less than 0

To test whether the median men's weight is greater than the median women's weight

``` r
wilcox.test(weight ~ group, data = my_data,
            exact = FALSE, alternative = "greater")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  weight by group
    ## W = 66, p-value = 0.01356
    ## alternative hypothesis: true location shift is greater than 0

Pairwise Wilcoxon Rank Sum Tests
--------------------------------

Load the data

``` r
attach(airquality)
airquality
```

    ##     Ozone Solar.R Wind Temp Month Day
    ## 1      41     190  7.4   67     5   1
    ## 2      36     118  8.0   72     5   2
    ## 3      12     149 12.6   74     5   3
    ## 4      18     313 11.5   62     5   4
    ## 5      NA      NA 14.3   56     5   5
    ## 6      28      NA 14.9   66     5   6
    ## 7      23     299  8.6   65     5   7
    ## 8      19      99 13.8   59     5   8
    ## 9       8      19 20.1   61     5   9
    ## 10     NA     194  8.6   69     5  10
    ## 11      7      NA  6.9   74     5  11
    ## 12     16     256  9.7   69     5  12
    ## 13     11     290  9.2   66     5  13
    ## 14     14     274 10.9   68     5  14
    ## 15     18      65 13.2   58     5  15
    ## 16     14     334 11.5   64     5  16
    ## 17     34     307 12.0   66     5  17
    ## 18      6      78 18.4   57     5  18
    ## 19     30     322 11.5   68     5  19
    ## 20     11      44  9.7   62     5  20
    ## 21      1       8  9.7   59     5  21
    ## 22     11     320 16.6   73     5  22
    ## 23      4      25  9.7   61     5  23
    ## 24     32      92 12.0   61     5  24
    ## 25     NA      66 16.6   57     5  25
    ## 26     NA     266 14.9   58     5  26
    ## 27     NA      NA  8.0   57     5  27
    ## 28     23      13 12.0   67     5  28
    ## 29     45     252 14.9   81     5  29
    ## 30    115     223  5.7   79     5  30
    ## 31     37     279  7.4   76     5  31
    ## 32     NA     286  8.6   78     6   1
    ## 33     NA     287  9.7   74     6   2
    ## 34     NA     242 16.1   67     6   3
    ## 35     NA     186  9.2   84     6   4
    ## 36     NA     220  8.6   85     6   5
    ## 37     NA     264 14.3   79     6   6
    ## 38     29     127  9.7   82     6   7
    ## 39     NA     273  6.9   87     6   8
    ## 40     71     291 13.8   90     6   9
    ## 41     39     323 11.5   87     6  10
    ## 42     NA     259 10.9   93     6  11
    ## 43     NA     250  9.2   92     6  12
    ## 44     23     148  8.0   82     6  13
    ## 45     NA     332 13.8   80     6  14
    ## 46     NA     322 11.5   79     6  15
    ## 47     21     191 14.9   77     6  16
    ## 48     37     284 20.7   72     6  17
    ## 49     20      37  9.2   65     6  18
    ## 50     12     120 11.5   73     6  19
    ## 51     13     137 10.3   76     6  20
    ## 52     NA     150  6.3   77     6  21
    ## 53     NA      59  1.7   76     6  22
    ## 54     NA      91  4.6   76     6  23
    ## 55     NA     250  6.3   76     6  24
    ## 56     NA     135  8.0   75     6  25
    ## 57     NA     127  8.0   78     6  26
    ## 58     NA      47 10.3   73     6  27
    ## 59     NA      98 11.5   80     6  28
    ## 60     NA      31 14.9   77     6  29
    ## 61     NA     138  8.0   83     6  30
    ## 62    135     269  4.1   84     7   1
    ## 63     49     248  9.2   85     7   2
    ## 64     32     236  9.2   81     7   3
    ## 65     NA     101 10.9   84     7   4
    ## 66     64     175  4.6   83     7   5
    ## 67     40     314 10.9   83     7   6
    ## 68     77     276  5.1   88     7   7
    ## 69     97     267  6.3   92     7   8
    ## 70     97     272  5.7   92     7   9
    ## 71     85     175  7.4   89     7  10
    ## 72     NA     139  8.6   82     7  11
    ## 73     10     264 14.3   73     7  12
    ## 74     27     175 14.9   81     7  13
    ## 75     NA     291 14.9   91     7  14
    ## 76      7      48 14.3   80     7  15
    ## 77     48     260  6.9   81     7  16
    ## 78     35     274 10.3   82     7  17
    ## 79     61     285  6.3   84     7  18
    ## 80     79     187  5.1   87     7  19
    ## 81     63     220 11.5   85     7  20
    ## 82     16       7  6.9   74     7  21
    ## 83     NA     258  9.7   81     7  22
    ## 84     NA     295 11.5   82     7  23
    ## 85     80     294  8.6   86     7  24
    ## 86    108     223  8.0   85     7  25
    ## 87     20      81  8.6   82     7  26
    ## 88     52      82 12.0   86     7  27
    ## 89     82     213  7.4   88     7  28
    ## 90     50     275  7.4   86     7  29
    ## 91     64     253  7.4   83     7  30
    ## 92     59     254  9.2   81     7  31
    ## 93     39      83  6.9   81     8   1
    ## 94      9      24 13.8   81     8   2
    ## 95     16      77  7.4   82     8   3
    ## 96     78      NA  6.9   86     8   4
    ## 97     35      NA  7.4   85     8   5
    ## 98     66      NA  4.6   87     8   6
    ## 99    122     255  4.0   89     8   7
    ## 100    89     229 10.3   90     8   8
    ## 101   110     207  8.0   90     8   9
    ## 102    NA     222  8.6   92     8  10
    ## 103    NA     137 11.5   86     8  11
    ## 104    44     192 11.5   86     8  12
    ## 105    28     273 11.5   82     8  13
    ## 106    65     157  9.7   80     8  14
    ## 107    NA      64 11.5   79     8  15
    ## 108    22      71 10.3   77     8  16
    ## 109    59      51  6.3   79     8  17
    ## 110    23     115  7.4   76     8  18
    ## 111    31     244 10.9   78     8  19
    ## 112    44     190 10.3   78     8  20
    ## 113    21     259 15.5   77     8  21
    ## 114     9      36 14.3   72     8  22
    ## 115    NA     255 12.6   75     8  23
    ## 116    45     212  9.7   79     8  24
    ## 117   168     238  3.4   81     8  25
    ## 118    73     215  8.0   86     8  26
    ## 119    NA     153  5.7   88     8  27
    ## 120    76     203  9.7   97     8  28
    ## 121   118     225  2.3   94     8  29
    ## 122    84     237  6.3   96     8  30
    ## 123    85     188  6.3   94     8  31
    ## 124    96     167  6.9   91     9   1
    ## 125    78     197  5.1   92     9   2
    ## 126    73     183  2.8   93     9   3
    ## 127    91     189  4.6   93     9   4
    ## 128    47      95  7.4   87     9   5
    ## 129    32      92 15.5   84     9   6
    ## 130    20     252 10.9   80     9   7
    ## 131    23     220 10.3   78     9   8
    ## 132    21     230 10.9   75     9   9
    ## 133    24     259  9.7   73     9  10
    ## 134    44     236 14.9   81     9  11
    ## 135    21     259 15.5   76     9  12
    ## 136    28     238  6.3   77     9  13
    ## 137     9      24 10.9   71     9  14
    ## 138    13     112 11.5   71     9  15
    ## 139    46     237  6.9   78     9  16
    ## 140    18     224 13.8   67     9  17
    ## 141    13      27 10.3   76     9  18
    ## 142    24     238 10.3   68     9  19
    ## 143    16     201  8.0   82     9  20
    ## 144    13     238 12.6   64     9  21
    ## 145    23      14  9.2   71     9  22
    ## 146    36     139 10.3   81     9  23
    ## 147     7      49 10.3   69     9  24
    ## 148    14      20 16.6   63     9  25
    ## 149    30     193  6.9   70     9  26
    ## 150    NA     145 13.2   77     9  27
    ## 151    14     191 14.3   75     9  28
    ## 152    18     131  8.0   76     9  29
    ## 153    20     223 11.5   68     9  30

``` r
Month <- factor(Month, labels = month.abb[5:9])
```

``` r
pairwise.wilcox.test(Ozone, Month)
```

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test 
    ## 
    ## data:  Ozone and Month 
    ## 
    ##     May    Jun    Jul    Aug   
    ## Jun 0.5775 -      -      -     
    ## Jul 0.0003 0.0848 -      -     
    ## Aug 0.0011 0.1295 1.0000 -     
    ## Sep 0.4744 1.0000 0.0060 0.0227
    ## 
    ## P value adjustment method: holm

These give warnings because of ties

``` r
pairwise.wilcox.test(Ozone, Month, p.adj = "bonf")
```

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## Warning in wilcox.test.default(xi, xj, paired = paired, ...): cannot
    ## compute exact p-value with ties

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test 
    ## 
    ## data:  Ozone and Month 
    ## 
    ##     May    Jun    Jul    Aug   
    ## Jun 1.0000 -      -      -     
    ## Jul 0.0003 0.1414 -      -     
    ## Aug 0.0012 0.2591 1.0000 -     
    ## Sep 1.0000 1.0000 0.0074 0.0325
    ## 
    ## P value adjustment method: bonferroni

``` r
detach()
```

Krusal Wallis Test
------------------

Load the data

Dataset is about the reaction time taken after intake of 3 different drinks

``` r
load("rdas/reactionR.rda")
str(reactionR)
```

    ## 'data.frame':    30 obs. of  3 variables:
    ##  $ ReactionTime: num  0.37 0.38 0.61 0.78 0.83 0.86 0.9 0.95 0.98 1.11 ...
    ##  $ Drink       : Factor w/ 3 levels "Alcohol","Coffee",..: 3 3 3 3 3 3 3 3 2 2 ...
    ##  $ Rank        : num  1 2 3 4 5 6 7 8 9 10 ...

Medians

``` r
tapply(reactionR$ReactionTime, reactionR$Drink, median)
```

    ## Alcohol  Coffee   Water 
    ##   2.250   1.445   0.845

Summary

``` r
Water_summary<-summary(reactionR$ReactionTime[reactionR$Drink=='Water'])
Coffee_summary<-summary(reactionR$ReactionTime[reactionR$Drink=='Coffee'])
Alcohol_summary<-summary(reactionR$ReactionTime[reactionR$Drink=='Alcohol'])
```

``` r
compare1<-cbind(Water_summary,Coffee_summary,Alcohol_summary)
round(compare1,2)
```

    ##         Water_summary Coffee_summary Alcohol_summary
    ## Min.             0.37           0.98            1.69
    ## 1st Qu.          0.65           1.28            1.77
    ## Median           0.84           1.44            2.25
    ## Mean             0.93           1.64            2.38
    ## 3rd Qu.          0.94           1.68            2.85
    ## Max.             1.97           3.07            3.47

``` r
boxplot(reactionR$ReactionTime~reactionR$Drink)
```

![](report3_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
kruskal.test(reactionR$ReactionTime~reactionR$Drink)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  reactionR$ReactionTime by reactionR$Drink
    ## Kruskal-Wallis chi-squared = 16.322, df = 2, p-value = 0.0002856

The simplest adjustment is the Bonferroni adjustment p.adj='bonferroni' which multiplies each Wilcoxon signed rank p-value by the total number of Wilcoxon tests being carried out (here it is 3) while the exact=F stands for the asymptotic test which allows tied ranks.

Look for which drinks are

``` r
pairwise.wilcox.test(reactionR$ReactionTime,reactionR$Drink,p.adj='bonferroni',exact=F)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test 
    ## 
    ## data:  reactionR$ReactionTime and reactionR$Drink 
    ## 
    ##        Alcohol Coffee
    ## Coffee 0.042   -     
    ## Water  0.002   0.027 
    ## 
    ## P value adjustment method: bonferroni

Friedman Test
-------------

Load the data

``` r
load("rdas/Data.rda")
str(Data)
```

    ## 'data.frame':    40 obs. of  3 variables:
    ##  $ Instructor: Factor w/ 5 levels "'Bob Belcher'",..: 1 1 1 1 1 1 1 1 3 3 ...
    ##  $ Rater     : Factor w/ 8 levels "a","b","c","d",..: 1 2 3 4 5 6 7 8 1 2 ...
    ##  $ Likert    : int  4 5 4 6 6 6 10 6 8 6 ...

``` r
Data$Likert.f <- as.factor(Data$Likert)
XT <- xtabs( ~ Instructor + Likert.f,
             data = Data)
XT
```

    ##                   Likert.f
    ## Instructor         4 5 6 7 8 9 10
    ##   'Bob Belcher'    2 1 4 0 0 0  1
    ##   'Gene Belcher'   1 4 3 0 0 0  0
    ##   'Linda Belcher'  0 0 1 1 4 1  1
    ##   'Louise Belcher' 0 0 0 1 4 2  1
    ##   'Tina Belcher'   0 1 0 2 2 2  1

As proportions

``` r
prop.table(XT, 
           margin = 1)
```

    ##                   Likert.f
    ## Instructor             4     5     6     7     8     9    10
    ##   'Bob Belcher'    0.250 0.125 0.500 0.000 0.000 0.000 0.125
    ##   'Gene Belcher'   0.125 0.500 0.375 0.000 0.000 0.000 0.000
    ##   'Linda Belcher'  0.000 0.000 0.125 0.125 0.500 0.125 0.125
    ##   'Louise Belcher' 0.000 0.000 0.000 0.125 0.500 0.250 0.125
    ##   'Tina Belcher'   0.000 0.125 0.000 0.250 0.250 0.250 0.125

``` r
histogram(~ Likert.f | Instructor,
          data=Data,
          layout=c(1,5)      #  columns and rows of individual plots
)
```

![](report3_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
friedman.test(Likert ~ Instructor | Rater,
              data = Data)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  Likert and Instructor and Rater
    ## Friedman chi-squared = 23.139, df = 4, p-value = 0.0001188

Also done as

``` r
friedman.test(Data$Likert, Data$Instructor, Data$Rater)
```

    ## 
    ##  Friedman rank sum test
    ## 
    ## data:  Data$Likert, Data$Instructor and Data$Rater
    ## Friedman chi-squared = 23.139, df = 4, p-value = 0.0001188

``` r
Ratertable <- xtabs(Likert ~ Instructor + Rater, 
                    data = Data)
```

Can do rowmeans of ratertable to see the difference in mean ratings,the diff is tested statistically by friedmantest

### Conover test

``` r
PT = posthoc.friedman.conover.test(y      = Data$Likert,
                                   groups = Data$Instructor,
                                   blocks = Data$Rater,
                                   p.adjust.method="fdr")
PT
```

    ## 
    ##  Pairwise comparisons using Conover's test for a two-way 
    ##                     balanced complete block design 
    ## 
    ## data:  Data$Likert , Data$Instructor and Data$Rater 
    ## 
    ##                  'Bob Belcher' 'Gene Belcher' 'Linda Belcher'
    ## 'Gene Belcher'   0.17328       -              -              
    ## 'Linda Belcher'  2.8e-05       1.2e-06        -              
    ## 'Louise Belcher' 1.9e-06       1.1e-07        0.27303        
    ## 'Tina Belcher'   0.00037       8.8e-06        0.31821        
    ##                  'Louise Belcher'
    ## 'Gene Belcher'   -               
    ## 'Linda Belcher'  -               
    ## 'Louise Belcher' -               
    ## 'Tina Belcher'   0.05154         
    ## 
    ## P value adjustment method: fdr
