---
title: Exploring 2018 Baseball Data
author: ''
date: '2022-04-30'
slug: exploring-2018-baseball-data
categories: []
tags: []
subtitle: ''
summary: ''
authors: []
lastmod: '2022-04-30T16:24:32Z'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
## ✔ tibble  3.1.6     ✔ dplyr   1.0.8
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(readxl)
library(corrplot)
```

```
## corrplot 0.92 loaded
```



```r
standard_pitching_df = read_xlsx("three_types_of_pitching_2018.xlsx",sheet = "standard_pitching")
```




```r
mod_1_standard_pitching_df = standard_pitching_df %>% 
  distinct(Name, .keep_all = TRUE)%>% 
  drop_na()  %>% 
  mutate(ERA = as.numeric(ERA))
```


## Question 1
How old is the average player at each level of play (American League, National League, Major League)?
#### Variables
Response: Age
Explanatory: Lg

### Visualize Data

```r
mod_1_standard_pitching_df %>% 
  ggplot(aes(x=Lg, y=Age, fill=Lg)) +
  geom_boxplot()+
  labs(x = 'League', y = 'Age', title = 'Ages by League') +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold', color = 'blue'))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

### Summarize of Data

```r
mod_1_standard_pitching_df %>% 
  group_by(Lg) %>% 
  summarise(n(),
            mean(Age),
            sd(Age))
```

```
## # A tibble: 3 × 4
##   Lg    `n()` `mean(Age)` `sd(Age)`
##   <chr> <int>       <dbl>     <dbl>
## 1 AL      289        28.0      3.72
## 2 MLB      43        29.1      3.27
## 3 NL      302        27.6      3.61
```

### Analysis of Data

```r
mod1_pitching_df_aov = aov(Age~Lg, data = mod_1_standard_pitching_df)
summary(mod1_pitching_df_aov)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)  
## Lg            2     96   47.98   3.622 0.0273 *
## Residuals   631   8358   13.25                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## Question 2     
How many runs does the average pitcher allow in one inning by Age? (Runs allowed / Innings pitched by Age)
#### Variables
Response: ERA
Explanatory: Age


### Visualize Data


```r
mod_1_standard_pitching_df %>% 
  ggplot(aes(x = Age, y = ERA)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = 'y ~ x',se = FALSE) + 
  labs(x = 'Age', y = 'Estimated Runs Allowed', title = 'Age vs Average Runs Allowed Every 9 innings') +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'bold', color = 'red'))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### Summarize of Data

```r
mod_1_standard_pitching_df %>% 
  select(Age,ERA) %>% 
  cor()
```

```
##              Age          ERA
## Age  1.000000000 -0.007812722
## ERA -0.007812722  1.000000000
```
#### Summary of Age

```r
summary(mod_1_standard_pitching_df$Age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   20.00   25.00   27.00   27.89   30.00   45.00
```

#### Summary of ERA

```r
summary(mod_1_standard_pitching_df$ERA)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.780   3.333   4.295   5.048   5.567  81.000
```

### Analysis of Data

```r
mod1_pitching_df_lm = lm(ERA~Age, data = mod_1_standard_pitching_df)
summary(mod1_pitching_df_lm)
```

```
## 
## Call:
## lm(formula = ERA ~ Age, data = mod_1_standard_pitching_df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.247 -1.717 -0.763  0.514 75.932 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.33277    1.46087   3.650 0.000283 ***
## Age         -0.01020    0.05193  -0.196 0.844349    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.775 on 632 degrees of freedom
## Multiple R-squared:  6.104e-05,	Adjusted R-squared:  -0.001521 
## F-statistic: 0.03858 on 1 and 632 DF,  p-value: 0.8443
```


## Question 3
Typically, do older or younger pitchers get more playing time (Innings Pitched), or is there not correlation?
#### Variables
Response: IP
Explanatory: Age


### Visualize Data

```r
mod_1_standard_pitching_df %>% 
  ggplot(aes(x = Age, y = IP)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = 'y ~ x',se = FALSE) + 
  labs(x = 'Age', y = 'Estimated Runs Allowed', title = 'Age vs Average Runs Allowed Every 9 innings') +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'bold', color = 'red'))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

### Summarize of Data

```r
mod_1_standard_pitching_df %>% 
  select(Age,IP) %>% 
  cor()
```

```
##           Age        IP
## Age 1.0000000 0.1433379
## IP  0.1433379 1.0000000
```
#### Summary of Age

```r
summary(mod_1_standard_pitching_df$Age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   20.00   25.00   27.00   27.89   30.00   45.00
```

#### Summary of IP

```r
summary(mod_1_standard_pitching_df$IP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.10   27.10   52.00   66.50   85.08  220.20
```

### Analysis of Data

```r
mod2_pitching_df_lm = lm(IP~Age, data = mod_1_standard_pitching_df)
summary(mod2_pitching_df_lm)
```

```
## 
## Call:
## lm(formula = IP ~ Age, data = mod_1_standard_pitching_df)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -80.33 -39.49 -16.44  20.11 151.64 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.2863    16.1244   0.514 0.607505    
## Age           2.0869     0.5732   3.641 0.000294 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52.7 on 632 degrees of freedom
## Multiple R-squared:  0.02055,	Adjusted R-squared:  0.019 
## F-statistic: 13.26 on 1 and 632 DF,  p-value: 0.0002938
```





## Question 4
Does wild pitches have an impact the number of hits?
#### Variables
Response: H (hits)
Explanatory: WP (wild pitches)


### Visualize Data

```r
mod_1_standard_pitching_df %>% 
  ggplot(aes(x = WP, y = H)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = 'y ~ x',se = FALSE) + 
  labs(x = 'Wild Pitches', y = 'Hits', title = 'Wild Pitches Compared With Hits') +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = 'bold', color = 'blue'))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

### Summarize of Data

```r
mod_1_standard_pitching_df %>% 
  select(WP,H) %>% 
  cor()
```

```
##           WP         H
## WP 1.0000000 0.4977737
## H  0.4977737 1.0000000
```
#### Summary of H

```r
summary(mod_1_standard_pitching_df$H)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00   26.25   47.00   62.61   84.75  211.00
```

#### Summary of WP

```r
summary(mod_1_standard_pitching_df$WP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   1.000   2.000   2.793   4.000  17.000
```

### Analysis of Data

```r
mod2_pitching_df_lm = lm(H~WP, data = mod_1_standard_pitching_df)
summary(mod2_pitching_df_lm)
```

```
## 
## Call:
## lm(formula = H ~ WP, data = mod_1_standard_pitching_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -104.67  -29.34  -12.06   19.02  151.28 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  38.3382     2.3829   16.09   <2e-16 ***
## WP            8.6886     0.6022   14.43   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42.5 on 632 degrees of freedom
## Multiple R-squared:  0.2478,	Adjusted R-squared:  0.2466 
## F-statistic: 208.2 on 1 and 632 DF,  p-value: < 2.2e-16
```



## Question 5
Does earned run averages influence the number of bases on balls per nine innings pitched?
#### Variables
Response: BB (Bases on Balls/Walks)
Explanatory: ERA (Estimated Runs Allowed)


### Visualize Data

```r
mod_1_standard_pitching_df %>% 
  ggplot(aes(x = ERA, y = BB)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = 'y ~ x',se = FALSE) + 
  labs(x = 'Estimated Runs Allowed', y = 'Bases on Balls', title = 'ERA Compared With Bases on Balls') +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold', color = 'orange'))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-22-1.png" width="672" />

### Summarize of Data

```r
mod_1_standard_pitching_df %>% 
  select(ERA,BB) %>% 
  cor()
```

```
##            ERA         BB
## ERA  1.0000000 -0.2035691
## BB  -0.2035691  1.0000000
```
#### Summary of Age

```r
summary(mod_1_standard_pitching_df$ERA)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.780   3.333   4.295   5.048   5.567  81.000
```

#### Summary of IP

```r
summary(mod_1_standard_pitching_df$BB)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    11.0    19.0    23.8    32.0    95.0
```

### Analysis of Data

```r
mod2_pitching_df_lm = lm(BB~ERA, data = mod_1_standard_pitching_df)
summary(mod2_pitching_df_lm)
```

```
## 
## Call:
## lm(formula = BB ~ ERA, data = mod_1_standard_pitching_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.619 -12.183  -4.898   7.865  71.396 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  27.6498     1.0139  27.271  < 2e-16 ***
## ERA          -0.7633     0.1460  -5.227 2.34e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 17.53 on 632 degrees of freedom
## Multiple R-squared:  0.04144,	Adjusted R-squared:  0.03992 
## F-statistic: 27.32 on 1 and 632 DF,  p-value: 2.343e-07
```
