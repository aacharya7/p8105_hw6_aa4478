Homework 6
================
Ayeshra Acharya
11/24/2020

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
library(modelr)
```

## Problem 1

``` r
homicide_df = 
  read_csv("data/homicide-data.csv", na = c("", "NA", "Unknown")) %>% 
  mutate(
    city_state = str_c(city, state, sep = ", "),
    victim_age = as.numeric(victim_age),
    resolution = case_when(
      disposition == "Closed without arrest" ~ 0,
      disposition == "Open/No arrest"        ~ 0,
      disposition == "Closed by arrest"      ~ 1)
  ) %>% 
  filter(
    victim_race %in% c("White", "Black"),
    city_state != "Tulsa, AL") %>% 
  select(city_state, resolution, victim_age, victim_race, victim_sex)
```

Start with one city.

``` r
baltimore_df =
  homicide_df %>% 
  filter(city_state == "Baltimore, MD")
glm(resolution ~ victim_age + victim_race + victim_sex, 
    data = baltimore_df,
    family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(term, OR, starts_with("CI")) %>% 
  knitr::kable(digits = 3)
```

| term              |    OR | CI\_lower | CI\_upper |
| :---------------- | ----: | --------: | --------: |
| (Intercept)       | 1.363 |     0.975 |     1.907 |
| victim\_age       | 0.993 |     0.987 |     1.000 |
| victim\_raceWhite | 2.320 |     1.648 |     3.268 |
| victim\_sexMale   | 0.426 |     0.325 |     0.558 |

Want to do same process for every city in dataset. How? First, nest
dataset and then map over that fitting the regression model and then map
across regression model. We want city estimates of OR and estimates
comparing age, race and sex.

``` r
models_results_df = 
  homicide_df %>% 
  nest(data = -city_state) %>% 
  mutate(
    models = 
      map(.x = data, ~glm(resolution ~ victim_age + victim_race + victim_sex, data = .x, family = binomial())),
    results = map(models, broom::tidy)
  ) %>% 
  select(city_state, results) %>% 
  unnest(results) %>% 
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>% 
  select(city_state, term, OR, starts_with("CI")) 
```

Make a plot of these OR comparing male homicide victims to female
homicide victims.

``` r
models_results_df %>% 
  filter(term == "victim_sexMale") %>% 
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="Homework6_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />
\#\# Problem 2 \#\#\# Load and clean data

``` r
birth_df = 
  read.csv("data/birthweight.csv") %>% 
  mutate(
    babysex = as.factor(babysex), 
    mrace = as.factor(mrace),
    malform = as.factor(malform),
    frace = as.factor(frace))
```

### Check for missing values

``` r
count_na =
birth_df %>%
  map_df(~sum(is.na(.)))
```

There are no NAs in `birth_df`

I will be using a backward stepwise regression approach, which will
begin with a full model and gradually eliminate variables.

``` r
# Full model 
full_model = lm(bwt ~ ., data = birth_df)
step(full_model, direction = "backward")
```

    ## Start:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - frace     4    124365 320848704 48712
    ## - malform   1      1419 320725757 48716
    ## - ppbmi     1      6346 320730684 48716
    ## - momage    1     28661 320752999 48716
    ## - mheight   1     66886 320791224 48717
    ## - menarche  1    111679 320836018 48717
    ## - ppwt      1    131132 320855470 48718
    ## <none>                  320724338 48718
    ## - fincome   1    193454 320917792 48718
    ## - parity    1    413584 321137922 48721
    ## - mrace     3    868321 321592659 48724
    ## - babysex   1    853796 321578134 48727
    ## - gaweeks   1   4611823 325336161 48778
    ## - smoken    1   5076393 325800732 48784
    ## - delwt     1   8008891 328733230 48823
    ## - blength   1 102050296 422774634 49915
    ## - bhead     1 106535716 427260054 49961
    ## 
    ## Step:  AIC=48711.51
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - malform   1      1447 320850151 48710
    ## - ppbmi     1      6975 320855679 48710
    ## - momage    1     28379 320877083 48710
    ## - mheight   1     69502 320918206 48710
    ## - menarche  1    115708 320964411 48711
    ## - ppwt      1    133961 320982665 48711
    ## <none>                  320848704 48712
    ## - fincome   1    194405 321043108 48712
    ## - parity    1    414687 321263390 48715
    ## - babysex   1    852133 321700837 48721
    ## - gaweeks   1   4625208 325473911 48772
    ## - smoken    1   5036389 325885093 48777
    ## - delwt     1   8013099 328861802 48817
    ## - mrace     3  13540415 334389119 48885
    ## - blength   1 101995688 422844392 49908
    ## - bhead     1 106662962 427511666 49956
    ## 
    ## Step:  AIC=48709.53
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppbmi + ppwt + 
    ##     smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - ppbmi     1      6928 320857079 48708
    ## - momage    1     28660 320878811 48708
    ## - mheight   1     69320 320919470 48708
    ## - menarche  1    116027 320966177 48709
    ## - ppwt      1    133894 320984044 48709
    ## <none>                  320850151 48710
    ## - fincome   1    193784 321043934 48710
    ## - parity    1    414482 321264633 48713
    ## - babysex   1    851279 321701430 48719
    ## - gaweeks   1   4624003 325474154 48770
    ## - smoken    1   5035195 325885346 48775
    ## - delwt     1   8029079 328879230 48815
    ## - mrace     3  13553320 334403471 48883
    ## - blength   1 102009225 422859375 49906
    ## - bhead     1 106675331 427525481 49954
    ## 
    ## Step:  AIC=48707.63
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - momage    1     29211 320886290 48706
    ## - menarche  1    117635 320974714 48707
    ## <none>                  320857079 48708
    ## - fincome   1    195199 321052278 48708
    ## - parity    1    412984 321270064 48711
    ## - babysex   1    850020 321707099 48717
    ## - mheight   1   1078673 321935752 48720
    ## - ppwt      1   2934023 323791103 48745
    ## - gaweeks   1   4621504 325478583 48768
    ## - smoken    1   5039368 325896447 48773
    ## - delwt     1   8024939 328882018 48813
    ## - mrace     3  13551444 334408523 48881
    ## - blength   1 102018559 422875638 49904
    ## - bhead     1 106821342 427678421 49953
    ## 
    ## Step:  AIC=48706.02
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - menarche  1    100121 320986412 48705
    ## <none>                  320886290 48706
    ## - fincome   1    240800 321127090 48707
    ## - parity    1    431433 321317724 48710
    ## - babysex   1    841278 321727568 48715
    ## - mheight   1   1076739 321963029 48719
    ## - ppwt      1   2913653 323799943 48743
    ## - gaweeks   1   4676469 325562760 48767
    ## - smoken    1   5045104 325931394 48772
    ## - delwt     1   8000672 328886962 48811
    ## - mrace     3  14667730 335554021 48894
    ## - blength   1 101990556 422876847 49902
    ## - bhead     1 106864308 427750598 49952
    ## 
    ## Step:  AIC=48705.38
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     mheight + mrace + parity + ppwt + smoken
    ## 
    ##           Df Sum of Sq       RSS   AIC
    ## <none>                 320986412 48705
    ## - fincome  1    245637 321232048 48707
    ## - parity   1    422770 321409181 48709
    ## - babysex  1    846134 321832545 48715
    ## - mheight  1   1012240 321998651 48717
    ## - ppwt     1   2907049 323893461 48743
    ## - gaweeks  1   4662501 325648912 48766
    ## - smoken   1   5073849 326060260 48771
    ## - delwt    1   8137459 329123871 48812
    ## - mrace    3  14683609 335670021 48894
    ## - blength  1 102191779 423178191 49903
    ## - bhead    1 106779754 427766166 49950

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     gaweeks + mheight + mrace + parity + ppwt + smoken, data = birth_df)
    ## 
    ## Coefficients:
    ## (Intercept)     babysex2        bhead      blength        delwt      fincome  
    ##   -6098.822       28.558      130.777       74.947        4.107        0.318  
    ##     gaweeks      mheight       mrace2       mrace3       mrace4       parity  
    ##      11.592        6.594     -138.792      -74.887     -100.678       96.305  
    ##        ppwt       smoken  
    ##      -2.676       -4.843

The smallest AIC is 48705.38, which we will call model 1.

``` r
model_1 = lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    gaweeks + mheight + mrace + parity + ppwt + smoken, data = birth_df)

model_1 %>% 
  broom::tidy() %>% 
  select(-std.error, -statistic) %>% 
  knitr::kable()
```

| term        |       estimate |   p.value |
| :---------- | -------------: | --------: |
| (Intercept) | \-6098.8219113 | 0.0000000 |
| babysex2    |     28.5580171 | 0.0007374 |
| bhead       |    130.7770408 | 0.0000000 |
| blength     |     74.9471109 | 0.0000000 |
| delwt       |      4.1067316 | 0.0000000 |
| fincome     |      0.3180229 | 0.0688436 |
| gaweeks     |     11.5924873 | 0.0000000 |
| mheight     |      6.5940377 | 0.0002231 |
| mrace2      |  \-138.7924801 | 0.0000000 |
| mrace3      |   \-74.8867755 | 0.0768374 |
| mrace4      |  \-100.6781427 | 0.0000002 |
| parity      |     96.3046933 | 0.0170038 |
| ppwt        |    \-2.6755853 | 0.0000000 |
| smoken      |    \-4.8434197 | 0.0000000 |

### Residuals vs predicted

``` r
birth_df %>% 
  add_residuals(model_1) %>% 
  add_predictions(model_1) %>% 
  ggplot(aes(x = pred, y = resid)) + 
  geom_point(alpha = 0.5)
```

<img src="Homework6_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />
Based on the plot above, we can see that the points are all nearby each
other, scattered around a residual value of 0, with some possible
outliers.

### Comparing to other models

Now, I will create two models, one which is the main effects and the
other with a 3-way interaction.

``` r
main_mod = lm(bwt ~ blength + gaweeks, data = birth_df)
interaction_mod = lm(bwt ~ bhead * blength * babysex, data = birth_df)
```

### Cross validation of models

``` r
cross_df = 
  crossv_mc(birth_df, 100) %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )
cross_df = 
  cross_df %>% 
  mutate(
    model_1 = map(.x = train, ~lm(bwt ~ babysex + bhead + blength + delwt + fincome + 
    gaweeks + mheight + mrace + parity + ppwt + smoken, data = .x)),
    main_mod = map(.x = train, ~lm(bwt ~ blength + gaweeks, data = .x)),
    interaction_mod = map(.x = train, ~lm(bwt ~ bhead * blength * babysex, data = .x))
  ) %>% 
  mutate(
    rmse_model_1 = map2_dbl(model_1, test, ~rmse(model = .x, data = .y)),
    rmse_main_mod = map2_dbl(main_mod, test, ~rmse(model = .x, data = .y)),
    rmse_interaction_mod = map2_dbl(interaction_mod, test, ~rmse(model = .x, data = .y))
    )
```

### Now, we will compare root mean square errors between the models.

``` r
cross_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_violin(aes(fill = model)) 
```

<img src="Homework6_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />
The violin plot comparing the 3 models shows us that model\_1 has the
lowest RMSE and thus it is the best model. This model also had the
lowest AIC, as indicated above.

## Problem 3

### Read in data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/ayeshra/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2020-12-08 16:42:18 (7.536)

    ## file min/max dates: 1869-01-01 / 2020-12-31

### Using 5000 Bootstraps to produce estimates of r̂2 and log(β̂ 0∗β1)

``` r
bootstrap_results = 
  weather_df %>% 
  modelr::bootstrap(n = 5000, id = "strap_number") %>% 
  mutate(
    models = map(.x = strap, ~ lm(tmax ~ tmin, data = .x)),
    r_squared = map(models, broom::glance),
    results = map(models, broom::tidy)
  ) %>% 
  unnest(results) %>% 
  select(strap_number, term, estimate, r_squared) %>% 
  unnest(r_squared) %>% 
  select(strap_number, term, estimate, r.squared)
```

### Calculating log(β̂ 0∗β1)

``` r
log_beta = 
  bootstrap_results %>% 
  select(strap_number:estimate) %>% 
  pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    log_beta = log(intercept*tmin)
  ) %>% 
  select(strap_number, log_beta)
final_result = left_join(bootstrap_results, log_beta, by = "strap_number")
```

### Plotting the distribution of estimates

``` r
## Plot of r-squared 
final_result %>%
  filter(term == "tmin") %>% 
  ggplot(aes(x = r.squared)) +
  geom_density() +
  labs(
    title = "Distribution of r squared"
  )
```

<img src="Homework6_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />

``` r
## Plot of log(β^0∗β^1)
final_result %>% 
  filter(term == "tmin") %>% 
  ggplot(aes(x = log_beta)) +
  geom_density() +
  labs(
    title = "Distribution of log(β^0∗β^1)"
  )
```

<img src="Homework6_files/figure-gfm/unnamed-chunk-17-2.png" width="90%" />

By looking at the plots, we can see that the distribution of r-squared
appears normally distributed and slightly left skewed. The distribution
of log(β<sup>0∗β</sup>1) is centered around 2 and also looks normally
distributed with a slight left skew.

### Identify the 2.5% and 97.5% quantiles to provide a 95% confidence interval for r2 and log(β̂ 0∗β̂ 1)

``` r
final_result %>%
  group_by(term) %>%
  filter(term == "tmin") %>%
   summarize(
    rsq_lower = quantile(r.squared, 0.025),
    rsq_upper = quantile(r.squared, 0.975),
    beta_lower = quantile(log_beta, 0.025),
    beta_upper = quantile(log_beta, 0.975)
  ) %>% 
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| term | rsq\_lower | rsq\_upper | beta\_lower | beta\_upper |
| :--- | ---------: | ---------: | ----------: | ----------: |
| tmin |  0.8936105 |  0.9270779 |    1.964851 |    2.058871 |

The 95% CI for r-squared is 0.8943 to 0.9272 The 95% CI for
log(β<sup>0∗β</sup>1) is 1.9647 to 2.0585
