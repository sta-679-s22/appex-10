Appex 10
================
Lucy D’Agostino McGowan
2022-03-23

``` r
library(tidyverse)
library(MatchIt)
library(broom)
library(smd)
#install.packages("smd")
```

``` r
customer_satisfaction <- read_csv("customer_satisfaction.csv")
```

``` r
customer_satisfaction <- glm(
  satisfied_customer_service ~ gender + 
    education + married + n_kids + geographic_location + 
    previous_spend + new_customer + has_pets + former_customer_service, 
  data = customer_satisfaction, family = binomial) %>%
  augment(customer_satisfaction, type.predict = "response") %>%
  mutate(wt_ate = satisfied_customer_service / .fitted + 
           (1 - satisfied_customer_service) / (1 - .fitted),
         wt_ato = (1 - satisfied_customer_service) * .fitted + 
           satisfied_customer_service * (1 - .fitted))
```

``` r
customer_satisfaction %>%
  summarize(
    across(c(gender, age, education, geographic_location, income, married, 
             n_kids, has_pets, former_customer_service, previous_spend,
             new_customer),
           list(
             unweighted = ~smd(.x, satisfied_customer_service)$estimate,
             ate.weighted = ~smd(.x, satisfied_customer_service, w = wt_ate)$estimate,
             ato.weighted = ~smd(.x, satisfied_customer_service, w = wt_ato)$estimate
           ))
  )
```

    ## # A tibble: 1 × 33
    ##   gender_unweighted gender_ate.weighted gender_ato.weighted age_unweighted
    ##               <dbl>               <dbl>               <dbl>          <dbl>
    ## 1            0.0161              0.0106            6.67e-13          0.340
    ## # … with 29 more variables: age_ate.weighted <dbl>, age_ato.weighted <dbl>,
    ## #   education_unweighted <dbl>, education_ate.weighted <dbl>,
    ## #   education_ato.weighted <dbl>, geographic_location_unweighted <dbl>,
    ## #   geographic_location_ate.weighted <dbl>,
    ## #   geographic_location_ato.weighted <dbl>, income_unweighted <dbl>,
    ## #   income_ate.weighted <dbl>, income_ato.weighted <dbl>,
    ## #   married_unweighted <dbl>, married_ate.weighted <dbl>, …
