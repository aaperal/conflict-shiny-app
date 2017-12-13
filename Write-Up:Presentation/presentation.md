Visualizing Conflict
========================================================
author: Alan Peral and Kadija Yilla
date: December 12, 2017
autosize: true



Motivation
========================================================

- Differences in armed conflict in Western countries vs. Global South
- Quality of life vs. armed conflict
- Relationship amongst warring sides

SES Data Set
========================================================


```
# A tibble: 5 x 6
   wbid     country  year      SES    gdppc    popshare
  <chr>       <chr> <int>    <dbl>    <dbl>       <dbl>
1   AFG Afghanistan  2010 5.676400 1662.803 0.004149612
2   AFG Afghanistan  2000 2.061114  565.000 0.003308824
3   AFG Afghanistan  1990 1.269530  604.000 0.002346751
4   AFG Afghanistan  1980 3.465048  690.000 0.003057307
5   AFG Afghanistan  1970 3.474212  709.000 0.003097430
```

World Bank Data Set
========================================================

- Extracted using an R package titled `WDI`
- Choose 17 Indicators
  - Health expenditure, total (% of GDP)
  - Mortality rate, under-5 (per 1000 live births)
  - Refugee population by country or territory of asylum
  - Refugee population by country or territory of origin
  - Access to electricity (% of population)
  
World Bank Data Set
========================================================
title: false

```
     country year fetility_rate life_expectancy_female
1 Arab World 1960      6.948747               47.90606
2 Arab World 1961      6.971370               48.48539
3 Arab World 1962      6.992350               49.06783
4 Arab World 1963      7.009265               49.65889
5 Arab World 1964      7.020105               50.26037
```
Armed Conflict Set
==================================
- Uppsala Conflict Data Program  (UCDP) at the department of Peace and Conflict Research
- Uppsala University and the Centre for the Study of Civil War at the Peace Research Institute Oslo (PRIO)
- Internal and external conflict from 1946 to the present

“A contested incompatibility that concerns government and/or
territory where the use of armed force between two parties, of which at least one is the
government of a state, results in at least 25 battle-related deaths in a calendar year"

Armed Conflict Set
==================================
title: false

```
# A tibble: 5 x 4
              location                 sidea
                 <chr>                 <chr>
1              Bolivia Government of Bolivia
2              Bolivia Government of Bolivia
3              Bolivia Government of Bolivia
4 Cambodia (Kampuchea)  Government of France
5 Cambodia (Kampuchea)  Government of France
# ... with 2 more variables: `side b` <chr>, year <int>
```
Wrangling Steps
==================================
- Merging conflicts
- Created a function to speperate and replicate values
- Renamed countries based on familiarity
- Removed specific regions in WBD set



Wrangling Example
==================================
Example of wrong value:

```
# A tibble: 5 x 4
         location               sidea               `side b`  year
            <chr>               <chr>                  <chr> <int>
1 India, Pakistan Government of India Government of Pakistan  1948
2 India, Pakistan Government of India Government of Pakistan  1964
3 India, Pakistan Government of India Government of Pakistan  1965
4 India, Pakistan Government of India Government of Pakistan  1971
5 India, Pakistan Government of India Government of Pakistan  1984
```
Wrangling Example (Con't)
==================================
Changed to:

```
# A tibble: 5 x 4
  location               sidea               `side b`  year
     <chr>               <chr>                  <chr> <int>
1    India Government of India Government of Pakistan  1948
2 Pakistan Government of India Government of Pakistan  1948
3    India Government of India Government of Pakistan  1964
4 Pakistan Government of India Government of Pakistan  1964
5    India Government of India Government of Pakistan  1965
```
Final Set
==============================

```
        location year                     sidea
1    Afghanistan 1990 Government of Afghanistan
2        Albania 1990                      <NA>
3        Algeria 1990                      <NA>
4 American Samoa 1990                      <NA>
5        Andorra 1990                      <NA>
  labor_force_Particpation_rate      SES
1                        16.442  1.26953
2                        53.155 72.88290
3                         9.925 56.70537
4                            NA       NA
5                            NA       NA
```
Limitations
========================
- Armed conflict set is under-reported
- Singular attacks won't have a grand effect on indicators
- Time

Shiny App
=========================
Final Product:

https://yillak.shinyapps.io/armed_conflicts2/

