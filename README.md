# README

This is the accompanying code repository for the research paper:

*"Flexible modelling of time-varying exposures and recurrent events to analyze training loads effects in team sports injuries"*.


## Session info
The R session information is given below:
```
sessionInfo()
R version 4.3.1 (2023-06-16)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.6 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1 
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=es_ES.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=es_ES.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=es_ES.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=es_ES.UTF-8 LC_IDENTIFICATION=C       

time zone: Europe/Madrid
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.3.1    magrittr_2.0.3    cli_3.6.1         tools_4.3.1      
[5] rstudioapi_0.15.0 vctrs_0.6.3       lifecycle_1.0.3   rlang_1.1.1      
[9] purrr_1.0.1  

devtools::session_info()
─ Session info ─────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.3.1 (2023-06-16)
 os       Ubuntu 18.04.6 LTS
 system   x86_64, linux-gnu
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Madrid
 date     2023-08-09
 rstudio  2023.06.0+421 Mountain Hydrangea (desktop)
 pandoc   1.19.2.4 @ /usr/bin/pandoc

─ Packages ─────────────────────────────────────────────────────────────────────
 package     * version date (UTC) lib source
 cachem        1.0.8   2023-05-01 [1] CRAN (R 4.3.1)
 callr         3.7.3   2022-11-02 [1] CRAN (R 4.3.1)
 cli           3.6.1   2023-03-23 [1] CRAN (R 4.3.1)
 crayon        1.5.2   2022-09-29 [1] CRAN (R 4.3.1)
 devtools      2.4.5   2022-10-11 [1] CRAN (R 4.3.1)
 digest        0.6.33  2023-07-07 [1] CRAN (R 4.3.1)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.3.1)
 fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.3.1)
 fs            1.6.3   2023-07-20 [1] CRAN (R 4.3.1)
 glue          1.6.2   2022-02-24 [1] CRAN (R 4.3.1)
 htmltools     0.5.5   2023-03-23 [1] CRAN (R 4.3.1)
 htmlwidgets   1.6.2   2023-03-17 [1] CRAN (R 4.3.1)
 httpuv        1.6.11  2023-05-11 [1] CRAN (R 4.3.1)
 later         1.3.1   2023-05-02 [1] CRAN (R 4.3.1)
 lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.3.1)
 magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.3.1)
 memoise       2.0.1   2021-11-26 [1] CRAN (R 4.3.1)
 mime          0.12    2021-09-28 [1] CRAN (R 4.3.1)
 miniUI        0.1.1.1 2018-05-18 [1] CRAN (R 4.3.1)
 pkgbuild      1.4.2   2023-06-26 [1] CRAN (R 4.3.1)
 pkgload       1.3.2.1 2023-07-08 [1] CRAN (R 4.3.1)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.3.1)
 processx      3.8.2   2023-06-30 [1] CRAN (R 4.3.1)
 profvis       0.3.8   2023-05-02 [1] CRAN (R 4.3.1)
 promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.3.1)
 ps            1.7.5   2023-04-18 [1] CRAN (R 4.3.1)
 purrr         1.0.1   2023-01-10 [1] CRAN (R 4.3.1)
 R6            2.5.1   2021-08-19 [1] CRAN (R 4.3.1)
 Rcpp          1.0.11  2023-07-06 [1] CRAN (R 4.3.1)
 remotes       2.4.2.1 2023-07-18 [1] CRAN (R 4.3.1)
 rlang         1.1.1   2023-04-28 [1] CRAN (R 4.3.1)
 rstudioapi    0.15.0  2023-07-07 [1] CRAN (R 4.3.1)
 sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.3.1)
 shiny         1.7.4.1 2023-07-06 [1] CRAN (R 4.3.1)
 stringi       1.7.12  2023-01-11 [1] CRAN (R 4.3.1)
 stringr       1.5.0   2022-12-02 [1] CRAN (R 4.3.1)
 urlchecker    1.0.1   2021-11-30 [1] CRAN (R 4.3.1)
 usethis       2.2.2   2023-07-06 [1] CRAN (R 4.3.1)
 vctrs         0.6.3   2023-06-14 [1] CRAN (R 4.3.1)
 xtable        1.8-4   2019-04-21 [1] CRAN (R 4.3.1)

 [1] /home/lzumeta/R/x86_64-pc-linux-gnu-library/4.3
 [2] /usr/local/lib/R/site-library
 [3] /usr/lib/R/site-library
 [4] /usr/lib/R/library

────────────────────────────────────────────────────────────────────────────────
```
