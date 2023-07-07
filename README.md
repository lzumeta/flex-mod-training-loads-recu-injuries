# README

This is the accompanying code repository for the manuscript 

*"Flexible modelling of time-varying exposures and recurrent events to analyze training loads and injuries"*.


## Session info
Our R session information is given below:
```
sessionInfo()
R version 4.2.3 (2023-03-15)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.6 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=es_ES.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=es_ES.UTF-8   
 [6] LC_MESSAGES=en_US.UTF-8    LC_PAPER=es_ES.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=es_ES.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] injurytools_1.0.1 extraDistr_1.9.1  tsModel_0.6-1     dlnm_2.4.7        dplyr_1.1.2       patchwork_1.1.1   stringr_1.5.0    
 [8] ggpubr_0.4.0      latex2exp_0.9.6   kableExtra_1.3.4  knitr_1.42        gridExtra_2.3     tidyr_1.3.0       ggplot2_3.4.2    
[15] batchtools_0.9.16

loaded via a namespace (and not attached):
  [1] colorspace_2.1-0  ggsignif_0.6.2    ellipsis_0.3.2    rio_0.5.27        rprojroot_2.0.3   fs_1.6.2          rstudioapi_0.14  
  [8] remotes_2.4.2     lubridate_1.9.2   fansi_1.0.4       xml2_1.3.3        splines_4.2.3     cachem_1.0.8      pkgload_1.3.0    
 [15] broom_1.0.1       shiny_1.7.3       compiler_4.2.3    httr_1.4.4        backports_1.4.1   Matrix_1.5-3      fastmap_1.1.1    
 [22] cli_3.6.1         later_1.3.0       htmltools_0.5.5   prettyunits_1.1.1 tools_4.2.3       gtable_0.3.3      glue_1.6.2       
 [29] rappdirs_0.3.3    Rcpp_1.0.10       carData_3.0-4     cellranger_1.1.0  vctrs_0.6.2       debugme_1.1.0     svglite_2.0.0    
 [36] nlme_3.1-162      metR_0.14.0       xfun_0.39         ps_1.7.2          openxlsx_4.2.4    rvest_1.0.2       timechange_0.2.0 
 [43] mime_0.12         miniUI_0.1.1.1    lifecycle_1.0.3   sys_3.4.1         devtools_2.4.4    rstatix_0.7.0     scales_1.2.1     
 [50] hms_1.1.2         promises_1.2.0.1  credentials_1.3.2 gert_1.6.0        curl_5.0.0        memoise_2.0.1     stringi_1.7.12   
 [57] checkmate_2.2.0   pkgbuild_1.3.1    zip_2.2.1         rlang_1.1.1       pkgconfig_2.0.3   systemfonts_1.0.3 evaluate_0.21    
 [64] lattice_0.20-45   purrr_1.0.1       htmlwidgets_1.6.2 tidyselect_1.2.0  processx_3.8.0    magrittr_2.0.3    R6_2.5.1         
 [71] generics_0.1.3    profvis_0.3.7     base64url_1.4     pillar_1.9.0      haven_2.5.0       foreign_0.8-81    withr_2.5.0      
 [78] mgcv_1.8-42       abind_1.4-5       tibble_3.2.1      crayon_1.5.2      car_3.0-11        utf8_1.2.3        rmarkdown_2.21   
 [85] urlchecker_1.0.1  progress_1.2.2    usethis_2.1.6     grid_4.2.3        readxl_1.4.0      data.table_1.14.8 callr_3.7.2      
 [92] forcats_1.0.0     digest_0.6.31     webshot_0.5.2     xtable_1.8-4      httpuv_1.6.6      brew_1.0-8        openssl_2.0.5    
 [99] munsell_0.5.0     viridisLite_0.4.2 sessioninfo_1.2.2 askpass_1.1

devtools::session_info()
─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.3 (2023-03-15)
 os       Ubuntu 18.04.6 LTS
 system   x86_64, linux-gnu
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/Madrid
 date     2023-07-07
 rstudio  2023.06.0+421 Mountain Hydrangea (desktop)
 pandoc   3.1.1 @ /usr/lib/rstudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
 package     * version date (UTC) lib source
 abind         1.4-5   2016-07-21 [1] CRAN (R 4.1.0)
 askpass       1.1     2019-01-13 [1] CRAN (R 4.1.0)
 backports     1.4.1   2021-12-13 [1] CRAN (R 4.1.1)
 base64url     1.4     2018-05-14 [1] CRAN (R 4.1.1)
 batchtools  * 0.9.16  2023-02-03 [1] CRAN (R 4.2.2)
 brew          1.0-8   2022-09-29 [1] CRAN (R 4.2.1)
 broom         1.0.1   2022-08-29 [1] CRAN (R 4.2.2)
 cachem        1.0.8   2023-05-01 [1] CRAN (R 4.2.3)
 callr         3.7.2   2022-08-22 [1] CRAN (R 4.2.1)
 car           3.0-11  2021-06-27 [1] CRAN (R 4.1.0)
 carData       3.0-4   2020-05-22 [1] CRAN (R 4.1.0)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.1.0)
 checkmate     2.2.0   2023-04-27 [1] CRAN (R 4.2.3)
 cli           3.6.1   2023-03-23 [1] CRAN (R 4.2.3)
 colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.2.2)
 crayon        1.5.2   2022-09-29 [1] CRAN (R 4.2.1)
 credentials   1.3.2   2021-11-29 [1] CRAN (R 4.1.1)
 curl          5.0.0   2023-01-12 [1] CRAN (R 4.2.2)
 data.table    1.14.8  2023-02-17 [1] CRAN (R 4.2.2)
 debugme       1.1.0   2017-10-22 [1] CRAN (R 4.2.2)
 devtools      2.4.4   2022-07-20 [1] CRAN (R 4.2.1)
 digest        0.6.31  2022-12-11 [1] CRAN (R 4.2.2)
 dlnm        * 2.4.7   2021-10-07 [1] CRAN (R 4.2.2)
 dplyr       * 1.1.2   2023-04-20 [1] CRAN (R 4.2.3)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.0)
 evaluate      0.21    2023-05-05 [1] CRAN (R 4.2.3)
 extraDistr  * 1.9.1   2020-09-07 [1] CRAN (R 4.1.0)
 fansi         1.0.4   2023-01-22 [1] CRAN (R 4.2.2)
 fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.2.2)
 forcats       1.0.0   2023-01-29 [1] CRAN (R 4.2.2)
 foreign       0.8-81  2020-12-22 [1] CRAN (R 4.1.0)
 fs            1.6.2   2023-04-25 [1] CRAN (R 4.2.3)
 generics      0.1.3   2022-07-05 [1] CRAN (R 4.2.0)
 gert          1.6.0   2022-03-29 [1] CRAN (R 4.1.1)
 ggplot2     * 3.4.2   2023-04-03 [1] CRAN (R 4.2.3)
 ggpubr      * 0.4.0   2020-06-27 [1] CRAN (R 4.1.0)
 ggsignif      0.6.2   2021-06-14 [1] CRAN (R 4.1.0)
 glue          1.6.2   2022-02-24 [1] CRAN (R 4.1.1)
 gridExtra   * 2.3     2017-09-09 [1] CRAN (R 4.2.0)
 gtable        0.3.3   2023-03-21 [1] CRAN (R 4.2.3)
 haven         2.5.0   2022-04-15 [1] CRAN (R 4.2.0)
 hms           1.1.2   2022-08-19 [1] CRAN (R 4.2.2)
 htmltools     0.5.5   2023-03-23 [1] CRAN (R 4.2.3)
 htmlwidgets   1.6.2   2023-03-17 [1] CRAN (R 4.2.3)
 httpuv        1.6.6   2022-09-08 [1] CRAN (R 4.2.1)
 httr          1.4.4   2022-08-17 [1] CRAN (R 4.2.1)
 injurytools * 1.0.1   2023-07-06 [1] Github (lzumeta/injurytools@eb34d69)
 kableExtra  * 1.3.4   2021-02-20 [1] CRAN (R 4.1.0)
 knitr       * 1.42    2023-01-25 [1] CRAN (R 4.2.3)
 later         1.3.0   2021-08-18 [1] CRAN (R 4.2.1)
 latex2exp   * 0.9.6   2022-11-28 [1] CRAN (R 4.2.2)
 lattice       0.20-45 2021-09-22 [1] CRAN (R 4.2.0)
 lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.2.1)
 lubridate     1.9.2   2023-02-10 [1] CRAN (R 4.2.2)
 magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.1.1)
 Matrix        1.5-3   2022-11-11 [4] CRAN (R 4.2.2)
 memoise       2.0.1   2021-11-26 [1] CRAN (R 4.1.1)
 metR          0.14.0  2023-03-23 [1] CRAN (R 4.2.3)
 mgcv          1.8-42  2023-03-02 [4] CRAN (R 4.2.3)
 mime          0.12    2021-09-28 [1] CRAN (R 4.1.1)
 miniUI        0.1.1.1 2018-05-18 [1] CRAN (R 4.1.0)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 4.1.0)
 nlme          3.1-162 2023-01-31 [4] CRAN (R 4.2.2)
 openssl       2.0.5   2022-12-06 [1] CRAN (R 4.2.2)
 openxlsx      4.2.4   2021-06-16 [1] CRAN (R 4.1.0)
 patchwork   * 1.1.1   2020-12-17 [1] CRAN (R 4.1.1)
 pillar        1.9.0   2023-03-22 [1] CRAN (R 4.2.3)
 pkgbuild      1.3.1   2021-12-20 [1] CRAN (R 4.1.1)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.0)
 pkgload       1.3.0   2022-06-27 [1] CRAN (R 4.2.0)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.1.0)
 processx      3.8.0   2022-10-26 [1] CRAN (R 4.2.1)
 profvis       0.3.7   2020-11-02 [1] CRAN (R 4.2.1)
 progress      1.2.2   2019-05-16 [1] CRAN (R 4.1.0)
 promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.1.0)
 ps            1.7.2   2022-10-26 [1] CRAN (R 4.2.1)
 purrr         1.0.1   2023-01-10 [1] CRAN (R 4.2.2)
 R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.1)
 rappdirs      0.3.3   2021-01-31 [1] CRAN (R 4.1.0)
 Rcpp          1.0.10  2023-01-22 [1] CRAN (R 4.2.2)
 readxl        1.4.0   2022-03-28 [1] CRAN (R 4.2.0)
 remotes       2.4.2   2021-11-30 [1] CRAN (R 4.1.1)
 rio           0.5.27  2021-06-21 [1] CRAN (R 4.1.0)
 rlang         1.1.1   2023-04-28 [1] CRAN (R 4.2.3)
 rmarkdown     2.21    2023-03-26 [1] CRAN (R 4.2.3)
 rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.1.1)
 rstatix       0.7.0   2021-02-13 [1] CRAN (R 4.1.0)
 rstudioapi    0.14    2022-08-22 [1] CRAN (R 4.2.1)
 rvest         1.0.2   2021-10-16 [1] CRAN (R 4.1.1)
 scales        1.2.1   2022-08-20 [1] CRAN (R 4.2.0)
 sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.1)
 shiny         1.7.3   2022-10-25 [1] CRAN (R 4.2.1)
 stringi       1.7.12  2023-01-11 [1] CRAN (R 4.2.2)
 stringr     * 1.5.0   2022-12-02 [1] CRAN (R 4.2.2)
 svglite       2.0.0   2021-02-20 [1] CRAN (R 4.1.0)
 sys           3.4.1   2022-10-18 [1] CRAN (R 4.2.2)
 systemfonts   1.0.3   2021-10-13 [1] CRAN (R 4.1.1)
 tibble        3.2.1   2023-03-20 [1] CRAN (R 4.2.3)
 tidyr       * 1.3.0   2023-01-24 [1] CRAN (R 4.2.2)
 tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.2.1)
 timechange    0.2.0   2023-01-11 [1] CRAN (R 4.2.2)
 tsModel     * 0.6-1   2022-05-10 [1] CRAN (R 4.2.2)
 urlchecker    1.0.1   2021-11-30 [1] CRAN (R 4.2.1)
 usethis       2.1.6   2022-05-25 [1] CRAN (R 4.2.1)
 utf8          1.2.3   2023-01-31 [1] CRAN (R 4.2.2)
 vctrs         0.6.2   2023-04-19 [1] CRAN (R 4.2.3)
 viridisLite   0.4.2   2023-05-02 [1] CRAN (R 4.2.3)
 webshot       0.5.2   2019-11-22 [1] CRAN (R 4.1.0)
 withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.1)
 xfun          0.39    2023-04-20 [1] CRAN (R 4.2.3)
 xml2          1.3.3   2021-11-30 [1] CRAN (R 4.1.1)
 xtable        1.8-4   2019-04-21 [1] CRAN (R 4.1.0)
 zip           2.2.1   2022-09-08 [1] CRAN (R 4.2.1)

 [1] /home/lzumeta/R/x86_64-pc-linux-gnu-library/4.2
 [2] /usr/local/lib/R/site-library
 [3] /usr/lib/R/site-library
 [4] /usr/lib/R/library

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
```
