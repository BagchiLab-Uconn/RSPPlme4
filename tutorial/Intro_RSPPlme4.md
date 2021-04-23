Introduction to the RSPPlme4 package
================
Robert Bagchi
August 25, 2020

  - [Introduction](#introduction)
      - [Session Information](#session-information)

If you need to install the package, the easiest way is to do so directly
from github using the devtools package. Once devtools has been
installed, the package can be installed with ‘r
devtools::install\_github(“BagchiLab-Uconn/RSPPlme4”)’.

``` r

library(tidyverse)
library(RSPPlme4)
```

# Introduction

## Session Information

``` 
R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggthemes_4.2.0  RSPPlme4_0.0.1  forcats_0.5.0   stringr_1.4.0  
 [5] dplyr_1.0.2     purrr_0.3.4     readr_1.3.1     tidyr_1.1.1    
 [9] tibble_3.0.3    ggplot2_3.3.2   tidyverse_1.3.0

loaded via a namespace (and not attached):
 [1] nlme_3.1-149          fs_1.5.0              usethis_1.6.1        
 [4] lubridate_1.7.9       devtools_2.3.1        httr_1.4.2           
 [7] rprojroot_1.3-2       tools_3.6.3           backports_1.1.9      
[10] R6_2.4.1              rpart_4.1-15          mgcv_1.8-32          
[13] DBI_1.1.0             colorspace_1.4-1      withr_2.2.0          
[16] tidyselect_1.1.0      prettyunits_1.1.1     processx_3.4.3       
[19] curl_4.3              compiler_3.6.3        cli_2.0.2            
[22] rvest_0.3.6           xml2_1.3.2            desc_1.2.0           
[25] scales_1.1.1          spatstat.data_1.4-3   callr_3.4.3          
[28] goftest_1.2-2         spatstat_1.64-1       digest_0.6.25        
[31] spatstat.utils_1.17-0 minqa_1.2.4           rmarkdown_2.3        
[34] pkgconfig_2.0.3       htmltools_0.5.0       lme4_1.1-23          
[37] sessioninfo_1.1.1     dbplyr_1.4.4          rlang_0.4.7          
[40] readxl_1.3.1          rstudioapi_0.11       generics_0.0.2       
[43] jsonlite_1.7.0        magrittr_1.5          Matrix_1.2-18        
[46] Rcpp_1.0.5            munsell_0.5.0         fansi_0.4.1          
[49] abind_1.4-5           lifecycle_0.2.0       stringi_1.4.6        
[52] yaml_2.2.1            MASS_7.3-52           pkgbuild_1.1.0       
[55] grid_3.6.3            blob_1.2.1            parallel_3.6.3       
[58] crayon_1.3.4          deldir_0.1-28         lattice_0.20-41      
[61] haven_2.3.1           splines_3.6.3         tensor_1.5           
[64] hms_0.5.3             knitr_1.29            ps_1.3.4             
[67] pillar_1.4.6          boot_1.3-25           codetools_0.2-16     
[70] pkgload_1.1.0         reprex_0.3.0          glue_1.4.1           
[73] evaluate_0.14         remotes_2.2.0         modelr_0.1.8         
[76] vctrs_0.3.2           nloptr_1.2.2.2        testthat_2.3.2       
[79] cellranger_1.1.0      polyclip_1.10-0       gtable_0.3.0         
[82] assertthat_0.2.1      xfun_0.16             broom_0.7.0          
[85] memoise_1.1.0         statmod_1.4.34        ellipsis_0.3.1       
```
