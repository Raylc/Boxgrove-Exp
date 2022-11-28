# Boxgrove-Exp
Dissecting the interaction between skill level and mental template in Late Acheulean biface morphology: Archaeological and experimental insights (code, data, manuscript).

## Getting Started
Please open the Rproj file instead of the Rcode directly to make sure relative paths work!

## File Structure
The repository is organised into four main directories: code, data, figure, and manuscript.

### code
* `Boxgrove_Iovita.R` ... This is the R script generating the main results and figures of this paper, including the compilation of Boxgrove morphonetric data and the comparative analysis of Boxgrove and experimental data.

### data
* `Boxgrove_Iovita`... This folder includes two subfolder. Subfolder `Sillhouettes` contains sillhouettes of the Boxgrove handaxe assemblage generated through Adobe Photoshop. Subfolder `Measurements` contains morphometric measurements of sillhouettes generated through ImageJ.
* `Experiment`... This folder includes data of experimental handaxe collections.
* `Table1.csv`... This csv file is the output file for Table 1 in the manuscript.

### figure
* `Fig1.png` ... This is Fig.1 in the manuscript.
* `Fig2.png` ... This is Fig.2 in the manuscript.
* `Fig3.png` ... This is Fig.3 in the manuscript.
* `Fig4.png` ... This is Fig.4 in the manuscript.
* `Fig5.png` ... This is Fig.5 in the manuscript.
* `Fig6.png` ... This is Fig.6 in the manuscript.
* `Fig7.png` ... This is Fig.7 in the manuscript.
* `Fig8.png` ... This is Fig.8 in the manuscript.

### manuscript
* `apa.csl` ... This is the APA citation styleguide.
* `bibliography.bib` ... This is the reference file.
* `manuscript.pdf` ... This is the pdf file of the manuscript.
* `manuscript.Rmd` ... This is the RMarkdown file of the manuscript.

## Dependencies
The code has been successfully executed on on CL's PC with the following R settings.

* CL's R setting
 ``` 
R version 4.1.1 (2021-08-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.7             svglite_2.0.0          here_1.0.1            
 [4] mvtnorm_1.1-2          lattice_0.20-44        zoo_1.8-9             
 [7] assertthat_0.2.1       zeallot_0.1.0          rprojroot_2.0.2       
[10] digest_0.6.27          utf8_1.2.2             correlation_0.8.0     
[13] R6_2.5.1               plyr_1.8.6             evaluate_0.14         
[16] coda_0.19-4            httr_1.4.2             ggplot2_3.3.5         
[19] pillar_1.8.0           rlang_1.0.4            multcomp_1.4-17       
[22] performance_0.9.0      rstudioapi_0.13        Matrix_1.3-4          
[25] rmarkdown_2.10         splines_4.1.1          webshot_0.5.2         
[28] stringr_1.4.0          munsell_0.5.0          compiler_4.1.1        
[31] xfun_0.25              pkgconfig_2.0.3        systemfonts_1.0.2     
[34] parameters_0.17.0      ggstatsplot_0.9.1      WRS2_1.1-3            
[37] htmltools_0.5.1.1      insight_0.17.0         tidyselect_1.1.1      
[40] tibble_3.1.3           bookdown_0.23          codetools_0.2-18      
[43] reshape_0.8.8          fansi_0.5.0            viridisLite_0.4.0     
[46] dplyr_1.0.7            MASS_7.3-54            mc2d_0.1-21           
[49] grid_4.1.1             xtable_1.8-4           gtable_0.3.0          
[52] lifecycle_1.0.1        DBI_1.1.1              magrittr_2.0.1        
[55] bayestestR_0.11.5      statsExpressions_1.3.1 scales_1.2.0          
[58] datawizard_0.4.0       estimability_1.3       cli_3.0.1             
[61] stringi_1.7.3          paletteer_1.4.0        xml2_1.3.2            
[64] ellipsis_0.3.2         generics_0.1.0         vctrs_0.3.8           
[67] sandwich_3.0-1         TH.data_1.0-10         kableExtra_1.3.4      
[70] rematch2_2.1.2         tools_4.1.1            glue_1.4.2            
[73] purrr_0.3.4            emmeans_1.6.2-1        yaml_2.2.1            
[76] survival_3.2-11        colorspace_2.0-2       rvest_1.0.1           
[79] knitr_1.33             patchwork_1.1.1          
 ``` 

## Help

Please contact raylc1996@outlook.com if you have any questions realted to the code.