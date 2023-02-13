---
title:         "Trends of SDR in Thessaloniki "
author:
  - Natsis Athanasios^[Laboratory of Atmospheric Physics,AUTH, natsisthanasis@gmail.com]
  - Jane Doe^[Institution Two, jane@example.org]
abstract:
  "Study of GHI and DNI radiation for 'clear sky' and all sly conditions."

documentclass:  article
classoption:    a4paper,oneside
fontsize:       10pt
geometry:       "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
link-citations: yes
colorlinks:     yes

header-includes:
- \usepackage{caption}
- \usepackage{placeins}
- \captionsetup{font=small}

output:
  html_document:
    toc:        true
    keep_md:    yes
    fig_width:  7.5
    fig_height: 5
  bookdown::pdf_document2:
    number_sections:  no
    fig_caption:      no
    keep_tex:         yes
    latex_engine:     xelatex
    toc:              yes
    toc_depth:        4
    fig_width:        7
    fig_height:       4.5

date: "2023-02-13"

---





```


Load environment and data from:  ~/MANUSCRIPTS/2022_sdr_trends/data/common_data_14.Rds 
```



### Data info

Time data span 1993-04-12, 2022-06-30

Where is a **running mean the window is 1095.7275 days** or
3 years.

## 3. Consistency of trends





#### Calculate seasonal anomaly ####





\newpage
## Cumulative sums

Use deseasonalized monthly values to calculate cumulative sums



<img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesums-1.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesums-2.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesums-3.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesums-4.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesums-5.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesums-6.png" width="100%" style="display: block; margin: auto;" />




```r
plotsza <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","daily")
plotpNcol   <- c(2,3,4,5)
vars        <- c("DIR_att", "DIR_transp")
database    <- c("ALL_3_monthly_cumsum", "CLEAR_3_monthly_cumsum")
```
<img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesumsdir-1.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesumsdir-2.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesumsdir-3.png" width="100%" style="display: block; margin: auto;" /><img src="DHI_GHI_3_trends_consistency_files/figure-html/cumulativesumsdir-4.png" width="100%" style="display: block; margin: auto;" />


**END**


```
2023-02-13 10:36:21.3 athan@tyler Climatological_ 52.192160 mins
```

