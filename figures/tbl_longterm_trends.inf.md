<!-- This is a markdown file. -->


 General info.
---------------

Object name:    object      
Date written:   2022-10-12 11:17:49.7 UTC  
Data file:      /home/athan/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat      
Data file size: 1.1 KiB (1121) 
Size in memory: 3848 bytes      
Info file:      /home/athan/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.inf.md      
Script name:    /home/athan/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_1_longterm_trends.R      
User@Host:      athan@sagan   
Contact:        <lapauththanasis@gmail.com>      
Notes:          NA      


 Data structure.
-----------------

```
Classes ‘data.table’ and 'data.frame':	8 obs. of  9 variables:
 $ var               : chr  "DIR_att" "DIR_transp" "GLB_att" "HOR_att" ...
 $ data              : chr  "ALL" "ALL" "ALL" "ALL" ...
 $ slope             : num  0.0477 0.3662 0.4425 0.1959 0.2564 ...
 $ slope.sd          : num  0.8802 0.968 0.0435 0.8822 0.1814 ...
 $ slope.t           : num  0.0542 0.3783 10.1629 0.2221 1.4138 ...
 $ slope.p           : num  9.57e-01 7.05e-01 3.80e-24 8.24e-01 1.58e-01 ...
 $ Rsqrd             : num  1.41e-06 6.85e-05 1.02e-02 2.36e-05 1.46e-03 ...
 $ RsqrdAdj          : num  -0.000478 -0.00041 0.010119 -0.000455 0.00073 ...
 $ slope.ConfInt_0.95: num  0.004726 0.005197 0.000234 0.004737 0.000974 ...
 - attr(*, ".internal.selfref")=<externalptr> 
```


 Data quality.
---------------

| &nbsp; | var | data | slope | slope.sd | slope.t | slope.p | Rsqrd | RsqrdAdj | slope.ConfInt_0.95 |
|:------:|----:|-----:|------:|---------:|--------:|--------:|------:|---------:|-------------------:|
| Values |   0 |    0 |     8 |        8 |       8 |       8 |     8 |        8 |                  8 |
|  INFs  |   0 |    0 |     0 |        0 |       0 |       0 |     0 |        0 |                  0 |
|  NAs   |   0 |    0 |     0 |        0 |       0 |       0 |     0 |        0 |                  0 |


 Data Summary.
---------------

|              var |             data |            slope |         slope.sd |          slope.t |          slope.p |
|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|-----------------:|
|         Length:8 |         Length:8 | Min.   :-0.11028 | Min.   :0.001882 | Min.   :-2.66912 | Min.   :0.000000 |
| Class :character | Class :character | 1st Qu.: 0.03451 | 1st Qu.:0.039761 | 1st Qu.:-0.04681 | 1st Qu.:0.005771 |
| Mode  :character | Mode  :character | Median : 0.22619 | Median :0.248339 | Median : 0.30021 | Median :0.431438 |
|               NA |               NA | Mean   : 0.19713 | Mean   :0.412611 | Mean   : 2.83876 | Mean   :0.422275 |
|               NA |               NA | 3rd Qu.: 0.37053 | 3rd Qu.:0.880688 | 3rd Qu.: 3.60108 | 3rd Qu.:0.750992 |
|               NA |               NA | Max.   : 0.44252 | Max.   :0.967957 | Max.   :13.49769 | Max.   :0.956802 |

 

|             Rsqrd |           RsqrdAdj | slope.ConfInt_0.95 |
|------------------:|-------------------:|-------------------:|
| Min.   :1.406e-06 | Min.   :-0.0006424 |  Min.   :1.011e-05 |
| 1st Qu.:5.731e-05 | 1st Qu.:-0.0004609 |  1st Qu.:2.134e-04 |
| Median :7.754e-04 | Median : 0.0001599 |  Median :1.334e-03 |
| Mean   :5.498e-03 | Mean   : 0.0050132 |  Mean   :2.216e-03 |
| 3rd Qu.:6.446e-03 | 3rd Qu.: 0.0058747 |  3rd Qu.:4.729e-03 |
| Max.   :2.693e-02 | Max.   : 0.0267824 |  Max.   :5.197e-03 |



  * **var**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |      8 | character | character |

  * **data**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |      8 | character | character |

  * **slope**:


    |    Min. | 1st Qu. | Median |   Mean | 3rd Qu. |   Max. |
    |--------:|--------:|-------:|-------:|--------:|-------:|
    | -0.1103 | 0.03451 | 0.2262 | 0.1971 |  0.3705 | 0.4425 |

  * **slope.sd**:


    |     Min. | 1st Qu. | Median |   Mean | 3rd Qu. |  Max. |
    |---------:|--------:|-------:|-------:|--------:|------:|
    | 0.001882 | 0.03976 | 0.2483 | 0.4126 |  0.8807 | 0.968 |

  * **slope.t**:


    |   Min. |  1st Qu. | Median |  Mean | 3rd Qu. | Max. |
    |-------:|---------:|-------:|------:|--------:|-----:|
    | -2.669 | -0.04681 | 0.3002 | 2.839 |   3.601 | 13.5 |

  * **slope.p**:


    |      Min. |  1st Qu. | Median |   Mean | 3rd Qu. |   Max. |
    |----------:|---------:|-------:|-------:|--------:|-------:|
    | 5.639e-41 | 0.005771 | 0.4314 | 0.4223 |   0.751 | 0.9568 |

  * **Rsqrd**:


    |      Min. |   1st Qu. |    Median |     Mean |  3rd Qu. |    Max. |
    |----------:|----------:|----------:|---------:|---------:|--------:|
    | 1.406e-06 | 5.731e-05 | 0.0007754 | 0.005498 | 0.006446 | 0.02693 |

  * **RsqrdAdj**:


    |       Min. |    1st Qu. |    Median |     Mean |  3rd Qu. |    Max. |
    |-----------:|-----------:|----------:|---------:|---------:|--------:|
    | -0.0006424 | -0.0004608 | 0.0001599 | 0.005013 | 0.005875 | 0.02678 |

  * **slope.ConfInt_0.95**:


    |      Min. |   1st Qu. |   Median |     Mean |  3rd Qu. |     Max. |
    |----------:|----------:|---------:|---------:|---------:|---------:|
    | 1.011e-05 | 0.0002134 | 0.001334 | 0.002216 | 0.004729 | 0.005197 |


<!-- end of list -->


