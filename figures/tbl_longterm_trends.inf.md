<!-- This is a markdown file. -->


 General info.
---------------

Object name:    object      
Date written:   2022-10-10 15:12:49.8 UTC  
Data file:      /home/athan/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.dat      
Data file size: 995.0 B (995) 
Size in memory: 3384 bytes      
Info file:      /home/athan/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends.inf.md      
Script name:    /home/athan/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_1_longterm_trends.R      
User@Host:      athan@tyler   
Contact:        <lapauththanasis@gmail.com>      
Notes:          NA      


 Data structure.
-----------------

```
Classes ‘data.table’ and 'data.frame':	8 obs. of  8 variables:
 $ var     : chr  "DIR_att" "HOR_att" "GLB_att" "DIR_transp" ...
 $ data    : chr  "ALL" "ALL" "ALL" "ALL" ...
 $ slope   : num  3.57e-07 6.61e-07 3.32e-06 2.75e-06 1.92e-06 ...
 $ slope.sd: num  6.60e-06 6.61e-06 3.26e-07 7.26e-06 1.36e-06 ...
 $ slope.t : num  0.0542 0.1 10.1629 0.3783 1.4138 ...
 $ slope.p : num  9.57e-01 9.20e-01 3.80e-24 7.05e-01 1.58e-01 ...
 $ Rsqrd   : num  1.41e-06 4.79e-06 1.02e-02 6.85e-05 1.46e-03 ...
 $ RsqrdAdj: num  -0.000478 -0.000474 0.010119 -0.00041 0.00073 ...
 - attr(*, ".internal.selfref")=<externalptr> 
```


 Data quality.
---------------

| &nbsp; | var | data | slope | slope.sd | slope.t | slope.p | Rsqrd | RsqrdAdj |
|:------:|----:|-----:|------:|---------:|--------:|--------:|------:|---------:|
| Values |   0 |    0 |     8 |        8 |       8 |       8 |     8 |        8 |
|  INFs  |   0 |    0 |     0 |        0 |       0 |       0 |     0 |        0 |
|  NAs   |   0 |    0 |     0 |        0 |       0 |       0 |     0 |        0 |


 Data Summary.
---------------

|              var |             data |              slope |          slope.sd |          slope.t |
|-----------------:|-----------------:|-------------------:|------------------:|-----------------:|
|         Length:8 |         Length:8 | Min.   :-7.317e-07 | Min.   :1.410e-08 | Min.   :-2.66912 |
| Class :character | Class :character | 1st Qu.: 2.587e-07 | 1st Qu.:2.981e-07 | 1st Qu.:-0.03593 |
| Mode  :character | Mode  :character | Median : 1.292e-06 | Median :1.875e-06 | Median : 0.23916 |
|               NA |               NA | Mean   : 1.389e-06 | Mean   :3.096e-06 | Mean   : 2.82894 |
|               NA |               NA | 3rd Qu.: 2.778e-06 | 3rd Qu.:6.602e-06 | 3rd Qu.: 3.60108 |
|               NA |               NA | Max.   : 3.317e-06 | Max.   :7.256e-06 | Max.   :13.49769 |

 

|          slope.p |             Rsqrd |           RsqrdAdj |
|-----------------:|------------------:|-------------------:|
| Min.   :0.000000 | Min.   :1.406e-06 | Min.   :-0.0006634 |
| 1st Qu.:0.005771 | 1st Qu.:5.260e-05 | 1st Qu.:-0.0004750 |
| Median :0.431438 | Median :7.649e-04 | Median : 0.0001599 |
| Mean   :0.438397 | Mean   :5.493e-03 | Mean   : 0.0050083 |
| 3rd Qu.:0.799679 | 3rd Qu.:6.446e-03 | 3rd Qu.: 0.0058747 |
| Max.   :0.956802 | Max.   :2.693e-02 | Max.   : 0.0267824 |



  * **var**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |      8 | character | character |

  * **data**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |      8 | character | character |

  * **slope**:


    |       Min. |   1st Qu. |    Median |      Mean |   3rd Qu. |      Max. |
    |-----------:|----------:|----------:|----------:|----------:|----------:|
    | -7.317e-07 | 2.587e-07 | 1.292e-06 | 1.389e-06 | 2.778e-06 | 3.317e-06 |

  * **slope.sd**:


    |     Min. |   1st Qu. |    Median |      Mean |   3rd Qu. |      Max. |
    |---------:|----------:|----------:|----------:|----------:|----------:|
    | 1.41e-08 | 2.981e-07 | 1.875e-06 | 3.096e-06 | 6.602e-06 | 7.256e-06 |

  * **slope.t**:


    |   Min. |  1st Qu. | Median |  Mean | 3rd Qu. | Max. |
    |-------:|---------:|-------:|------:|--------:|-----:|
    | -2.669 | -0.03593 | 0.2392 | 2.829 |   3.601 | 13.5 |

  * **slope.p**:


    |      Min. |  1st Qu. | Median |   Mean | 3rd Qu. |   Max. |
    |----------:|---------:|-------:|-------:|--------:|-------:|
    | 5.639e-41 | 0.005771 | 0.4314 | 0.4384 |  0.7997 | 0.9568 |

  * **Rsqrd**:


    |      Min. |  1st Qu. |    Median |     Mean |  3rd Qu. |    Max. |
    |----------:|---------:|----------:|---------:|---------:|--------:|
    | 1.406e-06 | 5.26e-05 | 0.0007649 | 0.005493 | 0.006446 | 0.02693 |

  * **RsqrdAdj**:


    |       Min. |   1st Qu. |    Median |     Mean |  3rd Qu. |    Max. |
    |-----------:|----------:|----------:|---------:|---------:|--------:|
    | -0.0006634 | -0.000475 | 0.0001599 | 0.005008 | 0.005875 | 0.02678 |


<!-- end of list -->


