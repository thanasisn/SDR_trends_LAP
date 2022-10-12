<!-- This is a markdown file. -->


 General info.
---------------

Object name:    object      
Date written:   2022-10-12 15:13:39.8 UTC  
Data file:      /home/athan/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends_season.dat      
Data file size: 1.8 KiB (1816) 
Size in memory: 4440.0 B      
Info file:      /home/athan/MANUSCRIPTS/2022_sdr_trends/figures/tbl_longterm_trends_season.inf.md      
Script name:    /home/athan/MANUSCRIPTS/2022_sdr_trends/DHI_GHI_1_longterm_trends.R      
User@Host:      athan@sagan   
Contact:        <lapauththanasis@gmail.com>      
Notes:          NA      


 Data structure.
-----------------

```
Classes ‘data.table’ and 'data.frame':	16 obs. of  9 variables:
 $ slope             : num  3.046 -2.834 0.984 -0.815 0.812 ...
 $ slope.sd          : num  2.385 1.594 0.89 1.87 0.116 ...
 $ slope.p           : num  2.02e-01 7.59e-02 2.69e-01 6.63e-01 3.71e-12 ...
 $ slope.ConfInt_0.95: num  4.685 3.131 1.749 3.673 0.228 ...
 $ slope.ConfInt_0.99: num  6.17 4.12 2.3 4.83 0.3 ...
 $ DATA              : chr  "ALL" "ALL" "ALL" "ALL" ...
 $ Season            : chr  "Winter" "Spring" "Summer" "Automn" ...
 $ var               : chr  "DIR_att" "DIR_att" "DIR_att" "DIR_att" ...
 $ N                 : int  510 524 521 535 2395 2537 2529 2546 267 312 ...
 - attr(*, ".internal.selfref")=<externalptr> 
```


 Data quality.
---------------

| &nbsp; | slope | slope.sd | slope.p | slope.ConfInt_0.95 | slope.ConfInt_0.99 | DATA | Season | var |  N |
|:------:|------:|---------:|--------:|-------------------:|-------------------:|-----:|-------:|----:|---:|
| Values |    16 |       16 |      16 |                 16 |                 16 |    0 |      0 |   0 | 16 |
|  INFs  |     0 |        0 |       0 |                  0 |                  0 |    0 |      0 |   0 |  0 |
|  NAs   |     0 |        0 |       0 |                  0 |                  0 |    0 |      0 |   0 |  0 |


 Data Summary.
---------------

|            slope |        slope.sd |           slope.p | slope.ConfInt_0.95 | slope.ConfInt_0.99 |
|-----------------:|----------------:|------------------:|-------------------:|-------------------:|
| Min.   :-2.83442 | Min.   :0.03913 | Min.   :0.0000000 |    Min.   :0.07672 |     Min.   :0.1009 |
| 1st Qu.: 0.09779 | 1st Qu.:0.06564 | 1st Qu.:0.0000001 |    1st Qu.:0.12876 |     1st Qu.:0.1693 |
| Median : 0.33080 | Median :0.20816 | Median :0.0006131 |    Median :0.40883 |     Median :0.5379 |
| Mean   : 0.31831 | Mean   :0.54892 | Mean   :0.2036427 |    Mean   :1.07852 |     Mean   :1.4194 |
| 3rd Qu.: 0.69658 | 3rd Qu.:0.53293 | 3rd Qu.:0.3308537 |    3rd Qu.:1.04776 |     3rd Qu.:1.3794 |
| Max.   : 3.04553 | Max.   :2.38489 | Max.   :0.8405666 |    Max.   :4.68545 |     Max.   :6.1662 |

 

|             DATA |           Season |              var |              N |
|-----------------:|-----------------:|-----------------:|---------------:|
|        Length:16 |        Length:16 |        Length:16 | Min.   : 267.0 |
| Class :character | Class :character | Class :character | 1st Qu.: 495.8 |
| Mode  :character | Mode  :character | Mode  :character | Median : 848.5 |
|               NA |               NA |               NA | Mean   :1253.1 |
|               NA |               NA |               NA | 3rd Qu.:2287.8 |
|               NA |               NA |               NA | Max.   :2546.0 |



  * **slope**:


    |   Min. | 1st Qu. | Median |   Mean | 3rd Qu. |  Max. |
    |-------:|--------:|-------:|-------:|--------:|------:|
    | -2.834 | 0.09779 | 0.3308 | 0.3183 |  0.6966 | 3.046 |

  * **slope.sd**:


    |    Min. | 1st Qu. | Median |   Mean | 3rd Qu. |  Max. |
    |--------:|--------:|-------:|-------:|--------:|------:|
    | 0.03913 | 0.06564 | 0.2082 | 0.5489 |  0.5329 | 2.385 |

  * **slope.p**:


    |      Min. |   1st Qu. |    Median |   Mean | 3rd Qu. |   Max. |
    |----------:|----------:|----------:|-------:|--------:|-------:|
    | 9.045e-27 | 5.486e-08 | 0.0006131 | 0.2036 |  0.3309 | 0.8406 |

  * **slope.ConfInt_0.95**:


    |    Min. | 1st Qu. | Median |  Mean | 3rd Qu. |  Max. |
    |--------:|--------:|-------:|------:|--------:|------:|
    | 0.07672 |  0.1288 | 0.4088 | 1.079 |   1.048 | 4.685 |

  * **slope.ConfInt_0.99**:


    |   Min. | 1st Qu. | Median |  Mean | 3rd Qu. |  Max. |
    |-------:|--------:|-------:|------:|--------:|------:|
    | 0.1009 |  0.1693 | 0.5379 | 1.419 |   1.379 | 6.166 |

  * **DATA**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |     16 | character | character |

  * **Season**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |     16 | character | character |

  * **var**:


    | Length |     Class |      Mode |
    |-------:|----------:|----------:|
    |     16 | character | character |

  * **N**:


    | Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. |
    |-----:|--------:|-------:|-----:|--------:|-----:|
    |  267 |   495.8 |  848.5 | 1253 |    2288 | 2546 |


<!-- end of list -->


