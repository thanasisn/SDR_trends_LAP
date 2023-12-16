# 1 Introduction.

The shortwave downward solar irradiance (SDR) at Earth's surface plays a
significant role, on its climate. Changes of the SDR can be related to
changes on Earth's energy budget, the mechanisms of climate change, and
water and carbon cycles (Wild 2009). It can also affect solar and
agricultural production, and all living organisms. Studies of SDR
variability, have identified some distinct SDR trends on different
regions of the world on different time periods. The term 'brightening'
is generally used to describe periods of positive SDR trend, and
'dimming' for negative trend (Wild 2009). There are many cases in the
long term records of irradiance, showing a systematic change in the
magnitude of the trend, occurring roughly in the last decades of the
20th century. On multiple stations in China, a dimming period was
reported until about 2000, followed by a brightening period (Yang et al.
2021). A similar pattern was identified, with the breaking point around
1980, for stations in Central Europe (Wild et al. 2021) and Brazil
(Yamasoe et al. 2021). On global scale, an artificial Intelligence aided
spatial analysis on continental level with data from multiple stations
reach similar conclusions for these regions and for the global trend
(Yuan, Leirvik, and Wild 2021).

There is a consensus among researchers that the major factor affecting
the variability of SDR attenuation is the interactions of solar
radiation with atmospheric aerosols and clouds. Those interactions,
among other factors, have been analysed with models (Li et al. 2016;
Samset et al. 2018), showing the existence of feedback mechanisms
between the two. Similar findings have been shown in observational data
(Schwarz et al. 2020; Ohvril et al. 2009; Zerefos et al. 2009; Xia et
al. 2007 and references therein).

Due to the significant spatial and temporal variability of the trends,
and the contributing factors, there is a constant need to monitor and
investigate SDR in different sites in order to estimate the degree of
variability, and its relation to the local conditions. In this study, we
examine the trends of SDR, using ground-based measurements at
Thessaloniki, Greece, for the period 1993 to 2023, as derived from a
CM-21 pyranometer. We reevaluated and extended the dataset used by Bais
et al. (2013), we applied a different algorithm for the identification
of clear-/cloud-sky instances (Reno and Hansen 2016; Reno, Hansen, and
Stein 2012a), and we derived the SDR trends for the period of study,
under different sky conditions (all-sky, clear-sky and cloud-sky).
Finally, we investigated the dependence of the trends on solar zenith
angle and season.

# 2 Data and methodology

The SDR data were measured with a Kipp & Zonen CM-21 pyranometer
operating continuously at the Laboratory of Atmospheric Physics of the
Aristotle University of Thessaloniki ($40^{\circ}\, 38\prime\,$N,
$22^{\circ}\, 57\prime\,$E, $80\,$m a.s.l.) since 1993. Here we use data
for the period from 1993-04-13 to 2023-04-12. The monitoring site is
located near the city centre thus we expect that measurements are
affected by the urban environment. During the study period, the
pyranometer has been independently calibrated three times at the
Meteorologisches Observatorium Lindenberg, DWD, verifying that stability
of the instrument's sensitivity is better than $0.7\%$ relative to the
initial calibration by the manufacturer. Along with SDR, the direct beam
radiation (DNI) is also measured with a collocated Kipp & Zonen CHP-1
pyrheliometer since 2016-04-01. The DNI data were used as auxiliary data
to support the selection of appropriate thresholds in the clear sky
identification algorithm (CSid), which is discussed later. It is noted
that the limited dataset of DNI was not used for the identification of
clear sky cases in the entire SDR series to avoid any selection bias due
to unequal length of the two datasets. There are four distinct steps in
the creation of the dataset analyzed here: a) the acquisition of
radiation measurements from the sensors, b) the data quality check,
c) the identification of "clear sky" conditions from the radiometric
data, and d) the aggregation of data and trend analysis.

For the acquisition of radiometric data, the signal of the pyranometer
is sampled at a rate of $1\,\text{Hz}$. The mean and the standard
deviation of these samples are calculated and recorded every minute. The
measurements are corrected for the zero offset ("dark signal" in volts),
which is calculated by averaging all measurements recorded for a period
of $3\,\text{h}$, before (morning) or after (evening) the Sun reaches an
elevation angle of $- 10^{\circ}$. The signal is converted to irradiance
using a ramped value of the instrument's sensitivity between subsequent
calibrations.

A manual screening was performed, to remove inconsistent and erroneous
recordings that can occur stochastically or systematically, during the
continuous operation of the instruments. The manual screening is aided
by a radiation data quality assurance procedure, adjusted for the site,
which is based on the methods of Long and Shi (2008, 2006). Thus,
problematic recordings have been excluded from further processing.
Although it is impossible to detect all false data, the large number of
available data, and the aggregation scheme we used, ensures the quality
of the radiation measurements used in this study.

Due to the significant measurement uncertainty when the Sun is near the
horizon, we have excluded all measurements with SZA greater than
$85^{\circ}$. Moreover, due to obstructions around the site (hills and
buildings) which block the direct irradiance, we excluded data with
azimuth angle in the range $58^{\circ}$ - $120^{\circ}$ and with SZA
greater than $78^{\circ}$. To make the measurements comparable
throughout the dataset, we adjusted all one-minute data to the mean
Sun - Earth distance. Subsequently, we adjusted all measurements to the
Total Solar Irradiance (TSI) at $1\,\text{au}$, in order to compensate
for the Sun's intensity variability, using a time series of satellite
TSI observations. The TSI data we used are part of the "NOAA Climate
Data Record of Total Solar Irradiance" dataset (Coddington et al. 2005).
The initial daily values of this dataset were interpolated to match the
time step of our measurements.

In order to estimate the effect of the sky conditions on the long term
variability of SDR, we created three datasets by characterizing each
one-minute measurement with a corresponding sky-condition flag (i.e.,
all-sky, clear-sky and cloudy-sky). To identify the clear-cases we used
a method proposed by Long and Ackerman (2000) and by Reno and Hansen
(2016), which were adapted and configured for the site of Thessaloniki.
We note that all methods have some subjectivity in the definition of
clear or cloudy sky cases. As a result, the details of the definition
are site specific and they rely on a combination of thresholds and
comparisons with ideal radiation models and statistical analysis of
different signal metrics. The CSid algorithm was calibrated with the
main focus, to identify the presence of clouds. Despite the fine-tuning
of the procedure, in a few marginal cases false positive or false
negative results were identified by manual inspection. However, due to
their small number, they cannot affect the final results of the study.
For completeness, we provide below a brief overview of the clear-sky
identification algorithm (CSid), along with the site specific
thresholds.

## 2.1 The clear sky identification algorithm

To calculate the reference clear-sky $\text{SDR}_{\text{CSref}}$ we used
the $\text{SDR}_{\text{Haurwitz}}$ derived by the radiation model of
Haurwitz (1945), adjusted for our site with a factor $a$ (Eq. ),
estimated through an iterative optimization process, as described by
Long and Ackerman (2000) and Reno and Hansen (2016). The target of the
optimization was the minimization of a function $f(a)$ (Eq. ) and was
accomplished with the algorithmic function "optimise", which is an
implementation based on the work of Brent (1973), from the library
"stats" of the R programming language (R Core Team 2023).

$$f(a) = \frac{1}{n}\sum_{i = 1}^{n}\left( \text{SDR}_{\text{CSid},i} - a \times \text{SDR}_{\text{testCSref},i} \right)^{2}$$

where: $n$ is the total number of daylight data,
$\text{SDR}_{\text{CSid},i}$ are the data identified as clear sky by
CSid, $a$ is a site-specific adjustment factor, and
$\text{SDR}_{\text{testCSref},i}$ is the SDR derived by any of the
tested clear-sky radiation models.

The optimization and the selection of the clear sky reference model, was
performed on SDR observations for the period 2016 - 2021. During the
optimization, eight simple clear sky radiation models were tested
(namely, Daneshyar-Paltridge-Proctor, Kasten-Czeplak, Haurwitz,
Berger-Duffie, Adnot-Bourges-Campana-Gicquel, Robledo-Soler, Kasten and
Ineichen-Perez), with a wide range of factors. These models are
described in more details by Reno, Hansen, and Stein (2012b) and
evaluated by Reno and Hansen (2016). We found, that Haurwitz's model,
adjusted with the factor $a = 0.965$ yields one of the lowest root mean
squared errors (RMSE), while the procedure manages to characterize
successfully the majority of the data. Thus, our clear sky reference is
derived by the Eq. .

$$\text{SDR}_{\text{CSref}} = a \times \text{SDR}_{\text{Haurwitz}} = 0.965 \times 1098 \times \cos(\theta) \times \exp\left( \frac{- 0.057}{\cos(\theta)} \right)$$

where $\theta$ is the solar zenith angle (SZA).

The criteria that were used to identify whether a measurement was taken
under clear-sky conditions are presented below. A data point is flagged
as "clear-sky" if all criteria are satisfied; otherwise it is considered
as "cloud-sky". Each criterion was applied for a running window of $11$
consecutive one-minute measurements, and the characterization was
assigned to the central datum of the window. Each parameter, was
calculated both from the observations and the reference clear sky model,
for each comparison. The allowable range of variation is defined by the
model-derived value of the parameter multiplied by a factor plus an
offset. The factors and the offsets were determined empirically, by
manually inspecting each filter's performance on selected days and
adjusting them accordingly during an iterative process. The criteria are
listed below, together with the range of values within which the
respective parameter should fall in order to raise the clear-sky flag:

a)  Mean of the measured ${\overline{\text{SDR}}}_{i}$ (Eq. ).

$$0.91 \times {\overline{\text{SDR}}}_{\text{CSref},i} - 20\, Wm^{- 2} < {\overline{\text{SDR}}}_{i} < 1.095 \times {\overline{\text{SDR}}}_{\text{CSref},i} + 30\, Wm^{- 2}$$

-   

b)  Maximum measured value $M_{i}$ (Eq. ).

$$1 \times M_{\text{CSref},i} - 75\, Wm^{- 2} < M_{i} < 1 \times M_{\text{CSref},i} + 75\, Wm^{- 2}$$

-   

c)  Length $L_{i}$ of the sequential line segments, connecting the
    points of the $11$ SDR values (Eq. ).

$$L_{i} = \sum_{i = 1}^{n - 1}\sqrt{\left( \text{SDR}_{i + 1} - \text{SDR}_{i} \right)^{2} + \left( t_{i + 1} - t_{i} \right)^{2}}$$

$$1 \times L_{\text{CSref},i} - 5 < L_{i} < 1.3 \times L_{\text{CSref},i} + 13$$

-   where: $t_{i}$ is the time stamp of each SDR measurement.

d)  Standard deviation $\sigma_{i}$ of the slope ($s_{i}$) between the
    $11$ sequential points, normalized by the mean
    ${\overline{\text{SDR}}}_{i}$ (Eq. ).

$$\begin{matrix}
\sigma_{i} = \frac{\sqrt{\frac{1}{n - 1}\sum_{i = 1}^{n - 1}\left( s_{i} - \bar{s} \right)^{2}}}{{\overline{\text{SDR}}}_{i}} \\
s_{i} = \frac{\text{SDR}_{i + 1} - \text{SDR}_{i}}{t_{i + 1} - t_{i}},\mspace{6mu}\mspace{6mu}\bar{s} = \frac{1}{n - 1}\sum_{i = 1}^{n - 1}s_{i},\mspace{6mu}\mspace{6mu}\forall i \in \left\{ 1,2,\ldots,n - 1 \right\}\mspace{6mu}\mspace{6mu} \\
\end{matrix}$$

-   For this criterion, $\sigma_{i}$ should be below a certain threshold
    (Eq. ):

$$\sigma_{i} < 1.1 \times 10^{- 4}$$

-   

e)  Maximum difference $X_{i}$ between the change in measured irradiance
    and the change in clear sky irradiance over each measurement
    interval.

$$\begin{matrix}
X_{i} = \max\left\{ \left| x_{i} - x_{\text{CSref},i} \right| \right\} \\
x_{i} = \text{SDR}_{i + 1} - \text{SDR}_{i}\forall i \in \left\{ 1,2,\ldots,n - 1 \right\} \\
x_{\text{CSref},i} = \text{SDR}_{\text{CSref},i + 1} - \text{SDR}_{\text{CSref},i}\forall i \in \left\{ 1,2,\ldots,n - 1 \right\} \\
\end{matrix}$$

-   For this criterion, $X_{i}$ should be below a certain threshold
    (Eq. ):

$$X_{i} < 7.5\, Wm^{- 2}$$

In the final dataset $23.3\%$ of the 1-minute data were identified as
under clear-sky conditions and $43\%$ as under cloud-sky conditions.

## 2.2 Aggregation of data and statistical approach

In order to investigate the SDR trends which are the main focus of the
study, we implemented an appropriate aggregation scheme to the
one-minute data to derive a series in coarser time-scales. To preserve
the representativeness of the data we used the following criteria: a) we
excluded all days with less than 50% of the expected daytime
measurements, b) daily means for the clear-sky and cloudy-sky datasets
were calculated only for days with more than 60% of the expected daytime
measurements identified as clear or cloudy respectively, c) monthly
means were computed from daily means only when at least 20 days of the
month were available. To create the daily and monthly climatological
means, we averaged the data based on the day of year and calendar month,
respectively. For the seasonal means we averaged the mean daily values
in each season (Winter: December - February, Spring: March - May, etc.).
Finally, each data set was deseasonalized by subtracting the
corresponding climatological annual cycle (daily or monthly) from the
actual data. Finally, to estimate the SZA effect on the SDR trends, the
one-minute data were aggregated in $1^{\circ}$ SZA bins, separately for
the morning and afternoon hours, and then were deseasonalized as
mentioned above.

# 3 Results

## 3.1 Long-term SDR trends

We calculated the linear trends of SDR, from the departures of the mean
daily values from the daily climatology and for the three sky conditions
(Table ). In Figure  we present only the time series under all-sky
conditions; the plots for clear-sky and cloud-sky conditions, are shown
in the Appendix (Figures  and  ). We observe a positive trend for
all-sky conditions ($0.38\,\%/y$), a very close but smaller trend for
clear-skies ($0.097\,\%/y$) and a negative weaker trend for cloudy-skies
($0.41\,\%/y$). In the studied period, there is no significant break or
change in the variability of the time series. Other studies for the
European region reported a change of the SDR slope, around 1980 (Wild et
al. 2021; Yuan, Leirvik, and Wild 2021; Ohmura 2009), a few years before
the start of our records. It is interesting to note, that for the
observations period, the trend of the TSI is $- 0.00024\,\%/y$, and thus
we can not attribute any major effect on SDR trend to Solar variability.

![Figure 3.1: Anomalies (%) of the daily all-sky SDR from the
climatological mean for the period1993 - 2023. The black line is the
long term linear trend.](media/rId24.png){width="5.833333333333333in"
height="2.6942082239720033in"}

Figure 3.1: Anomalies (%) of the daily all-sky SDR from the
climatological mean for the period1993 - 2023. The black line is the
long term linear trend.

Although the year-to-year variability of the anomalies (Figure and
Figures , in Appendix), shows a rather homogeneous behaviour, plots of
the cumulative sums (CUSUM) (Regier, Briceño, and Boyer 2019) of the
anomalies can reveal different structures in the records of all three
sky conditions. In the cases of all-sky and clear-sky conditions
(Figures and ), we observe three macroscopic periods. A downward part
from the start until about 2005, a relatively steady part until about
2016 and, finally, a steep upward part until the present. For cloud-sky
(Figure ), we have a different pattern; it begins with a relatively
steady part until 1997, followed by an upward part until 2005, and a
long decline until 2020, with a small positive slope until the present.
For a uniform trend, we would expect the CUSUMs of the anomalies to have
a symmetric 'V' shape. This would indicate that the anomalies are evenly
distributed around the climatological mean, and for a positive uniform
trend, the first half to be below and the other half above the
climatological mean. In our case, there is a more complex evolution of
the anomalies. Another distinct feature of the CUSUMs, is the different
pattern of the cloudy-sky dataset which peaks around the middle of the
period. Although, there seems to exist a complementary relation to the
CUSUMs of the clear- and all-sky cases, we can not assert that clouds
are the main driver for this relation due to the great difference in the
number of observational data between the two datasets (Table ).

In order to investigate further the features of the CUSUMs, we created
another set of CUSUM plots by subtracting the corresponding long term
trend from the SDR anomaly data, prior to the CUSUM calculation
(Figure ). With this approach periods when the CUSUMs diverge from zero
can be interpreted as a systematic variation of SDR from the
climatological mean. When the CUSUM is increasing, the added values are
above the climatological values of the SDR trend and vice versa.
Overall, for all- and clear-sky conditions (Figures  and ) we observe
periods when the anomalies diverge from the climatological value, each
lasting for several years. The pattern in both datasets is very similar,
suggesting prevalence in clear skies over Thessaloniki. It is
interesting that in the period 1993 - 2016 the anomalies have a high
variability around zero, while after 2016, the range of the variability
is decreased to about one third of the prior period. For cloudy-sky
conditions (Figure ) the period 1997 - 2008 is dominated by positive
CUSUMs, suggesting a reduced effect of clouds on SDR. From 1997 to
mid-2000s CUSUMs are increasing, likely due to a continuous decrease in
the optical thickness of clouds, followed by a period of rapid increase
(within 3 years) in cloud optical thickness lasting up to 2008. The
following stable period spans for about 15 years up to 2021 when CUSUMs
start increasing again.

## 3.2 Effects of the solar zenith angle on SDR.

The solar zenith angle is a major factor affecting the SDR, since
increases in SZA leads to enhancement of the radiation path in the
atmosphere, especially in urban environments with human activities
emitting aerosols (Wang et al. 2021). In order to estimate the effect of
SZA on the SDR trends, we grouped the anomaly data in bins of
$1^{\circ}$ SZA, and calculated the overall trend for each bin before
noon and after noon (Figure ). Although there are seasonal dependencies
of the minimum SZA (see Appendix, Figure ), these dependencies would not
be further examined here. For all-sky and clear-sky conditions the
brightening effect of SDR (positive trend) is stronger for large SZAs
(Figures  and ). The trends in the morning and afternoon hours are more
or less consistent with small differences, which can be attributed to
systematic diurnal variations of aerosols, particularly during the warm
period of the year (Wang et al. 2021). For cloudy-sky conditions
(Figure ), we can not discern any significant dependence of the SDR
trend with SZA. For SZAs $16^{\circ}$ - $50^{\circ}$, the trends range
within about $\pm 0.2\,\%/y$, with a weak statistical significance.
Between $50^{\circ}$ and $75^{\circ}$ SZA the trends for the period
before noon are stronger and negative, possibly associated with stronger
attenuation by clouds under oblique incidence angles.

## 3.3 Long term trends by season

Similarly to the long term trends from daily means of SDR discussed
above, we have calculated the trend of the anomalies for the three
different sky conditions, and for each season of the year, using the
corresponding mean monthly values (Figure  and Table ). For all-sky
conditions the trend in SDR in winter is the largest ($0.69\,\%/y$),
followed by the trend in autumn ($0.43\,\%/y$, a value close to the long
term trend) both statistically significant above the $99\,\%$ confidence
level. In spring and summer, the trends are much smaller and of lesser
statistical significance. These seasonal differences indicate a possible
relation of the trends in SDR to trends of clouds during winter and
autumn. For clear-skies, the trend in winter is $0.36\,\%/y$, larger
than for all-skies ($0.69\,\%/y$), which is another indication of a
decreasing trend in cloud optical thickness. Moreover, the trends under
clear- and cloudy-sky conditions are almost complementary to each other,
particularly for winter and autumn, where the signal is stronger. During
spring and summer the statistical significance is very low and the
actual trend too small for a meaningful comparison.

# 4 Conclusions

We have demonstrated that in the period 1993 - 2023, there is a positive
trend in SDR of ($0.38\,\%/y$) (brightening) (positive trend) in
Thessaloniki, Greece, under all-sky conditions. A previous study (Bais
et al. 2013) for the period 1993 - 2011 found also a positive trend of
$0.33\,\%/y$. The increase of this trend indicates that the brightening
of SDR continues and is probably caused by continuing decreases in
aerosol optical depth and the optical thickness of clouds over the area.
Moreover, we found a similar trend under clear-sky conditions
($0.097\,\%/y$) that further supports the assumption that the
brightening is caused mainly by decreasing aerosols. Unfortunately, for
the entire period there is no available data for the aerosols, in order
to quantify their effect on SDR. However, Siomos et al. (2020) have
shown that aerosol optical depth over Thessaloniki is decreasing
constantly at least up to 2018. The attenuation of SDR by aerosols over
Europe have been proposed as major factor by Wild et al. (2021). The
dimming effect on SDR under cloudy-sky conditions ($0.41\,\%/y$),
suggests that cloud optical thickness is decreasing during this period.
Because we have no adequate data to investigate the long term changes of
cloud thickness in the region, we cannot verify if the negative SDR
trend we observe under under cloudy-skies can be attributed solely to
changes in clouds.

The observed brightening on SDR over Thessaloniki is dependent on SZA
(larger SZAs lead to stronger brightening). The trend is also dependent
on season, with winter showing the strongest statistically significant
trend of $0.69$ and $0.36\,\%/y$ for all- and clear-skies, respectively,
in contrast to spring and summer. The trends for autumn are also
significant but smaller ( $0.43$ and $0.11\,\%/y$ for all- and
clear-skies, respectively). Our findings are in agreement with other
studies for the region.

Using the CUSUMs of the monthly departures for all- and clear-skies, we
observed periods where the CUSUMs remain relatively stable, with a steep
decline before and a steep increase after. This is an indication that
the whole brightening effect does not follow a smooth development over
time.

Continued observations with a collocated pyrheliometer, which started in
2016, will allow us to further investigate the variability of solar
radiation at ground level in Thessaloniki. Also, additional data of
cloudiness, aerosols, atmospheric water vapour, etc., will allow better
attribution and quantification of the effects of these factors on SRD.

Bais, A. F., T. Drosoglou, C. Meleti, K. Tourpali, and N. Kouremeti.
2013. "Changes in Surface Shortwave Solar Irradiance from 1993 to 2011
at Thessaloniki (Greece)." *International Journal of Climatology* 33
(13): 2871--76. <https://doi.org/f5dzz5>.

Brent, Richard P. 1973. "Algorithms for Minimization Without
Derivatives." *PrenticeHall, Englewood Cliffs, NJ*.

Coddington, Odele, Judith L. Lean, Doug Lindholm, Peter Pilewskie,
Martin Snow, and NOAA CDR Program. 2005. "NOAA Climate Data Record (CDR)
of Total Solar Irradiance (TSI), NRLTSI Version 2. Daily."
<https://doi.org/10.7289/V55B00C1>.

Haurwitz, Bernhard. 1945. "Insolation in Relation to Cloudiness and
Cloud Density." *Journal of Meteorology* 2 (September): 154--66.

Li, Zhanqing, W. K.‐M. Lau, V. Ramanathan, G. Wu, Y. Ding, M. G. Manoj,
J. Liu, et al. 2016. "Aerosol and Monsoon Climate Interactions over
Asia." *Reviews of Geophysics* 54 (4): 866--929.
<https://doi.org/10.1002/2015RG000500>.

Long, Charles N., and Thomas P. Ackerman. 2000. "Identification of Clear
Skies from Broadband Pyranometer Measurements and Calculation of
Downwelling Shortwave Cloud Effects." *Journal of Geophysical Research:
Atmospheres* 105 (D12, D12): 15609--26.
<https://doi.org/10.1029/2000jd900077>.

Long, Charles N., and Y. Shi. 2006. "The QCRad Value Added Product:
Surface Radiation Measurement Quality Control Testing, Including
Climatology Configurable Limits." DOE/SC-ARM/TR-074. Office of Science,
Office of Biological; Environmental Research, U.S. Department of Energy.

---------. 2008. "An Automated Quality Assessment and Control Algorithm
for Surface Radiation Measurements." *The Open Atmospheric Science
Journal*, 23--37.

Ohmura, Atsumu. 2009. "Observed Decadal Variations in Surface Solar
Radiation and Their Causes." *Jour* 114.
<https://doi.org/10.1029/2008JD011290>.

Ohvril, Hanno, Hilda Teral, Lennart Neiman, Martin Kannel, Marika
Uustare, Mati Tee, Viivi Russak, et al. 2009. "Global Dimming and
Brightening Versus Atmospheric Column Transparency, Europe, 1906--2007."
*Journal of Geophysical Research* 114 (May).
<https://doi.org/10.1029/2008JD010644>.

R Core Team. 2023. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

Regier, Peter, Henry Briceño, and Joseph N. Boyer. 2019. "Analyzing and
Comparing Complex Environmental Time Series Using a Cumulative Sums
Approach." *MethodsX* 6: 779--87.
<https://doi.org/10.1016/j.mex.2019.03.014>.

Reno, Matthew J., and Clifford W. Hansen. 2016. "Identification of
Periods of Clear Sky Irradiance in Time Series of GHI Measurements."
*Renewable Energy* 90: 520--31. <https://doi.org/gq3sbg>.

Reno, Matthew J., Clifford W. Hansen, and Joshua S. Stein. 2012b.
"Global Horizontal Irradiance Clear Sky Models: Implementation and
Analysis." SAND2012-2389, 1039404. <https://doi.org/gq5npv>.

---------. 2012a. "Global Horizontal Irradiance Clear Sky Models:
Implementation and Analysis." *SANDIA REPORT SAND2012-2389 Unlimited
Release Printed March 2012*.

Samset, B. H., M. Sand, C. J. Smith, S. E. Bauer, P. M. Forster, J. S.
Fuglestvedt, S. Osprey, and C.‐F. Schleussner. 2018. "Climate Impacts
from a Removal of Anthropogenic Aerosol Emissions." *Geophysical
Research Letters* 45 (2): 1020--29.
<https://doi.org/10.1002/2017GL076079>.

Schwarz, M., D. Folini, S. Yang, R. P. Allan, and M. Wild. 2020.
"Changes in Atmospheric Shortwave Absorption as Important Driver of
Dimming and Brightening." *Nature Geoscience* 13 (2): 110--15.
<https://doi.org/10.1038/s41561-019-0528-y>.

Siomos, Nikolaos, Ilias Fountoulakis, Athanasios Natsis, Theano
Drosoglou, and Alkiviadis Bais. 2020. "Automated Aerosol Classification
from Spectral UV Measurements Using Machine Learning Clustering."
*Remote Sensing* 12 (6): 965. <https://doi.org/10.3390/rs12060965>.

Wang, Yawen, Jiahua Zhang, Arturo Sanchez‐Lorenzo, Katsumasa Tanaka,
Jörg Trentmann, Wenping Yuan, and Martin Wild. 2021. "Hourly Surface
Observations Suggest Stronger Solar Dimming and Brightening at Sunrise
and Sunset over China." *Geophysical Research Letters* 48 (2).
<https://doi.org/10.1029/2020GL091422>.

Wild, Martin. 2009. "Global Dimming and Brightening: A Review." *Journal
of Geophysical Research Atmospheres* 114 (12): 1--31.
<https://doi.org/bcq>.

Wild, Martin, Stephan Wacker, Su Yang, and Arturo Sanchez-Lorenzo. 2021.
"Evidence for Clear‐sky Dimming and Brightening in Central Europe."
*Geophysical Research Letters* 48 (6).
<https://doi.org/10.1029/2020GL092216>.

Xia, Xiangao, Hongbin Chen, Zhanqing Li, Pucai Wang, and Jiankai Wang.
2007. "Significant Reduction of Surface Solar Irradiance Induced by
Aerosols in a Suburban Region in Northeastern China." *Journal of
Geophysical Research Atmospheres* 112 (22): 1--9.
<https://doi.org/cdtntw>.

Yamasoe, Marcia Akemi, Nilton Manuel Évora Rosário, Samantha Novaes
Santos Martins Almeida, and Martin Wild. 2021. "Fifty-Six Years of
Surface Solar Radiation and Sunshine Duration over são Paulo, Brazil:
1961--2016." *Atmospheric Chemistry and Physics* 21 (9): 6593--603.
<https://doi.org/10.5194/acp-21-6593-2021>.

Yang, Su, Zijiang Zhou, Yu Yu, and Martin Wild. 2021. "Cloud "Shrinking"
and "Optical Thinning" in the "Dimming" Period and a Subsequent Recovery
in the "Brightening" Period over China." *Environmental Research
Letters*, January. <https://doi.org/10.1088/1748-9326/abdf89>.

Yuan, Menghan, Thomas Leirvik, and Martin Wild. 2021. "Global Trends in
Downward Surface Solar Radiation from Spatial Interpolated Ground
Observations During 1961-2019." *Journal of Climate*, September, 1--56.
<https://doi.org/10.1175/JCLI-D-21-0165.1>.

Zerefos, C. S., K. Eleftheratos, C. Meleti, S. Kazadzis, A. Romanou, C.
Ichoku, G. Tselioudis, and A. Bais. 2009. "Solar Dimming and Brightening
over Thessaloniki, Greece, and Beijing, China." *Tellus B: Chemical and
Physical Meteorology* 61 (4): 657.
<https://doi.org/10.1111/j.1600-0889.2009.00425.x>.
