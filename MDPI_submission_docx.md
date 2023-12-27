# 1 Introduction

The shortwave downward solar irradiance (SDR) at Earth's surface plays a
significant role on its climate. Changes in the SDR can be related to
changes in Earth's energy budget, the mechanisms of climate change, and
water and carbon cycles (Wild 2009). It can also affect solar and
agricultural production and all living organisms. Studies of SDR
variability have identified some distinct SDR trends in different
regions of the world in different time periods. The term 'brightening'
is generally used to describe periods of positive SDR trend, and
'dimming' for periods of negative trend (Wild 2009). There are many
cases in the long-term records of irradiance showing a systematic change
in the magnitude of the trend, occurring roughly in the last decades of
the 20th century. At multiple stations in China, a dimming period was
reported until about 2000, followed by a brightening period (Yang et al.
2021). A similar pattern was identified, with a breaking point around
1980, for stations in Central Europe (Wild et al. 2021) and Brazil
(Yamasoe et al. 2021). On global scale, an artificial intelligence aided
spatial analysis on the continental level with data from multiple
stations reached similar conclusions for these regions and for the
global trend (Yuan, Leirvik, and Wild 2021).

There is a consensus among researchers that the major factor affecting
the variability of SDR attenuation is the interactions of solar
radiation with atmospheric aerosols and clouds. Those interactions,
among other factors, have been analyzed with models (Li et al. 2016;
Samset et al. 2018), showing the existence of feedback mechanisms
between the two. Similar findings have been shown from the analysis of
observations at other locations (Schwarz et al. 2020; Ohvril et al.
2009; Zerefos et al. 2009; Xia et al. 2007) \[and references therein\].
In the Mediterranean region, aerosols have been recognized as an
important factor affecting the penetration of solar radiation at the
surface (Fountoulakis et al. 2016; Siomos et al. 2018; Gkikas et al.
2013; Lozano et al. 2021). These studies investigated the long-term
trend in aerosol optical depth, which has been found to decrease in the
last three decades, the transport and composition of aerosols, and their
radiative effects.

Due to the significant spatial and temporal variability of the trends
and the contributing factors, there is a constant need to monitor and
investigate SDR at different sites in order to estimate the degree of
variability, and its relation to the local conditions. In this study, we
examine the trends of SDR, using ground-based measurements at
Thessaloniki, Greece, for the period from 1993 to 2023. We re-evaluated
and extended the dataset used by Bais et al. (2013), we applied a
different algorithm for the identification of clear-/cloud-sky instances
(Reno and Hansen 2016; Reno, Hansen, and Stein 2012), and we derived the
SDR trends for the period of study, under different sky conditions
(all-sky, clear-sky, and cloudy-sky). Finally, we investigated the
dependence of the trends on solar zenith angle and season.

# 2 Data and Methodology

The SDR data were measured with a Kipp & Zonen CM-21 pyranometer
operating continuously at the Laboratory of Atmospheric Physics of the
Aristotle University of Thessaloniki ($40^{\circ}\, 38\prime\,$N,
$22^{\circ}\, 57\prime\,$E, $80\,$m a.s.l.). Here, we used data for the
period from 13 April 1993 to 13 April 2023. The monitoring site was
located near the city center, thus we expect that measurements were
affected by the urban environment, mainly by aerosols. During the study
period, the pyranometer was independently calibrated three times at the
Meteorologisches Observatorium Lindenberg, DWD, verifying that the
stability of the instrument's sensitivity was better than $0.7\%$
relative to the initial calibration by the manufacturer. Along with SDR,
the direct beam radiation (DNI) was also measured with a collocated Kipp
& Zonen CHP-1 pyrheliometer since 1 April 2016. The DNI data were used
as auxiliary data to support the selection of appropriate thresholds in
the clear-sky identification algorithm (CSid), which is discussed in
Section . It is noted that the limited dataset of DNI was not used for
the identification of clear-sky cases in the entire SDR series to avoid
any selection bias due to the unequal length of the two datasets. There
are four distinct steps in the creation of the dataset analyzed here:
(a) the acquisition of radiation measurements from the sensors, (b) the
data quality check, (c) the identification of "clear sky" conditions
from the SDR data, and (d) the aggregation of data and trend analysis.

For the acquisition of radiometric data, the signal of the pyranometer
was sampled at a rate of $1\,\text{Hz}$. The mean and the standard
deviation of these samples were calculated and recorded every minute.
The measurements were corrected for the zero offset ("dark signal" in
volts), which was calculated by averaging all measurements recorded for
a period of $3\,\text{h}$, before (morning) or after (evening) the Sun
reaches an elevation angle of $- 10^{\circ}$. The signal was converted
to irradiance using a ramped value of the instrument's sensitivity
between subsequent calibrations.

A manual screening was performed, to remove inconsistent and erroneous
recordings that can occur stochastically or systematically, during the
continuous operation of the instruments. The manual screening was aided
by a radiation data quality assurance procedure, adjusted for the site,
which was based on the methods of Long and Shi (Long and Shi 2006,
2008). Thus, problematic recordings have been excluded from further
processing. Although it is impossible to detect all false data, the
large number of available data, and the aggregation scheme we used,
ensures the quality of the radiation measurements used in this study.

Due to the significant measurement uncertainty when the Sun is near the
horizon, we have excluded all measurements with solar zenith angle (SZA)
greater than $85^{\circ}$. Moreover, due to obstructions around the site
(hills and buildings) that block the direct irradiance, we excluded data
with azimuth angle in the range $58^{\circ}$--$120^{\circ}$ and with SZA
greater than $78^{\circ}$. To make the measurements comparable
throughout the dataset, we adjusted all one-minute data to the mean
Sun--Earth distance. Subsequently, we adjusted all measurements to the
Total Solar Irradiance (TSI) at $1\,\text{au}$, in order to compensate
for the Sun's intensity variability, using a time series of satellite
TSI observations. The TSI data we used are part of the 'NOAA Climate
Data Record of Total Solar Irradiance' dataset (Coddington et al. 2005).
The initial daily values of this dataset were interpolated to match the
time step of our measurements.

In order to estimate the effect of the sky conditions on the long-term
variability of SDR, we created three datasets by characterizing each
one-minute measurement with a corresponding sky-condition flag (i.e.,
all-sky, clear-sky, and cloudy-sky). To identify the clear cases, we
used the method proposed by Reno and Hansen (2016), which requires the
definition of some site specific parameters. These parameters were
determined by an iterative process, as the original authors proposed,
and are discussed in the next section.

We note that all methods have some subjectivity in the definition of
clear or cloudy sky cases. As a result, the details of the definition
are site specific, and they rely on a combination of thresholds and
comparisons with ideal radiation models and statistical analysis of
different signal metrics. The CSid algorithm was calibrated with the
main focus to identify the presence of clouds. Despite the fine-tuning
of the procedure, in a few marginal cases, false positive or false
negative results were identified by manual inspection. However, due to
their small number, they did not affect the final results of the study.
For completeness, we provide below a brief overview of the CSid
algorithm, along with the site-specific thresholds.

## 2.1 The Clear Sky Identification Algorithm

To calculate the reference clear-sky $\text{SDR}_{\text{CSref}}$, we
used the $\text{SDR}_{\text{Haurwitz}}$ derived by the radiation model
of Haurwitz (1945) (Equation ()), adjusted for our site:

$$\text{SDR}_{\text{Haurwitz}} = 1098 \times \cos(\theta) \times \exp\left( \frac{- 0.059}{\cos(\theta)} \right)$$

where $\theta$ is the SZA.

The adjustment was made with a factor $a$ (Equation ()), which was
estimated through an iterative optimization process, as described by
Long and Ackerman (2000) and Reno and Hansen (2016). The target of the
optimization was the minimization of a function $f(a)$ (Equation ()) and
was accomplished with the algorithmic function 'optimise', which is an
implementation based on the work of Brent (1973), from the library
'stats' of the R programming language (R Core Team 2023).

$$f(a) = \frac{1}{n}\sum_{i = 1}^{n}\left( \text{SDR}_{\text{CSid},i} - a \times \text{SDR}_{\text{testCSref},i} \right)^{2}$$

where $n$ is the total number of daylight data,
$\text{SDR}_{\text{CSid},i}$ are the data identified as clear-sky by
CSid, $a$ is a site-specific adjustment factor, and
$\text{SDR}_{\text{testCSref},i}$ is the SDR derived by any of the
tested clear-sky radiation models.

The optimization and the selection of the clear-sky reference model was
performed on SDR observations for the period 2016--2021. During the
optimization, eight simple clear-sky radiation models were tested
(namely, Daneshyar--Paltridge--Proctor, Kasten--Czeplak, Haurwitz,
Berger--Duffie, Adnot--Bourges--Campana--Gicquel, Robledo--Soler,
Kasten, and Ineichen--Perez), with a wide range of factors. These models
are described in more detail by Reno, Hansen, and Stein (2012) and are
evaluated by Reno and Hansen (2016). We found that Haurwitz's model,
adjusted with the factor $a = 0.965$, yields one of the lowest root mean
squared errors (RMSE), and the procedure manages to successfully
characterize the majority of the data. Thus, our clear sky reference is
derived by Equation ():

$$\text{SDR}_{\text{CSref}} = a \times \text{SDR}_{\text{Haurwitz}} = 0.965 \times 1098 \times \cos(\theta) \times \exp\left( \frac{- 0.057}{\cos(\theta)} \right)$$

The criteria that were used to identify whether a measurement was taken
under clear-sky conditions are presented below. A data point is flagged
as "clear-sky" if all criteria are satisfied; otherwise it is considered
as "cloud-sky". Each criterion was applied for a running window of $11$
consecutive one-minute measurements, and the characterization was
assigned to the central datum of the window. Each parameter was
calculated from the observations in comparison to the reference clear
sky model. The allowable range of variation is defined by the
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
    points of the $11$ SDR values (Equation ).

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

In the final dataset $26\,\%$ of the days were identified as under
clear-sky conditions and $48\,\%$ as under cloud-sky conditions. The
remaining $26\,\%$ of the data correspond to mixed cases and were not
analyzed as a separate group.

## 2.2 Aggregation of data and statistical approach

In order to investigate the SDR trends which are the main focus of the
study, we implemented an aggregation scheme to the one-minute data to
derive series in coarser time-scales. To preserve the representativeness
of the data we used the following criteria: a) we excluded all days with
less than 50% of the expected daytime measurements, b) daily means for
the clear-sky and cloudy-sky datasets were calculated only for days with
more than 60% of the expected daytime measurements identified as clear
or cloudy respectively, c) monthly means were computed from daily means.
For the all-skies dataset monthly means were computed only when at least
20 days were available. Seasonal means were derived by averaging the
monthly mean values in each season (winter: December - February, spring:
March - May, etc.). The daily and monthly climatological means were
derived by averaging the data for each day of year and calendar month,
respectively. The daily and monthly datasets were deseasonalized by
subtracting the corresponding climatological annual cycle (daily or
monthly) from the actual data. Finally, to estimate the SZA effect on
the SDR trends, the one-minute data were aggregated in $1^{\circ}$ SZA
bins, separately for the morning and afternoon hours.

The linear trends were calculated using a first order autoregressive
model with lag of 1 day using the "maximum likelihood" fitting method
(Gardner, Harvey, and Phillips 1980; Jones 1980), by implementing the
function "arima" from the library "stats" of the R programming language
(R Core Team 2023). The trends were reported together with the $2\sigma$
errors.

# 3 Results

## 3.1 Long-term SDR trends

We calculated the linear trends of SDR, from the departures of the mean
daily values from the daily climatology and for the three sky
conditions. These are presented in Table  which contains also the
$2\sigma$ standard error, the Pearson's correlation coefficient R and
the trend in absolute units. In Figure  we present only the time series
under all-sky conditions; the plots for clear-sky and cloud-sky
conditions, are shown in the Appendix (Figures  and  ). In the studied
period, there is no significant break or change in the variability
pattern of the time series. The linear trends in all three datasets are
positive and around $0.4\,\%/y$ for all-sky and cloudy-sky conditions,
while for clear-skies the trend is much smaller (\~$0.1\,\%/y$). The
linear trends were calculated taking into account the autocorrelation of
the time series and all three are statistically significant at least at
the $95\,\%$ confidence level, as they are larger than the corresponding
$2\sigma$ errors, despite the small values of R, which is due to the
large variability of the daily values. The clear-sky trend is very small
suggesting a small effect from aerosols and water vapor which are the
dominant factors of the SDR variability (Fountoulakis et al. 2016;
Siomos et al. 2018; Yu et al. 2022). In contrast, the large positive
trend of SDR under cloudy skies can be attributed to reduction in cloud
cover and/or cloud optical depth. Lack of continuous observations of
cloud optical thickness that could support these findings does not allow
drawing firm conclusions. However, there are indications that the total
cloud-cover as inferred from the ERA5 analysis for the grid point of
Thessaloniki is decreasing over the period of study. From the difference
between all-sky and clear-sky SDR trends, expressed in $W/m^{2}/y$ using
the long-term mean of the respective datasets, the radiative effect of
clouds is estimated to $0.96\, W/m^{2}/y$. This estimate is similar to
the cloud radiative forcing of $1.22\, W/m^{2}/y$ reported for Granada,
Spain (Lozano, Alados, and Foyo-Moreno 2023).

The all-sky trend is similar to the one reported in Bais et al. (2013)
from a ten-year shorter dataset suggesting that the tendency of SDR in
Thessaloniki is systematic. Other studies for the European region
reported a change in the SDR trend around 1980 from negative to positive
with comparable magnitude (Wild et al. 2021; Yuan, Leirvik, and Wild
2021; Ohmura 2009), well before the start of our records. However, the
trends reported here for the three datasets are in accordance with the
widely accepted solar radiation brightening over Europe. For the period
of our observations the trend in the TSI is negligible
($- 0.00022\,\%/y$), and thus we cannot attribute any significant effect
on the SDR trend to solar variability.

![Figure 3.1: Anomalies (%) of the daily all-sky SDR from the
climatological mean for the period 1993 -- 2023. The black line is the
long term linear trend.](media/rId24.png){width="5.833333333333333in"
height="2.6942082239720033in"}

Figure 3.1: Anomalies (%) of the daily all-sky SDR from the
climatological mean for the period 1993 -- 2023. The black line is the
long term linear trend.

Although the year-to-year variability of the anomalies (Figure and
Figures , in Appendix), shows a rather homogeneous behavior, plots of
the cumulative sums (CUSUM) (Regier, Briceño, and Boyer 2019) of the
anomalies can reveal different structures in the records of all three
sky conditions. For time series with a uniform trend, we would expect
the CUSUMs of the anomalies to have a symmetric 'V' shape centered
around the middle of the series. This would indicate that the anomalies
are evenly distributed around the climatological mean, and for a
positive uniform trend, the first half is below and the second half
above the climatological mean. In our case, there is a more complex
evolution of the anomalies. For all-skies (Figure ), we observe three
rather distinct periods: a) a downward part between the start of the
datasets and about 2000, denoting that all anomalies are negative thus
below the climatology; b) a relatively steady part lasting for almost 20
years suggesting little variability in SDR anomalies; and c) a steep
upward part to present indicating anomalies above the climatology. The
CUSUMs for cloudy-skies (Figure ), show a similar behavior with some
short-term differences that do not change the overall pattern. For clear
skies (Figure ), a monotonic downward tendency is evident until 2004,
suggesting that the anomalies are all negative. After 2004 the anomalies
turn to positive at a fast rate for about five years and at a slower
rate thereafter.

In order to unveil further the features of the variability of the three
datasets, Figure  presents another set of CUSUM plots using anomalies
after the long-term linear trend is removed. With this approach, periods
when the CUSUMs diverge from zero can be interpreted as a systematic
variation of SDR from the climatological mean. When the CUSUM is
increasing, the anomalies values are above the climatology and vice
versa. Overall, for all- and cloudy-sky conditions (Figures  and ) we
observe periods with anomalies diverging from the climatological values,
each lasting for several years. These fluctuations are probably within
the natural variability and no distinct changes are identified. The
pattern in both datasets is similar, suggesting prevalence in cloudy
skies over Thessaloniki. For clear skies (Figure ) the distinct change
in 2004 is now clearer. The most likely reason for this change is the
monotonic reduction of aerosols in Thessaloniki. At that year there a
change in the rate of decrease in aerosol optical depth as illustrated
in Figure 7 of Siomos et al. (2020). This abrupt change in CUSUMs lasts
until about 2010 when the anomalies become again variable.

## 3.2 Effects of the solar zenith angle on SDR

The solar zenith angle is a major factor affecting the SDR, since
increases in SZA leads to enhancement of the radiation path in the
atmosphere, especially in urban environments with human activities
emitting aerosols (Wang et al. 2021). In order to estimate the effect of
SZA on the SDR trends, we grouped the data in bins of $1^{\circ}$ SZA,
and calculated the overall trend for each bin, separately for the daily
periods before noon and after noon (Figure ). Although there are
seasonal dependencies of the minimum SZA (see Appendix, Figure ), these
dependencies are not discussed further.

For all-sky conditions the brightening effect of SDR (positive trend)
increases with SZAs (Figures ) ranging from about $0.1\,\%/y$ to about
$0.7\,\%/y$ for the statistically significant trends. The trends in the
morning and afternoon hours are more or less consistent with small
differences at small SZAs which can be attributed to effects on clear
sky SDR from systematic diurnal patterns of aerosols during the warm
period of the year, consistently with the results reported for China by
Wang et al. (2021). Note that SZAs less than $25^{\circ}$ can only occur
during the warm period of the year around noon when clear skies are more
frequent. The increasing trend with SZA is likely caused by the
increased attenuation of SDR with SZA. The effect is larger when aerosol
and/or cloud layers are optically thicker, therefore, decreases in
aerosol and clouds through the study period will result in larger
positive trends of SDR at larger SZAs.

Under clear skies (Figures ), the trends are smaller and less variable,
ranging between $0.1$ and $0.15\,\%/y$ up to $77^{\circ}$ SZA. At higher
SZAs and in the afternoon hours there a sharp increase in the trend up
to $0.3\,\%/y$, which may have been caused by the long path length of
radiation through the atmosphere as discussed above for the all-sky
conditions. The small differences in the trend between morning and
afternoon between $35^{\circ}$ and $60^{\circ}$ SZA is likely a result
of less attenuation of SDR in the morning hours due to lesser amounts of
aerosols and shallower boundary layer.

For cloudy-sky conditions (Figure ), we cannot discern any significant
dependence of the SDR trend with SZA as the variability of irradiance is
dominated by the cloud effects leading to insignificant trends.
Statistically significant trends appear only in the afternoon and for
SZAs larger than $60^{\circ}$. The sharp increase of the trend at SZAs
larger than $\sim 75^{\circ}$, observed also for clear skies, is
probably associated with stronger attenuation by clouds under oblique
incidence angles, which result also in smaller variability.

## 3.3 Long term SDR trends by season

Similarly to the long term trends from daily means of SDR discussed
above, we have calculated the trend for the three sky conditions and for
each season of the year, using the corresponding mean monthly anomalies
(Figure  and Table ). Table  contains also the $2\sigma$ standard error,
the Pearson's correlation coefficient R and the corresponding p-value.
The winter linear trends exhibit generally the largest R values ranging
between $0.54$ and $0.60\,\%/y$. For all-sky conditions the trend in SDR
in winter is the largest ($0.7\,\%/y$), followed by the trend in autumn
($0.42\,\%/y$, a value close to the long term trend) both statistically
significant at the $95\,\%$ confidence level. In spring and summer, the
trends are much smaller and of lesser statistical significance. These
seasonal differences indicate a possible relation of the trends in SDR
to trends in clouds during winter and autumn. For clear-skies, the trend
in winter is $0.4\,\%/y$ and is associated with the decreasing trend in
aerosol optical depth (Siomos et al. 2020). Moreover, it is almost half
of that for all-skies, which is another indication of a decreasing trend
in cloud optical thickness. In other seasons the clear-sky trend is very
small (below $0.1\,\%/y$). Finally, for cloudy-skies the winter trend is
the largest ($0.76\,\%/y$) and greater than for all-skies, followed by a
much smaller trend in autumn ($0.19\,\%/y$).

The trends under clear- and cloudy-sky conditions are of the same
direction, and it would be expected that their sum is similar to the
all-sky trend. This does not happen, especially for winter, likely due
to the way the monthly means for clear and cloudy skies were calculated.
Daily means were calculated only when at least $60\,\%$ of the clear- or
cloudy-sky data were available (see sect. ).

# 4 Conclusions

We have analyzed a 30-year dataset of SRD measurements in Thessaloniki
Greece (1993 -- 2023) aiming to identify the long term variability of
solar irradiance under different sky conditions. Under all-sky
conditions there is a positive trend in SDR of $0.38\,\%/y$
(brightening). A previous study (Bais et al. 2013) for the period 1993
-- 2011 reported also a positive trend of $0.33\,\%/y$. The slight
increase of this trend indicates that the brightening of SDR continues
and is likely caused by continuing decreases in aerosol optical depth
and the optical thickness of clouds over the area. A smaller trend has
been found under clear-sky conditions ($0.097\,\%/y$) which supports the
notion that part of the brightening is caused by decreasing aerosols.
Siomos et al. (2020) have shown that aerosol optical depth over
Thessaloniki is decreasing constantly at least up to 2018. The
attenuation of SDR by aerosols over Europe has been proposed as major
factor by Wild et al. (2021). Unfortunately, for this study aerosol data
for the entire period were not available in order to quantify their
effect on SDR. The brightening effect on SDR under cloudy-sky conditions
($0.41\,\%/y$), suggests that cloud optical thickness is also decreasing
during this period. As long term data of cloud optical thickness are
also not available for the region, we cannot draw quantitative
conclusions.

The observed brightening on SDR over Thessaloniki is dependent on SZA
(larger SZAs lead to stronger brightening). The trend is also dependent
on season, with winter showing the strongest statistically significant
trend of $0.7$ and $0.76\,\%/y$ for all- and cloudy-skies, respectively,
in contrast to spring and summer. The trends for autumn are also
significant but smaller ($0.42$ and $0.19\,\%/y$ for all- and
cloudy-skies, respectively). The trend for clear skies is largest in
winter ($0.4\,\%/y$) and negligible in spring, summer and autumn.

Using the CUSUMs of the monthly departures for all- and cloudy-skies, we
observed a 20-year period starting around 2000 where the CUSUMs remain
relatively stable, with a steep decline before and a steep increase
after. The rather smooth course of the CUSUMs suggests that no important
change in the SDR pattern has occurred in the entire record.

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

Fountoulakis, Ilias, Alberto Redondas, Alkiviadis F. Bais, Juan José
Rodriguez-Franco, Konstantinos Fragkos, and Alexander Cede. 2016. "Dead
Time Effect on the Brewer Measurements: Correction and Estimated
Uncertainties." *Atmospheric Measurement Techniques* 9 (4): 1799--1816.
<https://doi.org/gcc32t>.

Gardner, G., A. C. Harvey, and G. D. A. Phillips. 1980. "Algorithm AS
154: An Algorithm for Exact Maximum Likelihood Estimation of
Autoregressive-Moving Average Models by Means of Kalman Filtering."
*Applied Statistics* 29 (3): 311. <https://doi.org/10.2307/2346910>.

Gkikas, A., N. Hatzianastassiou, N. Mihalopoulos, V. Katsoulis, S.
Kazadzis, J. Pey, X. Querol, and O. Torres. 2013. "The Regime of Intense
Desert Dust Episodes in the Mediterranean Based on Contemporary
Satellite Observations and Ground Measurements." *Atmospheric Chemistry
and Physics* 13 (23): 12135--54.
<https://doi.org/10.5194/acp-13-12135-2013>.

Haurwitz, Bernhard. 1945. "Insolation in Relation to Cloudiness and
Cloud Density." *Journal of Meteorology* 2 (September): 154--66.

Jones, Richard H. 1980. "Maximum Likelihood Fitting of ARMA Models to
Time Series with Missing Observations." *Technometrics* 22 (3): 389--95.
<https://doi.org/10.1080/00401706.1980.10486171>.

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

Lozano, Ismael L., Inmaculada Alados, and Inmaculada Foyo-Moreno. 2023.
"Analysis of the Solar Radiation/Atmosphere Interaction at a
Mediterranean Site: The Role of Clouds." *Atmospheric Research* 296
(December): 107072. <https://doi.org/10.1016/j.atmosres.2023.107072>.

Lozano, Ismael L., Guadalupe Sánchez-Hernández, Juan Luis
Guerrero-Rascado, Inmaculada Alados, and Inmaculada Foyo-Moreno. 2021.
"Aerosol Radiative Effects in Photosynthetically Active Radiation and
Total Irradiance at a Mediterranean Site from an 11-Year Database."
*Atmospheric Research* 255 (June): 105538.
<https://doi.org/10.1016/j.atmosres.2021.105538>.

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

Reno, Matthew J., Clifford W. Hansen, and Joshua S. Stein. 2012. "Global
Horizontal Irradiance Clear Sky Models: Implementation and Analysis."
SAND2012-2389, 1039404. Albuquerque, New Mexico: Sandia National
Laboratories. <https://doi.org/10.2172/1039404>.

Samset, B. H., M. Sand, C. J. Smith, S. E. Bauer, P. M. Forster, J. S.
Fuglestvedt, S. Osprey, and C.‐F. Schleussner. 2018. "Climate Impacts
from a Removal of Anthropogenic Aerosol Emissions." *Geophysical
Research Letters* 45 (2): 1020--29.
<https://doi.org/10.1002/2017GL076079>.

Schwarz, M., D. Folini, S. Yang, R. P. Allan, and M. Wild. 2020.
"Changes in Atmospheric Shortwave Absorption as Important Driver of
Dimming and Brightening." *Nature Geoscience* 13 (2): 110--15.
<https://doi.org/10.1038/s41561-019-0528-y>.

Siomos, Nikolaos, Dimitris S. Balis, Kalliopi A. Voudouri, Eleni
Giannakaki, Maria Filioglou, Vassilis Amiridis, Alexandros Papayannis,
and Konstantinos Fragkos. 2018. "Are EARLINET and AERONET Climatologies
Consistent? The Case of Thessaloniki, Greece." *Atmospheric Chemistry
and Physics* 18 (16): 11885--903.
<https://doi.org/10.5194/acp-18-11885-2018>.

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

Yu, Lan, Ming Zhang, Lunche Wang, Wenmin Qin, Daoyang Jiang, and Junli
Li. 2022. "Variability of Surface Solar Radiation Under Clear Skies over
Qinghai-Tibet Plateau: Role of Aerosols and Water Vapor." *Atmospheric
Environment* 287 (October): 119286.
<https://doi.org/10.1016/j.atmosenv.2022.119286>.

Yuan, Menghan, Thomas Leirvik, and Martin Wild. 2021. "Global Trends in
Downward Surface Solar Radiation from Spatial Interpolated Ground
Observations During 1961-2019." *Journal of Climate*, September, 1--56.
<https://doi.org/10.1175/JCLI-D-21-0165.1>.

Zerefos, C. S., K. Eleftheratos, C. Meleti, S. Kazadzis, A. Romanou, C.
Ichoku, G. Tselioudis, and A. Bais. 2009. "Solar Dimming and Brightening
over Thessaloniki, Greece, and Beijing, China." *Tellus B: Chemical and
Physical Meteorology* 61 (4): 657.
<https://doi.org/10.1111/j.1600-0889.2009.00425.x>.
