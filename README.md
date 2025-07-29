
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clockworks

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/devel%20version-0.2.7-blue.svg)](https://github.com/pascalnoser/clockworks)
[![](https://img.shields.io/github/languages/code-size/pascalnoser/clockworks.svg)](https://github.com/pascalnoser/clockworks)
<!-- badges: end -->

<img src="man/figures/clockworks_logo_v1.png" width="200" align="right"/>

> clockworks is an R package for streamlined rhythmicity detection in
> time-series data. It serves as a wrapper for several popular analysis
> tools, eliminating the need to wrangle data into tool-specific formats
> or learn each method’s quirks. With simple, consistent inputs,
> clockworks can automatically adjust parameters based on the input data
> and prevent the use of methods not suited to a given dataset. In just
> a few lines of code, the user can easily run multiple methods, compare
> results, and generate basic visualizations.

------------------------------------------------------------------------

## Installation

You can install the development version of clockworks from GitHub with:

``` r
# install.packages("pak")
pak::pak("pascalnoser/clockworks")
```

## Usage

Detecting rhythmic features using your method of choice is very
straightforward. In the example below, we will use a synthetic data set
of two rhythmic and eight non-rhythmic genes. We will analyse them using
[RAIN](https://www.bioconductor.org/packages/release/bioc/html/rain.html)
as well as the [MetaCycle](https://github.com/gangwug/MetaCycle)
implementation the
[JTK_CYCLE](https://doi.org/10.1177/0748730410379711):

``` r
library(clockworks)
#> Warning: replacing previous import 'lme4::lmer' by 'lmerTest::lmer' when
#> loading 'RepeatedCircadian'

# Load example data and meta data
data("cw_data")
data("cw_metadata")

# Create CircadianData object
cd_obj = CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)
#> 
#> The following columns in `metadata` will be ignored: Subject_ID

# Add necessary experiment info. Using default values
cd_obj = add_experiment_info(cd_obj)
#> `period` not provided. Using default value of 24.
#> `data_type` not provided. Using default value of 'norm'.
#> `log_transformed` not provided. Using default value of FALSE.

# Look at object before running analysis
print(cd_obj)
#> An object of class 'CircadianData'
#>  Dimensions: 10 features, 96 samples
#>  Feature names: Gene_01 Gene_02 Gene_03 ... Gene_09 Gene_10 
#>  Sample names: CT01_S1 CT01_S2 CT01_S3 ... CT47_S3 CT47_S4 
#> 
#> Metadata preview:
#>         time group
#> CT01_S1    1     A
#> CT01_S2    1     A
#> CT01_S3    1     B
#> CT01_S4    1     B
#> CT03_S1    3     A
#> CT03_S2    3     A
#> 
#> Dataset preview:
#>  [showing 6 features x 6 samples]
#>          CT01_S1  CT01_S2  CT01_S3  CT01_S4  CT03_S1  CT03_S2
#> Gene_01 3.541119 3.632892 3.650497 3.639091 3.726633 3.854141
#> Gene_02 4.450808 4.317536 4.467017 4.353382 5.404408 5.257496
#> Gene_03 4.984153 8.497662 4.629792 6.002132 6.263176 4.813541
#> Gene_04 6.411975 6.667066 6.987838 5.369700 5.014173 7.096777
#> Gene_05 5.618924 6.541327 7.519745 5.659031 3.111079 5.994656
#> Gene_06 6.409402 5.986600 5.691259 4.843428 5.359518 6.707311
#> 
#> Experiment Info:
#>  $ period: [1] 24
#>  $ data_type: [1] "norm"
#>  $ log_transformed: [1] FALSE
#>  $ n_groups: [1] 2
#>  $ repeated_measures: [1] FALSE
#>  $ n_replicates:
#>     $A
#>     
#>      1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 
#>      2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
#>     
#>     $B
#>     
#>      1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 
#>      2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
#>      
#>  $ delta_t: [1] 2
#>  $ n_cycles:
#>     $A
#>     [1] 2
#>     
#>     $B
#>     [1] 2
#>      
#> 
#> Results:
#>  [No results stored]

# Run analysis using RAIN
cd_obj = clockworks(cd_obj, method = "RAIN")
# Run analysis using JTK_CYCLE
cd_obj = clockworks(cd_obj, method = "JTK_CYCLE")

# Show results
res = results(cd_obj)
head(res$RAIN$res_formatted)
#>   feature group period_estimate         pval     pval_adj method hr_period
#> 1 Gene_01     A              24 5.280247e-32 5.280247e-31   RAIN        24
#> 2 Gene_02     A              24 2.513425e-29 1.256713e-28   RAIN        24
#> 3 Gene_03     A              24 1.795976e-01 2.993293e-01   RAIN        24
#> 4 Gene_04     A              24 8.390387e-02 2.097597e-01   RAIN        24
#> 5 Gene_05     A              24 8.537546e-01 8.537546e-01   RAIN        24
#> 6 Gene_06     A              24 2.138942e-02 7.129806e-02   RAIN        24
#>   hr_phase_estimate hr_mesor_estimate hr_amplitude_estimate
#> 1         -6.409005          4.301432             0.7053057
#> 2         -1.461794          4.576863             1.4168445
#> 3         -8.919992          5.886923             0.4085035
#> 4        -19.482196          6.297386             0.4843375
#> 5         -8.305386          6.056194             0.1668934
#> 6         -1.320141          6.025998             0.3418065
#>   hr_relative_amplitude_estimate
#> 1                     0.16396996
#> 2                     0.30956671
#> 3                     0.06939169
#> 4                     0.07691087
#> 5                     0.02755747
#> 6                     0.05672197
head(res$JTK_CYCLE$res_formatted)
#>   feature group period_estimate phase_estimate amplitude_estimate         pval
#> 1 Gene_01     A              24             13          0.6508123 1.096605e-30
#> 2 Gene_02     A              24              8          1.2826325 5.092351e-27
#> 3 Gene_03     A              24             17          0.2759111 8.160690e-01
#> 4 Gene_04     A              24              2          0.3366538 3.292045e-01
#> 5 Gene_05     A              24             13          0.2693553 1.000000e+00
#> 6 Gene_06     A              24              8          0.2975580 5.159350e-01
#>       pval_adj    method hr_period hr_phase_estimate hr_mesor_estimate
#> 1 1.096605e-29 JTK_CYCLE        24         -6.409005          4.301432
#> 2 2.546176e-26 JTK_CYCLE        24         -1.461794          4.576863
#> 3 1.000000e+00 JTK_CYCLE        24         -8.919992          5.886923
#> 4 1.000000e+00 JTK_CYCLE        24        -19.482196          6.297386
#> 5 1.000000e+00 JTK_CYCLE        24         -8.305386          6.056194
#> 6 1.000000e+00 JTK_CYCLE        24         -1.320141          6.025998
#>   hr_amplitude_estimate hr_relative_amplitude_estimate
#> 1             0.7053057                     0.16396996
#> 2             1.4168445                     0.30956671
#> 3             0.4085035                     0.06939169
#> 4             0.4843375                     0.07691087
#> 5             0.1668934                     0.02755747
#> 6             0.3418065                     0.05672197
```

The column names in the formatted output follow the same pattern for all
methods. Furthermore, the results of a harmonic regression are also
included in this output, allowing for a quick comparison of the
estimated parameters to those of a simple sine wave fit. The harmonic
regression used is equivalent to the model
$y = M + A \sin(\frac{2 \pi}{P}(t + \phi))$ with $M$ the mesor, $A$ the
amplitude, $t$ the time (e.g. in hours), and $P$ and $\phi$ the period
and phase in the same units as $t$.
