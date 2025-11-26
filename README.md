
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clockworks

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/devel%20version-0.2.22-blue.svg)](https://github.com/pascalnoser/clockworks)
[![](https://img.shields.io/github/languages/code-size/pascalnoser/clockworks.svg)](https://github.com/pascalnoser/clockworks)
<!-- badges: end -->

<img src="man/figures/clockworks_logo_v2.png" width="150" align="right"/>

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

# The input data is a matrix or data frame with feature IDs as row names
print(cw_data[1:5, 1:5])
#>          CT00_S1  CT00_S2  CT00_S3  CT00_S4  CT02_S1
#> Gene_01 3.541119 3.632892 3.650497 3.639091 3.726633
#> Gene_02 4.450808 4.317536 4.467017 4.353382 5.404408
#> Gene_03 4.984153 8.497662 4.629792 6.002132 6.263176
#> Gene_04 6.411975 6.667066 6.987838 5.369700 5.014173
#> Gene_05 5.618924 6.541327 7.519745 5.659031 3.111079

# The meta data requires a column with sample IDs corresponding to 
# the row names of the data and a column containing the time
print(head(cw_metadata))
#>   Sample_ID Time Group Subject_ID
#> 1   CT00_S1    0     A         S1
#> 2   CT00_S2    0     A         S2
#> 3   CT00_S3    0     B         S3
#> 4   CT00_S4    0     B         S4
#> 5   CT02_S1    2     A         S1
#> 6   CT02_S2    2     A         S2

# Create CircadianData object with default period of 24 hours
cd = CircadianData(
  dataset = cw_data,
  metadata = cw_metadata,
  colname_sample = "Sample_ID",
  colname_time = "Time",
  colname_group = "Group"
)
#> 
#> The following columns in `metadata` will be ignored: Subject_ID

# Look at object before running analysis
print(cd)
#> An object of class 'CircadianData'
#>  Dimensions: 10 features, 96 samples
#>  Feature names: Gene_01 Gene_02 Gene_03 ... Gene_09 Gene_10 
#>  Sample names: CT00_S1 CT00_S2 CT00_S3 ... CT46_S3 CT46_S4 
#> 
#> Metadata preview:
#>         time group
#> CT00_S1    0     A
#> CT00_S2    0     A
#> CT00_S3    0     B
#> CT00_S4    0     B
#> CT02_S1    2     A
#> CT02_S2    2     A
#> 
#> Dataset preview:
#>  [showing 6 features x 6 samples]
#>          CT00_S1  CT00_S2  CT00_S3  CT00_S4  CT02_S1  CT02_S2
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
#>      0  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 
#>      2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2 
#>     
#>     $B
#>     
#>      0  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 
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
#> Wave Parameters:
#>  [Not calculated]
#> 
#> Results:
#>  [No results stored]

# Run analysis using RAIN
cd = clockworks(cd, method = "RAIN")
# Run analysis using JTK_CYCLE
cd = clockworks(cd, method = "JTK_CYCLE")

# Extract formatted (standardised) results
res = get_results(cd)
head(res$RAIN)
#>   feature group period_estimate         pval     pval_adj method hr_period
#> 1 Gene_01     A              24 5.280247e-32 5.280247e-31   RAIN        24
#> 2 Gene_02     A              24 2.513425e-29 1.256713e-28   RAIN        24
#> 3 Gene_03     A              24 1.795976e-01 2.993293e-01   RAIN        24
#> 4 Gene_04     A              24 8.390387e-02 2.097597e-01   RAIN        24
#> 5 Gene_05     A              24 8.537546e-01 8.537546e-01   RAIN        24
#> 6 Gene_06     A              24 2.138942e-02 7.129806e-02   RAIN        24
#>   hr_phase_estimate hr_mesor_estimate hr_amplitude_estimate
#> 1        11.4090051          4.301432             0.7053057
#> 2         6.4617936          4.576863             1.4168445
#> 3        13.9199917          5.886923             0.4085035
#> 4         0.4821957          6.297386             0.4843375
#> 5        13.3053857          6.056194             0.1668934
#> 6         6.3201412          6.025998             0.3418065
#>   hr_relative_amplitude_estimate
#> 1                     0.16396996
#> 2                     0.30956671
#> 3                     0.06939169
#> 4                     0.07691087
#> 5                     0.02755747
#> 6                     0.05672197
head(res$JTK_CYCLE)
#>   feature group period_estimate phase_estimate amplitude_estimate         pval
#> 1 Gene_01     A              24             12          0.6508123 1.096605e-30
#> 2 Gene_02     A              24              7          1.2826325 5.092351e-27
#> 3 Gene_03     A              24             16          0.2759111 8.160690e-01
#> 4 Gene_04     A              24              1          0.3366538 3.292045e-01
#> 5 Gene_05     A              24             12          0.2693553 1.000000e+00
#> 6 Gene_06     A              24              7          0.2975580 5.159350e-01
#>       pval_adj    method hr_period hr_phase_estimate hr_mesor_estimate
#> 1 1.096605e-29 JTK_CYCLE        24        11.4090051          4.301432
#> 2 2.546176e-26 JTK_CYCLE        24         6.4617936          4.576863
#> 3 1.000000e+00 JTK_CYCLE        24        13.9199917          5.886923
#> 4 1.000000e+00 JTK_CYCLE        24         0.4821957          6.297386
#> 5 1.000000e+00 JTK_CYCLE        24        13.3053857          6.056194
#> 6 1.000000e+00 JTK_CYCLE        24         6.3201412          6.025998
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
$y = M + A \cos(\frac{2 \pi}{T}(t - \varphi))$ with $M$ the mesor, $A$
the amplitude, $t$ the time (e.g. in hours), and $T$ and $\phi$ the
period and phase in the same units as $t$. This model was chosen because
the value of $\varphi$ corresponds to the time point of the peak of the
fitted wave.
