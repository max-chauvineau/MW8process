
# MW8process

<!-- badges: start -->
<!-- badges: end -->

MW8process: Open-source R package to capture and process actigraphy data from MotionWatch8 device and MotionWare software.

## Installation

You can install the last version of MW8process from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("max-chauvineau/MW8process", build_vignettes = TRUE)
```

## Package Loading

``` r
library(MW8process)
```

## Package Functions

``` r
# extract_all() -> extracts all data and returns the result (as a list)

# extract_sleep_analysis() -> extracts night-by-night sleep analysis results (data frame)

# extract_motion() -> extracts epoch-by-epoch motion (count) data (data frame)

# extract_npcra() -> extracts NPCRA (non-parametric circadian rhythm analysis) results (data frame)

# extract_nap() -> extracts nap data (data frame)

# extract_light() -> extracts epoch-by-epoch light data (data frame)

# extract_start_recording() -> extracts the recording start date and time

# extract_TZadj() -> extracts time zone adjustment in hours (to be modified in MotionWare)

# extract_userID() -> extracts the user ID

# extract_axial_mode() -> extracts the axial mode used

# extract_device_SN() -> extracts the MW8 device serial number (SN)

# extract_device_version() -> extracts the MW8 device version

# extract_position() -> extracts the recorded position setting

# extract_sex() -> extracts sex information

# extract_epoch() -> extracts the epoch setting

# extract_frequency_light() -> extracts light measurement frequency
```
