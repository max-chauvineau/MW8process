
# MW8process

<!-- badges: start -->
<!-- badges: end -->

MW8process: Open-source R package to capture and process actigraphy data from MotionWatch8 device and MotionWare software.

## Installation

You can install the development version of MW8process from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("max-chauvineau/MW8process")
```

## Chargement du package

``` r
library(MW8process)
```

## Fonctions du package

```{r functions}
# extract_all() -> extrait toutes les données

# extract_sleep_analysis() -> extrait les résultats de l'analyse du sommeil nuit par nuit

# extract_motion() -> extrait les données de mouvement (count)

# extract_npcra() -> extrait les résutats du npcra

# extract_light() -> extrait les données de luminosité

# extract_start_recording() -> extrait la date et l'heure de début de l'enregistrement

# extract_userID() -> extrait l'id

# extract_axial_mode() -> extrait le mode utilisé

# extract_device_SN() -> extrait le SN dfu MW8

# extract_device_version() -> extrait la version du MW8

# extract_position() -> extrait la position définie

# extract_sex() -> extrait le sexe

# extract_epoch() -> extrait l'epoch définie

# extract_frequency_light() -> extrait la fréquence de la luminosité
```

