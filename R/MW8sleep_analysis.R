#' @title
#' MotionWatch 8 - Analyse du sommeil nuit par nuit
#'
#' @description
#' Cette fonction extrait les résultats de l'analyse du sommeil nuit par nuit de la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Tableau de données
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_sleep_analysis()
#'
#' @export

extract_sleep_analysis <- function(file) {

  # Extraire tous les nœuds <ann> qui contiennent un ID pour différencier les nuits
  ann_nodes <- xml2::xml_find_all(
    xml2::read_xml(file),
    "//ann[@type='sleep']"
  )

  if (length(ann_nodes) > 0) {

    # Définir les variables d'intérêt
    variables_of_interest <- c(
      "SleepRep:StartDay:SpreadSheetFriendlyValue", "SleepRep:StartDayOfWeek:SpreadSheetFriendlyValue",
      "SleepRep:EndDay:SpreadSheetFriendlyValue", "SleepRep:LightsOut:SpreadSheetFriendlyValue",
      "SleepRep:Asleep:SpreadSheetFriendlyValue", "SleepRep:Woke:SpreadSheetFriendlyValue",
      "SleepRep:GotUp:SpreadSheetFriendlyValue", "SleepRep:TimeInBed:SpreadSheetFriendlyValue",
      "SleepRep:AssumedSleep:SpreadSheetFriendlyValue", "SleepRep:ActualSleepTime:SpreadSheetFriendlyValue",
      "SleepRep:ActualSleep%:SpreadSheetFriendlyValue", "SleepRep:ActualWakeTime:SpreadSheetFriendlyValue",
      "SleepRep:ActualWake%:SpreadSheetFriendlyValue", "SleepRep:SleepEfficiency%:SpreadSheetFriendlyValue",
      "SleepRep:SleepLatency:SpreadSheetFriendlyValue", "SleepRep:SleepBouts:SpreadSheetFriendlyValue",
      "SleepRep:WakeBouts:SpreadSheetFriendlyValue", "SleepRep:MeanSleepBout:SpreadSheetFriendlyValue",
      "SleepRep:MeanWakeBout:SpreadSheetFriendlyValue", "SleepRep:ImmobileMins:SpreadSheetFriendlyValue",
      "SleepRep:ImmobileTime%:SpreadSheetFriendlyValue", "SleepRep:MovingMins:SpreadSheetFriendlyValue",
      "SleepRep:MovingTime%:SpreadSheetFriendlyValue", "SleepRep:NumImmobilePhases:SpreadSheetFriendlyValue",
      "SleepRep:MeanLengthImmobility:SpreadSheetFriendlyValue", "SleepRep:OneMinuteImmobility:SpreadSheetFriendlyValue",
      "SleepRep:OneMinImmobility%:SpreadSheetFriendlyValue", "SleepRep:TotalActivityScore:SpreadSheetFriendlyValue",
      "SleepRep:MeanActivityScore:SpreadSheetFriendlyValue", "SleepRep:MeanScoreActivePeriods:SpreadSheetFriendlyValue",
      "SleepRep:FragmentationIndex:SpreadSheetFriendlyValue", "SleepThreshold", "Rest24%",
      "SleepRep:CentralPhaseMeasure:SpreadSheetFriendlyValue"
    )

    # Extraction des propriétés pour chaque nuit
    nights_list <- lapply(ann_nodes, function(ann_node) {
      # Extraire l'attribut id et le timestamp `time`
      id <- xml2::xml_attr(ann_node, "id")
      time <- as.POSIXct(
        xml2::xml_attr(
          xml2::xml_find_first(
            ann_node, "./.."), "time"),
        format = "%Y-%m-%d %H:%M:%S"
      )

      # Trouver tous les <property> sous chaque <ann>
      properties <- xml2::xml_find_all(ann_node, ".//property")

      # Extraire les noms et les valeurs
      names <- xml2::xml_text(xml2::xml_find_all(properties, ".//name"))
      values <- xml2::xml_text(xml2::xml_find_all(properties, ".//content"))

      # Filtrer uniquement les variables d'intérêt
      interest_mask <- !is.na(names) & !is.na(values) & names %in% variables_of_interest
      current_night <- stats::setNames(as.list(values[interest_mask]), names[interest_mask])

      # Ajouter l'id et le timestamp pour chaque nuit
      current_night$id <- id
      current_night$time <- time

      return(current_night)
    })

    # Convertir la liste des nuits en data frame
    df_nights_mtn <- dplyr::bind_rows(lapply(nights_list, function(x) {
      as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
    }))

    # Renommer les colonnes en supprimant les suffixes inutiles
    names(df_nights_mtn) <- gsub(":SpreadSheetFriendlyValue", "", names(df_nights_mtn))
    names(df_nights_mtn) <- gsub("SleepRep:", "", names(df_nights_mtn))

    # Convertir certaines colonnes en numérique
    df_sleep_analysis_mtn <- df_nights_mtn %>%
      dplyr::group_by(id, time) %>%
      dplyr::summarize(dplyr::across(tidyselect::everything(), ~ dplyr::first(stats::na.omit(.)), .names = "{.col}"), .groups = "drop") %>%
      dplyr::mutate(time = as.POSIXct(as.numeric(time), origin = "1970-01-01", tz = "UTC")) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(~ is.character(.) && any(grepl(",", .))), ~ as.numeric(gsub(",", ".", .)))) %>%
      dplyr::group_by(id) %>%
      dplyr::filter(time == max(time)) %>%
      dplyr::group_by(EndDay) %>%
      dplyr::filter(time == max(time)) %>%
      dplyr::arrange(EndDay) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(DayNumber = as.numeric(difftime(EndDay, min(EndDay, na.rm = T), units = "days")) + 1) %>%
      dplyr::mutate(dplyr::across(c(id, SleepThreshold, TimeInBed, AssumedSleep, ActualSleepTime, ActualWakeTime, SleepLatency, SleepBouts,
                                    WakeBouts, ImmobileMins, MovingMins, NumImmobilePhases, OneMinuteImmobility, TotalActivityScore, DayNumber),
                                  as.numeric)) %>%
      dplyr::relocate(c("SleepThreshold", "DayNumber"), .after = tidyselect::last_col()) %>%
      stats::na.omit() %>%
      dplyr::select(-c(id, time))

    return(df_sleep_analysis_mtn)

  }
}
