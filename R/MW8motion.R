#' @title
#' MotionWatch 8 - Données de mouvement (counts)
#'
#' @description
#' Cette fonction extrait les données de mouvement de la MotionWatch 8.
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
#' #extract_motion()
#'
#' @export

extract_motion <- function(file) {

  # Extraire les données de mouvement d'origine
  original_motion_text <- xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(file),
      "//channel[name = 'motion']//following-sibling::data"
    )
  )
  original_motion <- as.numeric(unlist(strsplit(original_motion_text, ",")))

  # Créer la série temporelle initiale basée sur les données d'origine
  original_length <- length(original_motion)
  start_date <- extract_start_recording(file)
  epoch <- extract_epoch(file)
  motion <- data.frame(
    time = seq(from = start_date, by = epoch, length.out = length(original_motion)),
    count = original_motion
  )

  # Trouver les sections de changement (modifications manuelles)
  change_nodes <- xml2::xml_find_all(xml2::read_xml(file), "//change[channel[@edit='true']][channel/name='motion']")

  # Si des modifications existent, traiter chaque changement
  if (length(change_nodes) > 0) {
    for (change_node in change_nodes) {
      # Extraire offset et epoch modifié pour le changement
      offset <- as.numeric(xml2::xml_text(xml2::xml_find_first(change_node, ".//offset")))
      modified_epoch <- as.numeric(xml2::xml_text(xml2::xml_find_first(change_node, ".//epoch")))

      # Extraire les nouvelles données de mouvement pour ce changement
      modified_data_text <- xml2::xml_text(xml2::xml_find_first(change_node, ".//data"))
      modified_data <- as.numeric(unlist(strsplit(modified_data_text, ",")))

      # Calculer l'index de début dans motion basé sur l'offset (en secondes)
      start_index <- offset / epoch + 1
      end_index <- start_index + length(modified_data) - 1

      # Si motion est trop courte, l'étendre
      if (end_index > nrow(motion)) {
        additional_rows <- data.frame(
          time = seq(
            from = motion$time[nrow(motion)] + epoch,
            by = epoch,
            length.out = end_index - nrow(motion)
          ),
          count = NA
        )
        motion <- rbind(motion, additional_rows)
      }

      # Remplacer les valeurs d'origine par les nouvelles données
      motion$count[start_index:end_index] <- modified_data
    }
  }

  motion$count[motion$count == -1] <- NA
  motion <- motion %>%
    dplyr::filter(dplyr::row_number() >= min(which(!is.na(count))) & dplyr::row_number() <= max(which(!is.na(count))))

  return(motion)
}
