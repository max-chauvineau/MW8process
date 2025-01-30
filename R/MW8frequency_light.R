#' @title
#' MotionWatch 8 - Extraction de l'échantillonnage de la luminosité
#'
#' @description
#' Cette fonction extrait la fréquence d'enregistrement des données de luminosité de la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Nombre numérique.
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_frequency_light()
#'
#' @export

extract_frequency_light <- function(file) {
  freq <- NULL
  tryCatch({
    freq <- as.numeric(
      xml2::xml_text(
        xml2::xml_find_first(
          xml2::read_xml(file),
          "//channel[name = 'Light']//following-sibling::epoch"
        )
      )
    )
  }, error = function(e) {
    message("Could not find light frequency.")
  })
  return(freq)
}
