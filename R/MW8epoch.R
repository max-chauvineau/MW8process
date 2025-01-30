#' @title
#' MotionWatch 8 - Extraction de l'epoch
#'
#' @description
#' Cette fonction extrait l'epoch définit lors de la programmation de la MotionWatch 8.
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
#' #extract_epoch()
#'
#' @export

extract_epoch <- function(file) {
  as.numeric(
    xml2::xml_text(
      xml2::xml_find_first(
        xml2::read_xml(file),
        "//channel[name = 'motion']//following-sibling::epoch"
      )
    )
  )
}
