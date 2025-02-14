#' @title
#' MotionWatch 8 - Extraction de l'ajustement du fuseau horaire
#'
#' @description
#' Cette fonction extrait l'ajustement du fuseau horaire en heure(s) (à modifier dans MotionWare).
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Temps en heure(s).
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_TZadj()
#'
#' @export

extract_TZadj <- function(file) {

  TZadj_nodes <- xml2::xml_find_all(
    xml2::read_xml(file),
    "//property[name = '=TimeZoneAdjusted']//following-sibling::content"
  )

  if (length(TZadj_nodes) != 0) {
    TZadj <- as.numeric(
      tail(
        xml2::xml_text(
          TZadj_nodes
        ), 1
      )
    )/60
  } else {
    TZadj <- 0
  }

  return(TZadj)
}
