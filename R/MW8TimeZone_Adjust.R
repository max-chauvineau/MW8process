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
  TZadj <- as.numeric(
    xml2::xml_text(
      xml2::xml_find_first(
        xml2::read_xml(file),
        "//property[name = '=TimeZoneAdjusted']//following-sibling::content"
      )
    )
  )/60

  TZadj <- ifelse(is.na(TZadj), 0, TZadj)

  return(TZadj)
}
