#' @title
#' MotionWatch 8 - Extraction du début de l'enregistrement
#'
#' @description
#' Cette fonction extrait la date et l'heure de début de l'enregistrement par la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Date-heure.
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_start_recording()
#'
#' @export

extract_start_recording <- function(file) {
  as.POSIXct(
    xml2::xml_text(
      xml2::xml_find_first(
        xml2::read_xml(file),
        "//property[name = '=StartTime']//following-sibling::content"
      )
    ),
    format="%Y-%m-%d %H:%M:%S", tz="UTC") + lubridate::hours(extract_TZadj(file))
}
