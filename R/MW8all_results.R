#' @title
#' MotionWatch 8 - Extraction de toutes les données
#'
#' @description
#' Cette fonction fournit une liste des résultats des analyses de la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Liste : bases de données et variables.
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_all()
#'

extract_all <- function(file) {

  return(list(
    sleeper = extract_userID(file),
    sex = extract_sex(file),
    device_position = extract_position(file),
    device_version = extract_device_version(file),
    device_SN = extract_device_SN(file),
    TimeZoneAdjusted = extract_TZadj(file),
    recording_start_date = extract_start_recording(file),
    epoch = extract_epoch(file),
    light_frequency = extract_frequency_light(file),
    df_sleep_analysis = extract_sleep_analysis(file),
    df_raw_count = extract_motion(file),
    df_npcra = extract_npcra(file),
    df_nap = extract_nap(file),
    light_frequency = extract_frequency_light(file),
    df_light = extract_light(file)
  ))

}
