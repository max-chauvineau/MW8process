#' @title
#' MotionWatch 8 - Extraction des données npcra
#'
#' @description
#' Cette fonction extrait les résultats de l'analyse des rythmes circadiens non-paramétriques (npcra) de la MotionWatch 8.
#'
#' @author
#' European Sleep Center (Paris, France)
#'
#' Maxime Chauvineau - mchauvineau@europeansleepcenter.fr
#'
#' @returns
#' Tableau de données.
#'
#' @param file Chemin du fichier .mtn
#'
#' @examples
#' #extract_npcra()
#'
#' @export

extract_npcra <- function(file) {

  # Extraire tous les nœuds <ann>
  ann_nodes <- xml2::xml_find_all(xml2::read_xml(file), "//ann[@type='npcra']")

  if (length(ann_nodes) > 0) {

    # Définir les variables d'intérêt
    variables_of_interest <- c(
      "NPCRARep:StartHour:SpreadSheetFriendlyValue", "NPCRARep:Length:SpreadSheetFriendlyValue", "NPCRARep:L5:SpreadSheetFriendlyValue",
      "NPCRARep:L5Hour:SpreadSheetFriendlyValue", "NPCRARep:M10:SpreadSheetFriendlyValue", "NPCRARep:M10Hour:SpreadSheetFriendlyValue",
      "NPCRARep:RA:SpreadSheetFriendlyValue", "NPCRARep:IS:SpreadSheetFriendlyValue", "NPCRARep:IV:SpreadSheetFriendlyValue"
    )

    # Extraction des propriétés pour chaque nuit
    npcra_list <- lapply(ann_nodes, function(ann_node) {
      # Extraire l'attribut id et le timestamp `time`
      id <- xml2::xml_attr(ann_node, "id")
      time <- as.POSIXct(xml2::xml_attr(xml2::xml_find_first(ann_node, "./.."), "time"), format = "%Y-%m-%d %H:%M:%S")

      # Trouver tous les <property> sous chaque <ann>
      properties <- xml2::xml_find_all(ann_node, ".//property")

      # Extraire les noms et les valeurs
      names <- xml2::xml_text(xml2::xml_find_all(properties, ".//name"))
      values <- xml2::xml_text(xml2::xml_find_all(properties, ".//content"))

      # Filtrer uniquement les variables d'intérêt
      interest_mask <- !is.na(names) & !is.na(values) & names %in% variables_of_interest
      current_npcra <- stats::setNames(as.list(values[interest_mask]), names[interest_mask])

      # Ajouter l'id et le timestamp pour chaque nuit
      current_npcra$id <- id
      current_npcra$time <- time

      return(current_npcra)
    })

    # Convertir la liste des nuits en data frame
    df_npcra_mtn <- dplyr::bind_rows(lapply(npcra_list, function(x) {
      as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
    }))

    # Renommer les colonnes en supprimant les suffixes inutiles
    names(df_npcra_mtn) <- gsub(":SpreadSheetFriendlyValue", "", names(df_npcra_mtn))
    names(df_npcra_mtn) <- gsub("NPCRARep:", "", names(df_npcra_mtn))

    # Convertir certaines colonnes en numérique
    df_npcra_mtn <- df_npcra_mtn %>%
      dplyr::group_by(id, time) %>%
      dplyr::summarize(across(everything(), ~ first(na.omit(.)), .names = "{.col}"), .groups = "drop") %>%
      dplyr::mutate(time = as.POSIXct(as.numeric(time), origin = "1970-01-01", tz = "UTC"),
             StartHour = as.POSIXct(StartHour, tz = "UTC")) %>%
      dplyr::mutate(across(where(~ is.character(.) && any(grepl(",", .))), ~ as.numeric(gsub(",", ".", .)))) %>%
      dplyr::mutate(dplyr::across(c(id, Length, L5, M10, RA, IS, IV),
                    as.numeric)) %>%
      dplyr::mutate(dplyr::across(c(L5Hour, M10Hour), ~ as.POSIXct(as.numeric(.) * 3600, origin = "1970-01-01", tz = "UTC"))) %>%
      dplyr::filter(time == max(time)) %>%
      dplyr::ungroup() %>%
      stats::na.omit() %>%
      dplyr::select(-c(id, time))

    return(df_npcra_mtn)

  }
}
