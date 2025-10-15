#' Default start date for selection
#' @export
deafult_start_date <- lubridate::as_date("2020-01-01")

#' List of RDVs and derived RDVs that are usable in PICTURE
#' It is currently necessary to hardcode these because of the way that reactives
#' work.
#' @export
rdv_list <- list(
  "df_pde",
  "df_dia_conditions",
  "df_dia_other",
  "df_pro_opcs",
  "df_pro_lab",
  "df_pro_img",
  "df_pro_other",
  "df_had",
  "df_lab",
  "df_med_admins",
  "df_lab_components"
)
names(rdv_list) <- rdv_list
