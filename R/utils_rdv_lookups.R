# Lookup tables
table_sources <- c("caboodle")
fn_rdv_lookup <- system.file("configs", "rdv_code_lookup.csv",
                             package = "picture.platform")
df_rdv_lookup <- readr::read_csv(fn_rdv_lookup, show_col_types = FALSE)


#' Function to extract all the valid-looking RDV CSVs in a directory
get_rdvs_in <- function(directory) {

  # Build regex
  re <- "^.*/(?!dataset)(.*)"
  re <- paste0(re, "_(", paste0(table_sources, collapse = "|"), ")")
  re <- paste0(re, "_(", paste0(df_rdv_lookup$rdv_type, collapse = "|"), ")")
  re <- paste0(re, ".parquet")

  # Return matches
  f_mat <- stringr::str_match(list.files(directory, full.names = TRUE), re)
  colnames(f_mat) <- c("filename", "group", "source", "type")
  tidyr::as_tibble(f_mat) %>%
    tidyr::drop_na()

}

#' Get all the dataset description yaml files
get_descriptions_in <- function(directory) {

  re <- "^.*/(.*)_description.ya?ml$"

  f_mat <- stringr::str_match(list.files(directory, full.names = TRUE), re)
  colnames(f_mat) <- c("filename", "group")
  tidyr::as_tibble(f_mat) %>%
    tidyr::drop_na()
}


#` Get the RDV code for a given file type
rdv_lookup <- function(value, col_in, col_out) {
  df_rdv_lookup %>%
    dplyr::filter(!!as.name(col_in) == value) %>%
    dplyr::pull(!!as.name(col_out))
}


get_rdv_code_lookup <- function() {
  fn_code_lookup <- system.file("configs", "rdv_code_lookup.csv",
                                package = "picture.platform")
  readr::read_csv(fn_code_lookup, show_col_types = FALSE)
}


get_rdv_Variable_lookup <- function() {
  fn_var_lookup <- system.file("configs", "rdv_variable_lookup.csv",
                               package = "picture.platform")
  readr::read_csv(fn_var_lookup, show_col_types = FALSE)
}


get_rdv_code <- function(rdv) {
  get_rdv_code_lookup() %>%
    dplyr::filter(rdv_code %in% rdv)
}


get_variable_label <- function(rdv, var, pull = "label") {
  # A variable maps to a single label/type; duplicate rows in the lookup table
  # would otherwise return a length->1 vector and break scalar callers (e.g. the
  # node summary text and the filter-modal query type). Keep the first match.
  vals <- get_rdv_Variable_lookup() %>%
    dplyr::filter(rdv_code == rdv & variable_code == var) %>%
    dplyr::pull(!!as.name(pull))
  if (length(vals) == 0) vals else vals[[1]]
}


col_is_select <- function(col) col_is_type(col, 'select')
col_is_text <- function(col) col_is_type(col, 'text')
col_is_date_range <- function(col) col_is_type(col, 'date_range')
col_is_age_range <- function(col) col_is_type(col, 'age_range')
col_is_numeric_range <- function(col) col_is_type(col, 'numeric_range')
col_is_type <- function(col, typec) {
  col %in% unlist(get_type_lookup()[typec,'ls'])
}


get_type_lookup <- function() {

  get_rdv_Variable_lookup() %>%
    dplyr::group_by(input_type) %>%
    dplyr::summarise(
      js = paste0("['", paste(unique(variable_code), collapse = "', '"), "']"),
      ls = list(unique(variable_code))
    ) %>%
    tibble::column_to_rownames("input_type")

}


#' @export
add_codename_col <- function(df_rdv, col_in, col_out) {

  co <- rlang::enquo(col_out)

  # Is the input column a known code column
  code_detect <- stringr::str_match(col_in, "(.*_)(local|nat)_code$")
  # Infer the label column name
  col_name <- paste0(code_detect[2], "name")

  # Abandon if not a known code or name column is not in the RDV
  if (is.na(code_detect[1]) | !(col_name %in% names(df_rdv))) {
    return(dplyr::mutate(df_rdv, !!col_out := !!as.name(col_in)))
  }

  # Return the RDV with the new codename column
  dplyr::mutate(df_rdv, !!col_out := paste(!!as.name(col_in), "-", !!as.name(col_name)))

}

#' Function to call all functions needed to get a list of RDVs and columns
#' to load
#'
#' @param yml list; The app_yaml loaded as a list
#' @param all_rdvs dataframe; Dplyr dataframe of rdvs to load
#'
#' @return comb list; A set of RDVs and columns to load. RDVs are the names and
#' the columns are the values.
#'
#' @family limited
#' @export
get_rdv_cols <- function(yml, all_rdvs){
  initial_config <- rdvs_from_initial_config(yml)
  col_lst <- get_cols_from_analysis(yml)
  initial_config <- rdv_lst_names_to_labels(df_rdv_lookup, initial_config)
  col_lst <- rdv_lst_names_to_labels(df_rdv_lookup, col_lst)
  name_lst <- names(initial_config)
  name_lst <- unique(c(name_lst, names(col_lst)))
  comb <- combine_rdv_cohorts(name_lst, initial_config, col_lst)
  return(comb)
}

#' Load the inital cohort from yaml, check if there is a cohort (or it will be "All)
#'
#'
#' @param app_config list; the app yaml loaded as a list
#'
#' @return col_lst list; RDVs and columns used to make the inital cohort
#'
#' @family limited
#' @noRd
rdvs_from_initial_config <- function(app_config){
  col_lst <- c()
  for(i in app_config$initialCohorts){
    if(!(i$label == "All")){
      for(j in i$config){
        if(j$rdv %in% names(col_lst)){
          col_lst[[j$rdv]] <- unique(c(col_lst[[j$rdv]], j$column))
        }else{
          col_lst[[j$rdv]] <- c("project_id", j$column)
        }
      }
    }
  }
  return(col_lst)
}

#' Takes part of the app yaml where a specific analysis is defined and returns the columns needed
#'
#' @param method_lst list; analysis tab from app yaml
#'
#' @family limited
#' @noRd
get_cols_from_analysis_cols <- function(method_lst){
  rdv_col_lst <- method_lst$params$analysis_cols
  return(rdv_col_lst)
}

#' tpl_pde_all is used for all apps and uses multiple rdvs so loads a yaml
#' that specifies the rdvs and columns needed. Location isn't always needed so
#' its only loaded if its present in the tpl_pde_all_cols.yaml
#'
#' @param col_lst list; list of rdvs and columns that are needed
#'
#' @return method_lst list; method list with columns and rdvs needed by tpe_pde_all
#'
#' @family limited
#' @noRd
get_cols_tpl_pde_all <- function(col_lst){
  tpl_pde_all_yaml <- system.file("configs", "tpl_pde_all_cols.yaml", package = "picture.platform")
  tpl_pde_all_cols <- yaml::read_yaml(tpl_pde_all_yaml)
  col_lst[["pde"]] <- tpl_pde_all_cols[["pde"]]
  if("loc" %in% names(tpl_pde_all_cols)){
    col_lst[["loc"]] <- tpl_pde_all_cols[["loc"]]
  }
  return(col_lst)
}


#' Remove substring "db_" from the rdv name as its not included in rdv_code_lookup.csv
#'
#' @param rdv_string string; rdv name
#'
#' @return rdv_string string; name without "df_"
#'
#' @family limited
#' @noRd
remove_df_from_rdv_name <- function(rdv_string){
  rdv_string <- gsub("df_", "", rdv_string)
  return(rdv_string)
}

#' get_cols_from_analysis loops through all the names of rdv to analyse
#' for different analysis types and gets the columns needed
#'
#' @param app_config list; the app yaml loaded as a list
#'
#' @return col_lst list; all rdvs and columns needed to do the analysis for the PICTURE app
#'
#' @family limited
#' @noRd
get_cols_from_analysis <- function(app_config){
  list <- c()
  col_lst <- list()
  for(i in app_config$analysis){
    for(j in i$methodList){
      if(j["fn"] == "tpl_pde_all"){
        col_lst <- get_cols_tpl_pde_all(col_lst)
      }
      if("df" %in% names(j$params)){
        rdv <- remove_df_from_rdv_name(j$params$df)
        if(rdv %in% names(col_lst)){
          col_lst[[rdv]] <- unique(c(col_lst[[rdv]], get_cols_from_analysis_cols(j)))
        }else{
          col_lst[[rdv]] <- get_cols_from_analysis_cols(j)
        }
      }
      else if("df_rdv" %in% names(j$params)){
        rdv <- remove_df_from_rdv_name(j$params$df_rdv)
        list <- c(j$params$col, j$params$filter_col)
        if(rdv %in% names(col_lst)){
          col_lst[[rdv]] <- unique(c(col_lst[[rdv]], get_cols_from_analysis_cols(j)))
        }else{
          col_lst[[rdv]] <- get_cols_from_analysis_cols(j)
        }
      }
      else if("df_server" %in% names(j$params)){
        rdv <- remove_df_from_rdv_name(j$params$df_server)
        if(rdv %in% names(col_lst)){
          col_lst[[rdv]] <- unique(c(col_lst[[rdv]], get_cols_from_analysis_cols(j)))
        }else{
          col_lst[[rdv]] <- get_cols_from_analysis_cols(j)
        }

      }
      else {
          rdv <- remove_df_from_rdv_name(j$params$df_rdv_server)
          if(!(length(rdv)==0)){
            if(rdv %in% names(col_lst)){
              col_lst[[rdv]] <- unique(c(col_lst[[rdv]], get_cols_from_analysis_cols(j)))
            }else{
              col_lst[[rdv]] <- get_cols_from_analysis_cols(j)
            }
          }
        }
    }
  }
  return(col_lst)
}

#' Takes a list of rdvs and converts them from rdv names to labels as define in rdv_code_lookup.csv
#'
#' @param df_rdv_lookup dataframe; config information on all the types of rdvs known to PICTURE.
#' @param rdv_lst list; list of rdv names needed.
#'
#' @return rdv_lst list; list of rdv labels needed.
#'
#' @family limited
#' @noRd
rdv_lst_names_to_labels <- function(df_rdv_lookup, rdv_lst){
  n <- nrow(df_rdv_lookup)
  for(i in seq(n)){
    rdv <- df_rdv_lookup[i,]
    if(rdv$rdv_code %in% names(rdv_lst)){
      rdv_lst[[rdv$rdv_type]] <- rdv_lst[[rdv$rdv_code]]
      rdv_lst[[rdv$rdv_code]] <- NULL
    }
  }
  return(rdv_lst)
}

#' Takes a list of names and adds them to a new list. If the key is already there the values
#' are just added to the existing key
#'
#' @param all_names output_lst; All rdvs names used in the app
#' @param input_lst list; list of rdv names needed to be added.
#'  @param output_lst list; output list of rdv names to be added to.
#'
#' @return output_lst list; output list of rdv names.
#'
#' @family limited
#' @noRd
combine_list_names <- function(all_names, input_lst, output_lst){
  temp <- c()
  for(i in all_names){
    if(i %in% names(input_lst)){
      if(i %in% names(output_lst)){
        temp <- output_lst[[i]]
        temp <- c(temp, input_lst[[i]])
        output_lst[[i]] <- NULL
        output_lst[[i]] <- temp
      }
      else{
        output_lst[[i]] <- input_lst[[i]]
      }
    }
  }
  return(output_lst)
}

#' Takes a
#'
#'
#' @param all_names output_lst; All rdvs names used in the app
#' @param initial_c_lst list; list of rdvs names used in initial cohort
#'  @param analysis_lst list; list of rdvs names used in analysis cohort
#'
#' @return combined_lst list; output list of all rdvs and cohorts used in the app.
#'
#' @family limited
#' @noRd
combine_rdv_cohorts <- function(name_lst, initial_c_lst, analysis_lst){
  combined_lst <- list()
  combined_lst <- combine_list_names(name_lst, initial_c_lst, combined_lst)
  combined_lst <- combine_list_names(name_lst, analysis_lst, combined_lst)
  return(combined_lst)
}
