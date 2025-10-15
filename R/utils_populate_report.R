# Get all Rmds
rmd_index <- system.file("rmd", "index.Rmd", package = "picture.platform")
rmd_titlepage <- system.file("rmd", "cohort_titlepage.Rmd", package = "picture.platform")
tex_preamble <- system.file("rmd/templates", "preamble.tex", package = "picture.platform")


populate_report <- function(cfg, dataset_description, cohorts_servers, cohort_defs) {

  shiny::observe({

    cfg_analysis <- cfg$analysis

    # Need to hard code as these can't be identified dynamically for the reactive
    df_pde <- cohorts_servers$df_pde()

    # Abandon if no data provided
    if (is.null(df_pde)) return()

    # sapply mangles list of lists so doing this manually
    outps <- list()
    for (cfga in cfg_analysis) {
      outi <- .tab2section(cfga, cohorts_servers)
      outps <- c(outps, outi)
    }

    # Build the input list
    input_params <- c(
      # Title page
      list(
        list(
          rmd_file = rmd_index,
          title = cfg$title
        ),
        list(
          rmd_file = rmd_titlepage,
          rmd_demote = 0,
          cohort_defs = cohort_defs(),
          analysis_description = cfg$description,
          dataset_description = dataset_description
        )
      ),
      # Each section
      outps
      # unlist(sapply(cfg_analysis, .tab2section, cohorts_servers = cohorts_servers), recursive = FALSE)
    )
    # Run prepress on the parameters
    result <- prepressr::prepress(input_params, default_rmd_demotes = 1)


    # Render the book
    book <- bookdown::render_book(
      result$output_dir,
      params = result$params,
      output_format = rmarkdown::pdf_document(
        number_sections = T, includes = list(in_header = tex_preamble)),
      clean = F
    )

    # Open the PDF for the user
    browseURL(book)

  })

}


.tab2section <- function(cfg_tab, cohorts_servers) {
  cfg_tab$methodList <- stripname(cfg_tab$methodList, "analysis_cols")

  params <- c(
    list(prepressr::generate_section_rmd(cfg_tab$tab)),
    sapply(cfg_tab$methodList, .method2prepressr, cohorts_servers = cohorts_servers)
  )

  params <-  params[lengths(params) != 0]

}

.method2prepressr <- function(mthd, cohorts_servers) {

  # Template or direct method
  if (stringr::str_starts(mthd$fn, "tpl_pde") || stringr::str_starts(mthd$fn, "tpl_event")) {

    # Template
    f_report <- .get_func_from_name(mthd$fn, "_report", mthd$rpkg)
    param_list <- .subs_rdvs_in_params(mthd$params, cohorts_servers)

    # Remove any params not required for this function
    valid_params <- param_list[names(param_list) %in% formalArgs(f_report)]

    # Call the server function
    params <- do.call(f_report, valid_params)

  } else {

    rpkg <- ifelse(is.null(mthd$rpkg), "base", mthd$rpkg)
    full_fn <- paste0(mthd$fn, "-report.Rmd")
    rmd_file <- system.file("rmd", full_fn, package = rpkg)

    if (rmd_file == "") rmd_file <- system.file("inst", "rmd", full_fn, package = rpkg)

    if (rmd_file == "") {
      logger::log_warn(paste(full_fn, "is not available in package", rpkg))
      shiny::showNotification(paste(full_fn, "is not available in package", rpkg))
    }

    # Direct rmd
    params <- list(c(
      list(rmd_file = rmd_file),
      .subs_rdvs_in_params(mthd$params, cohorts_servers, TRUE)
    ))

  }

  return(params)
}

# recursive function to remove name from all levels of list
# needed to remove analysis cols from reports. This is
# not generally used as reports are made with all data not limited
stripname <- function(x, name) {
  thisdepth <- depth(x)
  if (thisdepth == 0) {
    return(x)
  } else if (length(nameIndex <- which(names(x) == name))) {
    x <- x[-nameIndex]
  }
  return(lapply(x, stripname, name))
}

# function to find depth of a list element
# max_depth of 500 used as its unlikely to be reached in an app yaml
# see http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
depth <- function(this, thisdepth=0){
  max_depth <- 500
  if (!is.list(this)) {
    return(thisdepth)
  } else if(thisdepth > max_depth){
    return("Max depth reached")
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))
  }
}

.get_file_from_name <- function(fn, suffix, rpkg = "base") {
  full_fn <- paste0(fn, suffix)

}


# This function converts the list of YAML parameters into a list of parameters
# that can be do.call with a function to run the analysis.
# If the parameter is a reactive function, then it can be called and replaced
# by it's value with the parameter call_reactives
.subs_rdvs_in_params <- function(yaml_params, cohorts_servers, call_reactives = FALSE) {

  # Substitute any cohort servers
  available_rdvs <- names(cohorts_servers)
  used_rdvs_idx <- yaml_params %in% available_rdvs
  used_rdvs <- unlist(yaml_params[used_rdvs_idx])
  subs_rdvs <- cohorts_servers[used_rdvs]
  if(call_reactives) {
    subs_rdvs <- lapply(subs_rdvs, do.call, args = list())
    names(yaml_params)[used_rdvs_idx] <- stringr::str_remove(names(yaml_params)[used_rdvs_idx], "_server$")
  }
  yaml_params[used_rdvs_idx] <- subs_rdvs

  return(yaml_params)
}
