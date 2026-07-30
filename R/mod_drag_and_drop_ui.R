yaml_maker_ui <- function(mod_dirs = c("driveanalytics/R")) {
    bs4Dash::bs4DashPage(
        header  = bs4Dash::dashboardHeader(title = "YAML Maker"),
        sidebar = .sidebar_menu(),
        body    = .yaml_maker_body_main_ui(mod_dirs)
    )
}


.sidebar_menu <- function() {
    bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
            id = "current_tab",
            bs4Dash::menuItem(
                "App creator",
                tabName = "app_creation_tab",
                icon = shiny::icon("sliders")
            ),
            bs4Dash::menuItem(
                "Data preview",
                tabName = "data_preview_tab",
                icon = shiny::icon("database")
            ),
            bs4Dash::menuItem(
                "App preview",
                tabName = "app_preview_tab",
                icon = shiny::icon("laptop-code")
            )
        )
    )
}


.yaml_maker_body_main_ui <- function(mod_dirs) {
    bs4Dash::dashboardBody(
        bs4Dash::tabItems(
            .tab_app_creation(mod_dirs),
            .tab_data_preview(),
            .tab_app_preview()
        )
    )
}


.tab_app_creation <- function(mod_dirs) {
    bs4Dash::tabItem(
        tabName = "app_creation_tab",
        .app_creation_body(mod_dirs)
    )
}

.app_creation_body <- function(mod_dirs) {
    shiny::fluidRow(
        shiny::column(
            width = 7,
            bs4Dash::box(
                title = "Core parameters",
                width = 12, # Width of 12 as its the with of the sub column
                shiny::textInput(
                    "app_title",
                    "App title",
                    placeholder = "My App"
                ),

                shiny::textAreaInput(
                    "app_description",
                    "App description",
                    rows = 2,
                    resize = "vertical"
                ),

                shiny::textInput(
                    "app_creator",
                    "App creator",
                    placeholder = "Jane Doe"
                ),

                shiny::selectInput(
                    "app_png",
                    "App image",
                    .get_images()
                ),

                shiny::selectInput(
                    "app_dataset",
                    "Dataset used for analysis",
                    .get_datasets()
                ),

                shiny::checkboxInput(
                    "app_cohort_builder",
                    "Offer the users the ability to create new cohorts",
                    TRUE
                )
            ),

            bs4Dash::box(
                title = "Cohort builder",
                width = 12,
                cohort_builder_ui("yaml_cohort")
            ),

            bs4Dash::box(
                title = "test title",
                width = 12,
                sortable::bucket_list(
                    header = "Drag the modules into any order",
                    group_name = "modules_group",
                    sortable::add_rank_list(
                        text = "Avaliable modules",
                        labels = .get_avaliable_mod(mod_dirs),
                        input_id = "modules_avaliable"
                    ),
                    sortable::add_rank_list(
                        text = "Added modules",
                        labels = NULL,
                        input_id = "modules_added"
                    )
                )
            ),

            # corresponding params for modules in modules_added
            bs4Dash::box(
                title = "Parameter tuning",
                width = 12,
                shiny::uiOutput("module_param_boxes")
            )
        ),

        shiny::column(
            width = 5,
            style = "position: sticky; top: 20px; align-self: flex-start;",
            bs4Dash::box(
                title = "Yaml output",
                width = 12,
                collapsible = FALSE,
                shiny::div(
                    style = "height: 80vh; overflow-y: auto; padding-right: 10px;",
                    shiny::verbatimTextOutput("yaml_out")
                ),
                shiny::fluidRow(
                    shiny::column(
                        width = 8,
                        shiny::textInput(
                            "yaml_out_filename",
                            "Filename"
                        )
                    ),
                    shiny::column(
                        width = 4,
                        shiny::actionButton(
                            "yaml_out_button",
                            "Create YAML"
                        )
                    )
                )
            )
        )
    )
}

.tab_data_preview <- function() {
    bs4Dash::tabItem(
        tabName = "data_preview_tab",
        .data_preview_body()
    )
}

.data_preview_body <- function() {
    shiny::fluidRow(
        shiny::column(
            width = 12,
            bs4Dash::box(
                title = "Data preview",
                width = 12,
                shiny::p(
                    "This tab is to look at the first few columns to see what to stratify by for _col"
                ),
                shiny::checkboxInput(
                    "simple_names",
                    "Simple names",
                    TRUE
                ),
                shiny::selectInput(
                    "data_select",
                    "Select what data table to preview",
                    .get_data_names()
                ),
                shiny::div(
                    style = "overflow-x: auto; width: 100%;", 
                    DT::dataTableOutput("data_preview_table")
                )
            )
        )
    )
}

.tab_app_preview <- function() {
    bs4Dash::tabItem(
        tabName = "app_preview_tab",
        .app_preview_body()
    )
}

.app_preview_body <- function() {
    shiny::verbatimTextOutput("app_preview")
}

#' YAML maker UI as a single embeddable tab for the main PICTURE app
#'
#' Wraps the app-builder's three panels (app creator, data preview, app
#' preview) in a nested tabset so the whole builder lives under one sidebar
#' menu item in the main app.
yaml_maker_tab_ui <- function(mod_dirs = c("driveanalytics/R")) {
    shiny::tabsetPanel(
        id = "yaml_maker_tabs",
        shiny::tabPanel(
            "App creator", icon = shiny::icon("sliders"),
            shiny::br(),
            .app_creation_body(mod_dirs)
        ),
        shiny::tabPanel(
            "Data preview", icon = shiny::icon("database"),
            shiny::br(),
            .data_preview_body()
        ),
        shiny::tabPanel(
            "App preview", icon = shiny::icon("laptop-code"),
            shiny::br(),
            .app_preview_body()
        )
    )
}

.get_images <- function() {
    image_paths <- list.files("picture.platform/inst/www/images", full.names = TRUE)
    image_paths <- gsub("picture\\.platform/inst/", "", image_paths)

    image_names <- list.files("picture.platform/inst/www/images", full.names = FALSE)
    image_names <- gsub("\\.(png|jpg)$", "", image_names)
    
    names(image_paths) <- image_names
    return(image_paths)
}


# This is more of a server side function however it is more useful to be in ui with .get_datasets
.get_data_names <- function(source = "caboodle", simple_names = FALSE, dataset = NULL) {
    files <- list.files("picture.platform/DummyData/parquet") # TODO have this not hard coded
    if (simple_names && !is.null(dataset)) {
        simple <- gsub(paste(dataset, source, "", sep = "_"), "", files)
        simple <- gsub("__", "_", simple)
        names(files) <- simple
        return(files)
    }
    return(files)
}


.get_datasets <- function(...) {
    dataset_names <- .get_data_names(...) # Needs to be changes for proper implementation of the data and its true structure
    dataset_names <- unique((stringr::str_extract(dataset_names, "^[^_]+(?=_)")))
    return(dataset_names)
}

.get_avaliable_mod <- function(mod_dirs) {
    all_mods <- c()
    for (mod_dir in mod_dirs) {
        all_mods <- c(all_mods, list.files(mod_dir))
    }

    # Match all that dont begin with zzz. or utils
    all_mods <- grep("^(zzz|utils).*R$", all_mods, value=TRUE, invert=TRUE)
    all_mods <- grep("-shiny\\.R$", all_mods, value=TRUE)
    all_mods <- gsub("-shiny\\.R$", "", all_mods)

    return(all_mods)
}
