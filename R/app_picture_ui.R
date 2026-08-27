#' Shiny UI function for the full PICTURE platform
app_picture_ui <- function(app_config) {

  ui_bootstrap_gosh(
    logo_xl = shiny::div("PICTURE"),
    logo_xs = shiny::div("P"),
    sidebar_menu = bs4Dash::sidebarMenu(
      id = "main_menu",
      .app_gen_menuitems(app_config)
    ),
    body = .body_main_ui(app_config)
  )

}


.app_gen_menuitems <- function(app_config) {

  # Initialise menu with home button
  shiny::tagList(
    bs4Dash::menuItem(
      "Home", tabName = "home_tab", icon = shiny::icon("house")
    ),
    bs4Dash::menuItem(
      "App Creator", tabName = "app_creator_tab", icon = shiny::icon("sliders")
    ),
    # Dynamic menu elements
    bs4Dash::menuItemOutput("patient_select_menuitem"),
    bs4Dash::menuItemOutput("cohort_select_menuitem"),
    shiny::actionButton("run_analysis", "Run", style = "display: none;"),
    bs4Dash::menuItemOutput("analysis_menuitem"),
    shiny::hr(class = "w-100"),
    bs4Dash::bs4SidebarMenuItem(
      "Support", tabName = NULL,
      href = "",
      newTab = TRUE, icon = shiny::icon("headset")
    ),
    bs4Dash::menuItem(
      "About", tabName = "about_tab", icon = shiny::icon("info")
    ),
    shiny::hr(class = "w-100"),
    shiny::p(
      class = "menu-mini-label",
      paste0("picture.platform library v", packageVersion("picture.platform")),
      shiny::tags$br(),
      paste0("driveanalytics library v", packageVersion("driveanalytics"))
    )
  )
}


.cfg2btn <- function(cfg) {
  paste0("application_select_button_", cfg$id)
}


#' Function to create main body tab item window
.body_main_ui <- function(app_config) {

  cfg2card <- function(cfg) {

    bs4Dash::box(
      width = NULL,
      title = shiny::fluidRow(
        shiny::column(width = 10, cfg$title),
        shiny::column(
          width = 2,
          shiny::actionButton(.cfg2btn(cfg), "Select", class="stretched-link")
        )
      ),
      closable = FALSE,
      collapsible = FALSE,
      footer = paste("Created by", cfg$creator),
      shiny::img(
        src = cfg$img, height = "110px",
        style = "max-width: 100%;", class="d-block mx-auto"
      ),
      cfg$description
    )
  }

  core_tabitems <- list(

    # Home tab
    bs4Dash::tabItem(
      tabName = "home_tab",
      # shiny::h4("Select an analysis to perform ..."),
      bs4Dash::bs4CardLayout(
        type = "deck",
        lapply(app_config, cfg2card)
      )
    ),

    # App Creator tab (drag-and-drop YAML maker)
    bs4Dash::tabItem(
      tabName = "app_creator_tab",
      yaml_maker_tab_ui()
    ),

    # Patient select tab
    bs4Dash::tabItem(
      tabName = "patient_select_tab",
      shiny::uiOutput("patient_tab_output")
    ),

    # Cohort select tab
    bs4Dash::tabItem(
      tabName = "cohort_select_tab",
      shiny::uiOutput("cohort_tab_output")
    ),

    # About tab
    bs4Dash::tabItem(
      tabName = "about_tab",
      "TODO"
    )
  )

  # Pregenerate 30 tabs for housing analyses, this is not ideal, but dynamic tab
  # generation is very awkward in shiny
  analysis_tabitem_names <- paste0("analysis_tab_", 1:30)
  fn_tabitem <- function(name) {
    bs4Dash::tabItem(
      tabName = name,
      shiny::uiOutput(paste0(name, "_output"))
    )
  }
  analysis_tabitems <- lapply(analysis_tabitem_names, fn_tabitem)

  # Return all tabitems in a tabItems container
  do.call(bs4Dash::tabItems, c(core_tabitems, analysis_tabitems))

}
