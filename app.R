library(shiny)
library(dplyr)
library(DT)
library(stringr)
library(tippy)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
source("functions.R")

# Define UI for application
ui <- fluidPage(
  title = "Rangeland Indicator Calculator",
  useShinyjs(),
  tags$head(
    # Use the styles.css file for (nearly) all our styling needs
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # A function that lets us create links to tabs since there's no
    # equivalent to updateTabsetPanel() like updateTabPanel() for some reason.
    # This lets us make links with a(onclick = "fakeClick('Tab Name')")
    # This comes from StackOverflow, I think?
    tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))
  ),
  navbarPage(
    # title = img(src = "combined_logos_hires.png",
    #             height = "45px"),
    title = tags$div(class = "tool-title",
                     "Rangeland Indicator Calculator"),
    id = "navbar-full",
    position = "static-top",
    footer = tags$div(class = "footer",
                      # p(p(a(href = 'mailto:nelson.stauffer@usda.gov',
                      #       'Contact us with questions',
                      #       target = "_blank")))),
                      p(column(width = 6,
                               p(a(href = 'mailto:nelson.stauffer@usda.gov', 'Contact us with questions', target =
                                     "_blank"))),
                        column(width = 6,
                               p(img(src = "combined_logos_hires.png",
                                     width = "95%"))))),
    #### Interface ###################################
    ##### Tabs #######################################
    ###### Start #####################################
    tabPanel(title = "Start",
             sidebarLayout(
               sidebarPanel(
                 HTML(
                   "<div class = 'app-info'>
                    <h3>About</h3>
                    This application calculates ecological indicators from monitoring data collected following methods in the <a href='https://landscapetoolbox.org/methods-manuals/monitoring-manual-2nd-edition/' target='blank'>Monitoring Manual for Grassland, Shrubland, and Savannah Ecosystems</a> using the R package <a href='https://github.com/Landscape-Data-Commons/terradactyl' target='_blank'>terradactyl</a>.
                    <br>
                 </div>"
                 ),
                 selectInput(inputId = "data_type",
                             label = "Data type",
                             choices = c("Line-Point Intercept" = "lpi",
                                         "Height" = "height",
                                         "Gap" = "gap",
                                         "Soil Stability" = "soilstability"),
                             selected = "lpi"),
                 fluidRow(column(width = 10,
                                 radioButtons(inputId = "data_source",
                                              label = "Data source",
                                              choices = c("Download from Landscape Data Commons" = "ldc",
                                                          "Upload tabular data file" = "upload"),
                                              selected = character(0))),
                          column(width = 1,
                                 actionButton(inputId = "data_source_info",
                                              label = "",
                                              icon = icon("info")))),
                 conditionalPanel(condition = "input.data_source == 'upload'",
                                  fileInput(inputId = "raw_data",
                                            label = "Data CSV",
                                            multiple = FALSE,
                                            accept = ".csv")
                 ),
                 # If the data are going to come from the LDC, display options for querying
                 fluidRow(column(width = 10,
                                 uiOutput("query_method_ui")),
                          column(width = 1,
                                 uiOutput("query_method_info_ui"))),
                 # Should the polygons be uploaded or drawn?
                 fluidRow(column(width = 10,
                                 uiOutput("polygon_source_ui"))),
                 # If the query will be key-based, show the text box input for keys
                 fluidRow(column(width = 10,
                                 uiOutput("keys_input_ui")),
                          column(width = 1,
                                 uiOutput("keys_input_info_ui"))),
                 # If the query will be spatial, show the upload bar
                 fluidRow(column(width = 10,
                                 uiOutput("spatial_input_ui")),
                          column(width = 1,
                                 uiOutput("spatial_input_info_ui"))),
                 # If there's an uploaded polygon file, show the options to select a feature
                 fluidRow(column(width = 10,
                                 uiOutput("select_polygon_ui")),
                          column(width = 1,
                                 uiOutput("select_polygon_info_ui"))),
                 fluidRow(uiOutput("polygon_draw_prompt")),
                 # If there's an uploaded polygon file, show the option to repair the polygons
                 fluidRow(column(width = 10,
                                 uiOutput("repair_polygons_ui")),
                          column(width = 1,
                                 uiOutput("repair_polygons_info_ui"))),
                 # If querying the LDC and the query criteria are selected, show
                 # the fetch button
                 # There are three because I have to render them separately depending
                 # on different conditions and I'm not allowed to have multiple
                 # situations render to the same output name. The UI elements
                 # both contain identical fetch buttons though since they can
                 # never coexist.
                 uiOutput("fetch_ui1"),
                 uiOutput("fetch_ui2"),
                 uiOutput("fetch_ui3"),
                 hr()
               ),
               mainPanel(
                 uiOutput("drawing_map_ui"),
                 uiOutput("main_map_ui"),
                 # leaflet::leafletOutput(outputId = "map",
                 #                        height = "80vh"),
                 includeHTML("instructions.html")
               ))),
    ###### Data #####################################
    tabPanel(title = "Data",
             actionButton(inputId = "reset_data",
                          label = "Reset data"),
             tippy_this(elementId = "reset_data",
                        tooltip = "Reset data to original uploaded/downloaded state",
                        placement = "right",
                        delay = c(50, 0)),
             fluidRow(column(width = 10,
                             DT::DTOutput(outputId = "data")))),
    ###### Configure Data #####################################
    tabPanel(title = "Configure Data",
             actionLink(inputId = "data_help",
                        label = "What do these options mean?"),
             fluidRow(
               column(width = 5,
                      checkboxInput(inputId = "show_var_config",
                                    label = "Show variable configuration options",
                                    value = FALSE),
                      conditionalPanel(condition = "input.show_var_config",
                                       helpText("If a variable name is already selected, it should be correct."),
                                       selectInput(inputId = "primarykey_var",
                                                   label = "Variable containing PrimaryKey values",
                                                   choices = c("")),
                                       # LineKey (potentially) matters to LPI, gap, and height
                                       conditionalPanel(condition = "input.data_type == 'gap' || input.data_type == 'lpi' || input.data_type == 'height'",
                                                        selectInput(inputId = "linekey_var",
                                                                    label = "Variable containing LineKey values",
                                                                    choices = c(""))),
                                       # LPI-specific variables
                                       conditionalPanel(condition = "input.data_type == 'lpi'",
                                                        selectInput(inputId = "code_var",
                                                                    label = "Variable containing hit codes",
                                                                    choices = c("")),
                                                        selectInput(inputId = "pointnbr_var",
                                                                    label = "Variable containing the ordinal hit numbers",
                                                                    choices = c("")),
                                                        selectInput(inputId = "layer_var",
                                                                    label = "Variable containing the hit record layers",
                                                                    choices = c(""))),
                                       # Gap-specific variables
                                       conditionalPanel(condition = "input.data_type == 'gap'",
                                                        selectInput(inputId = "linelengthamount_var",
                                                                    label = "Variable containing line lengths",
                                                                    choices = c("")),
                                                        selectInput(inputId = "measure_var",
                                                                    label = "Variable containing the measurement units",
                                                                    choices = c("")),
                                                        selectInput(inputId = "rectype_var",
                                                                    label = "Variable containing the type of gaps",
                                                                    choices = c("")),
                                                        selectInput(inputId = "gap_var",
                                                                    label = "Variable containing gap sizes",
                                                                    choices = c(""))),
                                       # Height-specific variables
                                       conditionalPanel(condition = "input.data_type == 'height'",
                                                        selectInput(inputId = "height_var",
                                                                    label = "Variable containing heights",
                                                                    choices = c("")),
                                                        selectInput(inputId = "species_var",
                                                                    label = "Variable containing the species",
                                                                    choices = c(""))),
                                       # Soil-specific variables
                                       conditionalPanel(condition = "input.data_type == 'soilstability'",
                                                        selectInput(inputId = "rating_var",
                                                                    label = "Variable containing stability ratings",
                                                                    choices = c("")),
                                                        selectInput(inputId = "veg_var",
                                                                    label = "Variable containing vegetative cover type",
                                                                    choices = c("")))),
               ),
               column(width = 6,
                      # Species lookup table stuff
                      conditionalPanel(condition = "input.data_type == 'lpi' || input.data_type == 'height'",
                                       radioButtons(inputId = "species_source",
                                                    label = "Species lookup table source",
                                                    choices = c("None" = "none",
                                                                "Default USDA Plants" = "default",
                                                                "Upload" = "upload"),
                                                    selected = "none"),
                                       conditionalPanel(condition = "input.species_source != 'none'",
                                                        conditionalPanel(condition = "input.data_joining_var != '' && input.species_joining_var != ''",
                                                                         actionButton(inputId = "join_species",
                                                                                      label = "Join species information to data"),
                                                                         HTML("<br>"),
                                                                         conditionalPanel(condition = "input.join_species > 0",
                                                                                          HTML("<br>"),
                                                                                          downloadButton(outputId = 'downloadable_species',
                                                                                                         label = 'Download current species information'),
                                                                                          HTML("<br>"))
                                                                         
                                                        ),
                                                        HTML("<br>"),
                                                        conditionalPanel(condition = "input.species_source == 'upload'",
                                                                         fileInput(inputId = "species_data",
                                                                                   label = "Species CSV",
                                                                                   multiple = FALSE,
                                                                                   accept = ".csv")),
                                                        selectInput(inputId = "data_joining_var",
                                                                    label = "Species joining variable in data",
                                                                    choices = c(""),
                                                                    selected = "",
                                                                    multiple = FALSE),
                                                        selectInput(inputId = "species_joining_var",
                                                                    label = "Species joining variable in lookup table",
                                                                    choices = c(""),
                                                                    selected = "",
                                                                    multiple = FALSE),
                                                        checkboxInput(inputId = "add_generic_species",
                                                                      label = "Include generic species codes",
                                                                      value = TRUE),
                                                        tippy_this(elementId = "add_generic_species",
                                                                   tooltip = "Once variables are set, click the button to add generic species below",
                                                                   placement = "right",
                                                                   delay = c(50, 0)),
                                                        conditionalPanel(condition = "input.add_generic_species",
                                                                         actionButton(inputId = "add_generic_species_button",
                                                                                      label = "Add generic species codes to lookup table"),
                                                                         selectInput(inputId = "growth_habit_var",
                                                                                     label = "Growth habit variable in lookup table",
                                                                                     choices = c(""),
                                                                                     selected = "",
                                                                                     multiple = FALSE),
                                                                         selectInput(inputId = "duration_var",
                                                                                     label = "Duration variable in lookup table",
                                                                                     choices = c(""),
                                                                                     selected = "",
                                                                                     multiple = FALSE))
                                       )
                      )
               )
             ),
             conditionalPanel(condition = "input.join_species > 0",
                              fluidRow(column(width = 10,
                                              DT::DTOutput(outputId = "species_lut"))))),
    ###### Calculate Indicators #####################################
    tabPanel(title = "Calculate Indicators",
             sidebarLayout(sidebarPanel(actionLink(inputId = "indicator_help",
                                                   label = "What do these options mean?"),
                                        conditionalPanel(condition = "input.data_type == 'lpi'",
                                                         selectInput(inputId = "lpi_hit",
                                                                     label = "Cover calculation type",
                                                                     choices = c("Any hit" = "any",
                                                                                 "First hit" = "first",
                                                                                 "Basal hit" = "basal",
                                                                                 "Species" = "species",
                                                                                 "Bare soil" = "bare_ground",
                                                                                 "Litter" = "litter",
                                                                                 "Between-plant" = "between_plant",
                                                                                 "Total foliar" = "total_foliar",
                                                                                 "Non-plant surface" = "nonplant_ground")),
                                                         # Grouping variables are only options for first, any, and basal
                                                         conditionalPanel(condition = "input.lpi_hit == 'any' | input.lpi_hit == 'first' | input.lpi_hit == 'basal'",
                                                                          # tippy can't work with selectInput(multiple = TRUE)
                                                                          # So we can wrap it in a div() and tippy that
                                                                          div(id = "lpi_grouping_vars_wrapper",
                                                                              selectInput(inputId = "lpi_grouping_vars",
                                                                                          label = "Grouping variables",
                                                                                          multiple = TRUE,
                                                                                          choices = c(""))),
                                                                          tippy_this(elementId = "lpi_grouping_vars_wrapper",
                                                                                     tooltip = "E.g. code or growth habit and duration",
                                                                                     placement = "right",
                                                                                     delay = c(50, 0))
                                                         ),
                                                         selectInput(inputId = "lpi_unit",
                                                                     label = "Summary unit",
                                                                     choices = c("Plot" = "plot",
                                                                                 "Line" = "line"),
                                                                     selected = "plot"),
                                                         selectInput(inputId = "lpi_output_format",
                                                                     label = "Output format",
                                                                     choices = c("Wide" = "wide",
                                                                                 "Long" = "long"))
                                        ),
                                        conditionalPanel(condition = "input.data_type == 'gap'",
                                                         textInput(inputId = "gap_breaks",
                                                                   label = "Gap size breakpoints",
                                                                   value = "25, 51, 101, 201"),
                                                         selectInput(inputId = "gap_type",
                                                                     label = "Gap type",
                                                                     choices = c("Canopy" = "canopy",
                                                                                 "Basal" = "basal",
                                                                                 "Perennial canopy" = "perennial canopy"),
                                                                     selected = "canopy"),
                                                         checkboxGroupInput(inputId = "gap_indicator_types",
                                                                            label = "Gap indicator types to calculate",
                                                                            choices = c("Percent of line(s)" = "percent",
                                                                                        "Number of gaps" = "n",
                                                                                        "Length of gaps" = "length"),
                                                                            selected = c("percent")),
                                                         selectInput(inputId = "gap_unit",
                                                                     label = "Summary unit",
                                                                     choices = c("Plot" = "plot",
                                                                                 "Line" = "line"),
                                                                     selected = "plot"),
                                                         selectInput(inputId = "gap_output_format",
                                                                     label = "Output format",
                                                                     choices = c("Wide" = "wide",
                                                                                 "Long" = "long"))
                                        ),
                                        conditionalPanel(condition = "input.data_type == 'height'",
                                                         checkboxInput(inputId = "height_omit_zero",
                                                                       label = "Omit heights of 0 from calculation"),
                                                         # tippy can't work with selectInput(multiple = TRUE)
                                                         # So we can wrap it in a div() and tippy that
                                                         div(id = "height_grouping_vars_wrapper",
                                                             selectInput(inputId = "height_grouping_vars",
                                                                         label = "Grouping variables",
                                                                         multiple = TRUE,
                                                                         choices = c(""))),
                                                         tippy_this(elementId = "height_grouping_vars_wrapper",
                                                                    tooltip = "E.g. species or growth habit and duration",
                                                                    placement = "right",
                                                                    delay = c(50, 0)),
                                                         selectInput(inputId = "height_unit",
                                                                     label = "Summary unit",
                                                                     choices = c("Plot" = "plot",
                                                                                 "Line" = "line"),
                                                                     selected = "plot"),
                                                         selectInput(inputId = "height_output_format",
                                                                     label = "Output format",
                                                                     choices = c("Wide" = "wide",
                                                                                 "Long" = "long"))
                                        ),
                                        conditionalPanel(condition = "input.data_type == 'soilstability'",
                                                         checkboxGroupInput(inputId = "soil_covergroups",
                                                                            label = "Vegetative cover grouping",
                                                                            choices = c("All cover types" = "all",
                                                                                        "Perennial cover" = "covered",
                                                                                        "No cover" = "uncovered",
                                                                                        "By cover type" = "by_type")),
                                                         selectInput(inputId = "soil_output_format",
                                                                     label = "Output format",
                                                                     choices = c("Wide" = "wide",
                                                                                 "Long" = "long"))
                                        ),
                                        selectInput(inputId = "additional_output_vars",
                                                    label = "Additional metadata variables",
                                                    choices = c(""),
                                                    selected = "",
                                                    multiple = TRUE),
                                        hr(),
                                        actionButton(inputId = "calculate_button",
                                                     label = "Calculate!")),
             mainPanel(fluidRow(column(width = 10,
                                       uiOutput("download_button_ui"))),
                       fluidRow(column(width = 10,
                                       textOutput(outputId = "metadata_text"))),
                       fluidRow(column(width = 10,
                                       DT::DTOutput(outputId = "results_table")))))),
    ###### Help #####################################
    tabPanel(title = "Help",
             HTML("For further help or to report a bug, please contact <a href='mailto:nelson.stauffer@usda.gov' target='_blank'>Nelson Stauffer</>."),
             includeHTML("help.html")),
  )
)
#### Server ######################
server <- function(input, output, session) {
  ##### Intialization #####
  # Allow for wonking big files
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  # This is dangerous, but I'm doing it anyway so that polygons work consistently
  sf::sf_use_s2(FALSE)
  
  # Our workspace list for storing stuff
  workspace <- reactiveValues(temp_directory = tempdir(),
                              original_directory = getwd(),
                              mapping_header_sf = NULL,
                              mapping_polygons = NULL,
                              header_sf = NULL,
                              main_map = NULL,
                              drawn_coordinates = NULL,
                              drawn_polygon_sf = NULL,
                              metadata_lut = NULL,
                              polygons = NULL,
                              default_species_filename = "usda_plants_characteristics_lookup_20210830.csv",
                              data_fresh = TRUE,
                              data = NULL,
                              raw_data = NULL,
                              headers = NULL,
                              required_vars = list("lpi" = c("PrimaryKey",
                                                             "LineKey",
                                                             "code",
                                                             "PointNbr",
                                                             "layer"),
                                                   "gap" = c("PrimaryKey",
                                                             "LineLengthAmount",
                                                             "Measure",
                                                             "RecType",
                                                             "LineKey",
                                                             "Gap"),
                                                   "height" = c("PrimaryKey",
                                                                "LineKey",
                                                                "Height",
                                                                "Species"),
                                                   "soilstability" = c("PrimaryKey",
                                                                       "Rating",
                                                                       "Veg")))
  
  ##### Conditional UI elements #####
  ###### Sidebar ######
  # Query method for when grabbing data from the LDC
  output$query_method_ui <- renderUI(expr = if (req(input$data_source) == "ldc") {
    message("data_source is 'ldc'. Rendering query_method UI element.")
    selectInput(inputId = "query_method",
                label = "Query method",
                choices = c("Spatial" = "spatial",
                            "By ecological site" = "EcologicalSiteID",
                            "By PrimaryKey" = "PrimaryKey",
                            "By ProjectKey" = "ProjectKey"),
                selected = "spatial")
  })
  output$query_method_info_ui <- renderUI(expr = if (req(input$data_source) == "ldc") {
    message("data_source is 'ldc'. Rendering query_method_info UI element.")
    actionButton(inputId = "query_method_info",
                 label = "",
                 icon = icon("info"))
  })
  
  output$polygon_source_ui <- renderUI(expr = if (req(input$query_method) == "spatial" & req(input$data_source) == "ldc") {
    radioButtons(inputId = "polygon_source",
                 label = "Polygon source",
                 choices = c("Uploaded" = "upload",
                             "Drawn" = "draw"),
                 inline = TRUE)
  })
  
  output$polygon_draw_prompt <- renderUI(expr = if(req(input$polygon_source) == "draw") {
    HTML(text = paste0(img(src = "polygon_tool_icons.png",
                           height = "60px",
                           display = "inline",
                           align = "left",
                           hspace = "5px",
                           vspace = "5px"),
                       "Please use the buttons found on the left side of the map to draw your polygon boundary."))
  })
  
  # Add a fetch button when grabbing data from the LDC and the query criteria
  # are available
  # Apparently since the tool will never have input$keys and input$polygons_layer
  # at the same time, I can't capture them both in a single conditional, but I
  # can do it in two separate ones rendering an identical element because I know
  # they'll never come into conflict
  output$fetch_ui1 <- renderUI(expr = if (req(input$query_method) %in% c("EcologicalSiteID", "PrimaryKey", "ProjectKey") & req(input$keys) != "") {
    actionButton(inputId = "fetch_data",
                 label = "Fetch data")
  })
  output$fetch_ui2 <- renderUI(expr = if (req(input$query_method) == "spatial" & (req(input$polygon_source) == "upload" & req(input$polygons_layer) != "")) {
    actionButton(inputId = "fetch_data",
                 label = "Fetch data")
  })
  output$fetch_ui3 <- renderUI(expr = if (req(input$query_method) == "spatial" & (req(input$polygon_source) == "draw" & !is.null(req(workspace$drawn_polygon_sf)))) {
    actionButton(inputId = "fetch_data",
                 label = "Fetch data")
  })
  
  # Keys when grabbing data from the LDC by key values
  output$keys_input_ui <- renderUI(expr = if (req(input$query_method) %in% c("EcologicalSiteID", "PrimaryKey", "ProjectKey")) {
    message("query_method is in c('EcologicalSiteID', 'PrimaryKey', 'ProjectKey'). Rendering keys UI element.")
    # Use different placeholders for different key types!
    if (input$query_method == "EcologicalSiteID") {
      textInput(inputId = "keys",
                label = "Search key values",
                value = "",
                placeholder = "R042XB012NM")
    } else if (input$query_method == "PrimaryKey") {
      textInput(inputId = "keys",
                label = "Search key values",
                value = "")
    } else {
      textInput(inputId = "keys",
                label = "Search key values",
                value = "")
    }
  })
  
  output$keys_input_info_ui <- renderUI(expr = if (req(input$query_method) %in% c("EcologicalSiteID", "PrimaryKey", "ProjectKey")) {
    message("data_source is 'ldc'. Rendering keys_input_info UI element.")
    actionButton(inputId = "keys_input_info",
                 label = "",
                 icon = icon("info"))
  })
  
  # Uploading spatial data
  output$spatial_input_ui <- renderUI(expr = if (req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("query_method is 'spatial'. Rendering spatial_input UI element.")
    fileInput(inputId = "polygons",
              label = "Polygons ZIP file",
              multiple = FALSE,
              accept = ".zip")
  })
  output$spatial_input_info_ui <- renderUI(expr = if (req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("query_method is 'spatial'. Rendering spatial_input_info UI element.")
    actionButton(inputId = "spatial_input_info",
                 label = "",
                 icon = icon("info"))
  })
  # Only allow polygon selection if there's an uploaded polygon
  output$select_polygon_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("There are polygons available to select from. Rendering polygons_layer UI element.")
    selectInput(inputId = "polygons_layer",
                label = "Polygons name",
                choices = c(""),
                selected = "")
  })
  output$select_polygon_info_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("There are polygons available to select from. Rendering polygons_layer UI element.")
    actionButton(inputId = "select_polygons_info",
                 label = "",
                 icon = icon("info"))
  })
  # Only allow repair if there's an uploaded polygon
  output$repair_polygons_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc" & req(input$polygon_source) == "upload") {
    message("There are polygons selected. Rendering repair_polygons UI element")
    checkboxInput(inputId = "repair_polygons",
                  label = "Repair polygons",
                  value = FALSE)
  })
  output$repair_polygons_info_ui <- renderUI(expr = if (!is.null(req(input$polygons)) & req(input$query_method) == "spatial" & req(input$data_source) == "ldc") {
    message("There are polygons selected. Rendering repair_polygons_info UI element")
    actionButton(inputId = "repair_polygons_info",
                 label = "",
                 icon = icon("info"))
  })
  
  ###### Configure Data tab ######
  
  ###### Calculate Indicators tab ######
  
  ###### Results tab ######
  # The handling for the download button is in the chunk that creates the
  # the download file
  
  #### Help buttons #############################################################
  observeEvent(eventExpr = input$data_source_info,
               handlerExpr = {
                 message("Displaying info about data sources")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "If you want to retrieve data from the Landscape Data Commons to calculate indicators from, you can use this tool to search for and fetch the relevant data.",
                                            br(),
                                            br(),
                                            "If you already have tabular data as a CSV, you can upload that file.",
                                            
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$query_method_info,
               handlerExpr = {
                 message("Displaying info about LDC query methods")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "There are multiple ways to retrieve data from the Landscape Data Commons.",
                                            br(),
                                            br(),
                                            "You can retrieve all data that falls within a polygon feature class.",
                                            br(),
                                            br(),
                                            "You can retrieve all data associated with one or more ecological site IDs. You may find these through the Ecosystem Dynamics Interpretive Tool.",
                                            br(),
                                            br(),
                                            "You can retrieve all data associated with one or more PrimaryKeys (identifiers unique to each visit to each sampling location).",
                                            br(),
                                            br(),
                                            "You can retrieve all data associated with one or more ProjectKeys (identifiers unique to sampling efforts).",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$keys_input_info,
               handlerExpr = {
                 message("Displaying info about keys")
                 switch(input$query_method,
                        "EcologicalSiteID" = {
                          showModal(ui = modalDialog(size = "s",
                                                     easyClose = TRUE,
                                                     "Keys are values associated with data that can be used to filter and select data. You can query using one or more keys separated by commas.",
                                                     br(),
                                                     br(),
                                                     "Ecological sites are areas on a landscape which are alike in terms of abiotic factors (e.g., topography, hydrology, climate) and the kind and amount of vegetation they support. More information and ecological site IDs can be found in the Ecosystem Dynamics Interpretive Tool.",
                                                     footer = tagList(modalButton("Close"))))
                        },
                        "PrimaryKey" = {
                          showModal(ui = modalDialog(size = "s",
                                                     easyClose = TRUE,
                                                     "Keys are values associated with data that can be used to filter and select data. You can query using one or more keys separated by commas.",
                                                     br(),
                                                     br(),
                                                     "Each data collection event (i.e., visit) at a sampling location has a unique identifier stored as a value called a PrimaryKey. If you know the PrimaryKey values for the data you want, you can use them to retrieve only those data.",
                                                     footer = tagList(modalButton("Close"))))
                        },
                        "ProjectKey" = {
                          showModal(ui = modalDialog(size = "s",
                                                     easyClose = TRUE,
                                                     "Keys are values associated with data that can be used to filter and select data. You can query using one or more keys separated by commas.",
                                                     br(),
                                                     br(),
                                                     "All data in the Landscape Data Commons is associated with a project. If you know the ProjectKey values associated with the data you want, you can use them to retrieve only those data.",
                                                     footer = tagList(modalButton("Close"))))
                        })
               })
  
  observeEvent(eventExpr = input$spatial_input_info,
               handlerExpr = {
                 message("Displaying info about spatial inputs")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "You can retrieve all data from the Landscape Data Commons which fall within a polygon feature class, provided either as a shapefile or a feature class in a geodatabase.",
                                            br(),
                                            br(),
                                            "The uploaded file must be a ZIP file containing either all the files making up a polygon shapefile (e.g., polygons.shp, polygons.shx, polygons.dbf, and polygons.prj) or a geodatabase containing at least one polygon feature class.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$select_polygons_info,
               handlerExpr = {
                 message("Displaying info about selecting the polygons")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "If your uploaded polygons include more than one feature class (e.g., two shapefiles or a geodatabase with multiple feature classes), then you must select which to use to retrieve data.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  observeEvent(eventExpr = input$repair_polygons_info,
               handlerExpr = {
                 message("Displaying info about repairing polygons")
                 showModal(ui = modalDialog(size = "s",
                                            easyClose = TRUE,
                                            "Polygons which appear fine in software suites like Arc may still have underlying geometry issues. In the case that your polygons have issues like ring self-intersections, you may use the 'Repair polygons' function to attempt to correct them.",
                                            footer = tagList(modalButton("Close")))
                 )
               })
  
  ##### Directing to help #####
  observeEvent(eventExpr = input$indicator_help,
               handlerExpr = {
                 message("Help button pressed. Switching tabs")
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Help")
               })
  observeEvent(eventExpr = input$data_help,
               handlerExpr = {
                 message("Help button pressed. Switching tabs")
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Help")
               })
  observeEvent(eventExpr = input$help_link,
               handlerExpr = {
                 message("Help button pressed. Switching tabs")
                 updateTa
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Help")
               })
  
  ##### Polygon upload handling #####
  # When input$polygons updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$polygons,
               handlerExpr = {
                 message("Polygons file uploaded")
                 # Because it looks like I can't enforce filetype in the upload
                 # selection dialogue, check it here
                 # I'm assuming that a file extension can be 1-5 characters long
                 # although the longest I've seen is 4, I think
                 polygon_upload_extension <- toupper(stringr::str_extract(string = input$polygons$datapath,
                                                                          pattern = "(?<=\\.).{1,5}$"))
                 polygons_are_zip <- polygon_upload_extension == "ZIP"
                 
                 if (!polygons_are_zip) {
                   showNotification(ui = "Polygons must be uploaded as a zipped shapefile or zipped geodatabase.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "polygons_zip_error",
                                    type = "error")
                 } else {
                   message("Attempting to unzip file")
                   # Unzip with an OS-specific system call
                   # Setting the working directory
                   setwd(dirname(input$polygons$datapath))
                   # Passing this to the OS
                   system(sprintf("cd %s", dirname(input$polygons$datapath)))
                   # Just checking for debugging
                   message(getwd())
                   # The unzipping argument to pass to the OS
                   system(sprintf("unzip -u %s", input$polygons$datapath))
                   # Set the working directory back
                   setwd(workspace$original_directory)
                   
                   message("File unzipped")
                   # Get the shapefile name
                   extracted_files <- list.files(dirname(input$polygons$datapath),
                                                 full.names = TRUE,
                                                 recursive = TRUE)
                   
                   # Look for extracted shapefiles
                   shp_indices <- grepl(extracted_files,
                                        pattern = "\\.shp$",
                                        ignore.case = TRUE)
                   message(paste0("Found ",
                                  sum(shp_indices),
                                  " shapefiles"))
                   upload_has_shp <- any(shp_indices)
                   
                   # Look for extracted geodatabases
                   # Now because this is recursive, we're turning up the files
                   # inside the GDBs, so we need to get just the GDB path
                   gdb_indices <- grepl(extracted_files,
                                        pattern = "\\.gdb",
                                        ignore.case = TRUE)
                   gdb_paths <- unique(stringr::str_extract(string = extracted_files[gdb_indices],
                                                            pattern = ".*(?=\\.gdb/)"))
                   gdb_paths <- paste0(gdb_paths,
                                       ".gdb")
                   gdb_paths <- gdb_paths[!(gdb_paths %in% c(".gdb"))]
                   
                   message(paste0("Found ",
                                  length(gdb_paths),
                                  " geodatabases"))
                   upload_has_gdb <- length(gdb_paths) > 0
                   
                   # Prioritize geodatabases
                   if (upload_has_gdb) {
                     workspace$polygon_filetype <- "gdb"
                     message("Working from extracted geodatabase")
                     # If there's more than one geodatabase, just use the first
                     # but warn the user
                     if (length(gdb_paths) > 1) {
                       message("Multiple GDBs detected. Using 'first' one")
                       showNotification(ui = "More than one geodatabase found in ZIP file. Please upload one at a time.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "multiple_gdb_warning")
                     }
                     current_gdb_path <- gdb_paths[1]
                     
                     # So I can reference this when reading in layers later
                     workspace$gdb_filepath <- current_gdb_path
                     # Find which layers are available
                     available_polygons <- sf::st_layers(dsn = current_gdb_path)$name
                     message(paste0("Available layers in GDB are: ",
                                    paste(available_polygons,
                                          collapse = ", ")))
                     message("Updating selectInput(inputId = 'polygons_layer')")
                     # Update the selection options
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else if (upload_has_shp) {
                     workspace$polygon_filetype <- "shp"
                     message("Working with extracted shapefile(s)")
                     # Which files end in .shp?
                     available_polygons <- extracted_files[shp_indices]
                     
                     message(paste0("Before checking for all files, the available shapefiles are: ",
                                    paste(basename(available_polygons),
                                          collapse = ", ")))
                     
                     # And which of those .shp files has associated
                     # .prj, .dbf, and .shx files?
                     has_all_files <- sapply(X = available_polygons,
                                             files = extracted_files,
                                             FUN = function(X, files) {
                                               required_filetypes <- c("dbf",
                                                                       "prj",
                                                                       "shp",
                                                                       "shx")
                                               file_pattern <- gsub(x = X,
                                                                    pattern = "shp$",
                                                                    replacement = "",
                                                                    ignore.case = TRUE)
                                               all(sapply(X = required_filetypes,
                                                          files = files,
                                                          file_pattern = file_pattern,
                                                          FUN = function(X, files, file_pattern) {
                                                            paste0(file_pattern,
                                                                   X) %in% files
                                                          }))
                                             })
                     # So then which shapefiles are really valid?
                     available_polygons <- available_polygons[has_all_files]
                     message(paste0("After checking for all files, the available shapefiles are: ",
                                    paste(basename(available_polygons),
                                          collapse = ", ")))
                     
                     # Update the selection options
                     # This makes a named vector of the shp filepaths
                     # so it's easy to read them in later but the options are
                     # human readable in the GUI
                     polygon_shp_options <- available_polygons
                     shp_filenames <- gsub(x = basename(available_polygons),
                                           pattern = "\\.shp$",
                                           replacement = "",
                                           ignore.case = TRUE)
                     names(available_polygons) <- shp_filenames
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else {
                     showNotification(ui = "Uploaded file does not appear to contain either a valid shapefile or geodatabase.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error",
                                      id = "empty_upload_error")
                   }
                 }
               })
  
  #### Map time ####
  observeEvent(eventExpr = list(workspace$mapping_header_sf,
                                workspace$mapping_polygons),
               handlerExpr = {
                 message("Something changed for mapping purposes.")
                 # Initialize the map
                 map <- leaflet::leaflet()
                 
                 # Add some basic info
                 map <- leaflet::addTiles(map = map)
                 
                 # Add the polygons
                 message("Checking to see if !is.null(workspace$mapping_polygons)")
                 if (!is.null(workspace$mapping_polygons)) {
                   message("!is.null(workspace$mapping_polygons) was TRUE")
                   # Note that we have to manually remove Z dimensions with sf::st_zm()
                   # otherwise if there's a Z dimension this fails with an
                   # inscrutable error.
                   map <- leaflet::addPolygons(map = map,
                                               data = sf::st_transform(x = sf::st_zm(workspace$mapping_polygons),
                                                                       crs = "+proj=longlat +datum=WGS84"),
                                               fillColor = "coral",
                                               stroke = FALSE,
                                               fillOpacity = 0.5)
                 }
                 
                 # Add in the retrieved points
                 message("Checking to see if !is.null(workspace$mapping_header_sf)")
                 if (!is.null(workspace$mapping_header_sf)) {
                   message("!is.null(workspace$mapping_header_sf) was TRUE")
                   map <- leaflet::addCircleMarkers(map = map,
                                                    data = sf::st_transform(x = workspace$mapping_header_sf,
                                                                            crs = "+proj=longlat +datum=WGS84"),
                                                    stroke = TRUE,
                                                    opacity = 0.9,
                                                    color = "white",
                                                    weight = 1,
                                                    fillColor = "gray20",
                                                    fillOpacity = 1,
                                                    radius = 3)
                 }
                 
                 if (is.null(workspace$mapping_polygons) & is.null(workspace$mapping_header_sf)) {
                   # Set the framing
                   map <- setView(map = map,
                                  lng = -119,
                                  lat = 38.7,
                                  zoom = 4.25)
                   map <- setMaxBounds(map = map,
                                       lng1 = -125.5,
                                       lat1 = 25,
                                       lng2 = -66,
                                       lat2 = 49.5)
                 }
                 
                 # Add in the drawing controls
                 map_drawing <- addDrawToolbar(map = map,
                                               targetGroup = "draw",
                                               position = 'topleft',
                                               polylineOptions = FALSE,
                                               circleOptions = FALSE,
                                               markerOptions = FALSE,
                                               circleMarkerOptions = FALSE,
                                               singleFeature = TRUE)
                 
                 message("Rendering map")
                 output$drawing_map <- leaflet::renderLeaflet(map_drawing)
                 output$main_map <- leaflet::renderLeaflet(map)
                 message("Map rendered")
               })
  
  output$main_map_ui <- renderUI({
    if (req(input$query_method) != "spatial" & (!is.null(workspace$mapping_header_sf) | !is.null(workspace$mapping_polygons))) {
      message("Attempting to render main_map_ui")
      leafletOutput(outputId = "main_map",
                    height = "50vh")
    }
  })
  output$drawing_map_ui <- renderUI({
    if (req(input$query_method) == "spatial") {
      message("Attempting to render drawing_map_ui")
      leafletOutput(outputId = "drawing_map",
                    height = "50vh")
    }
  })
  
  ###### Making a polygon sf object from the polygon drawn on the map ##########
  # This is adapted from the RAP Production Explorer
  # Character string of coordinates from drawn features on Leaflet map
  observeEvent(input$drawing_map_draw_new_feature,{
    message("There's a new polygon drawn on the map! Getting coordinates")
    # This builds a neat little [x, y] string for each vertex
    # Frankly, this is a little silly to do considering we're going to split them
    # into a vector, but I can't be bothered to refactor beyond changing it to
    # a sapply() because it works as-is and isn't hurting anyone
    coords <- sapply(X = input$drawing_map_draw_new_feature$geometry$coordinates[1][[1]],
                     FUN = function(X) {
                       paste0("[", X[1], ", ", X[2], "]")
                     })
    coord_string <- paste0(coords,
                           collapse = ", ")
    workspace$drawn_coordinates <- coord_string
    message("Coordinates saved to workspace$drawn_coordinates")
  })
  # Convert the coordinates of the vertices on the map into a polygon sf object!
  observeEvent(eventExpr = workspace$drawn_coordinates,
               handlerExpr = {
                 if (!is.null(workspace$drawn_coordinates)) {
                   coords <- workspace$drawn_coordinates
                   message("workspace$drawn_coordinates has updated! Attempting to create a polygon sf object using the coordinates as vertices")
                   print(coords)
                   message("Cleaning coordinates and creating vector")
                   coords_clean <- strsplit(x = gsub(x = coords,
                                                     pattern = "\\[|\\]",
                                                     replacement = ""),
                                            split = ',') 
                   print(coords_clean)
                   message("Converting vector to numeric")
                   coords_numeric <- as.numeric(coords_clean[[1]])
                   print(coords_numeric)
                   n_vertices <- length(coords_clean[[1]])/2
                   message("Creating a matrix from the coordinates")
                   coords_matrix <- matrix(coords_numeric, nrow = n_vertices, byrow = TRUE)
                   coords_list <- list(coords_matrix)
                   message("Making a polygon matrix thingy")
                   print(coords_list)
                   polygon <- sf::st_polygon(coords_list)
                   message("Making an sf object")
                   polygon <- st_sfc(polygon,
                                     crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   workspace$drawn_polygon_sf <- sf::st_sf(polygon)
                   message("Polygon saved to workspace$drawn_polygon_sf")
                 }
               })
  
  ###### Getting freshly uploaded polygons set to map #####
  observeEvent(eventExpr = {input$polygons_layer},
               handlerExpr = {
                 message("input$polygons_layer has updated")
                 if (input$polygons_layer != "") {
                   message("Reading in polygons")
                   if (workspace$polygon_filetype == "gdb") {
                     workspace$mapping_polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                                               layer = input$polygons_layer)
                   } else if (workspace$polygon_filetype == "shp") {
                     workspace$mapping_polygons <- sf::st_read(dsn = input$polygons_layer)
                   }
                   message("Making sure the polygons are in NAD83")
                   workspace$mapping_polygons <- sf::st_transform(workspace$mapping_polygons,
                                                                  crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   
                   # If they've asked us to "repair" polygons, buffer by 0
                   if (input$repair_polygons) {
                     message("Attempting to repair polygons by buffering by 0")
                     workspace$mapping_polygons <- sf::st_buffer(x = workspace$mapping_polygons,
                                                                 dist = 0)
                   }
                 }
               })
  
  ##### CSV upload handling #####
  # When input$raw_data updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$raw_data,
               handlerExpr = {
                 message("input$raw_data has updated")
                 # Because it looks like I can't enforce filetype in the upload
                 # selection dialogue, check it here
                 # I'm assuming that a file extension can be 1-5 characters long
                 # although the longest I've seen is 4, I think
                 data_upload_extension <- toupper(stringr::str_extract(string = input$raw_data$datapath,
                                                                       pattern = "(?<=\\.).{1,5}$"))
                 data_are_csv <- data_upload_extension == "CSV"
                 
                 if (data_are_csv) {
                   message("Reading in raw data from uploaded CSV")
                   workspace[["raw_data"]] <- read.csv(input$raw_data$datapath,
                                                       stringsAsFactors = FALSE)
                   
                   # Set this variable so we can handle the data appropriately based on source
                   # Since these are uploaded data, we're not looking for header info
                   message("Data source set to upload")
                   workspace$current_data_source <- "upload"
                 } else {
                   message("Uploaded data aren't a .CSV; doing nothing with them")
                   data_csv_error_message <- paste0("You have uploaded a ",
                                                    data_upload_extension,
                                                    " file. Please upload a CSV instead.")
                   showNotification(ui = data_csv_error_message,
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "data_csv_error",
                                    type = "error")
                 }
               })
  
  # When input$species_data updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$species_data,
               handlerExpr = {
                 message("input$species_data has updated")
                 # Because it looks like I can't enforce filetype in the upload
                 # selection dialogue, check it here
                 species_upload_extension <- toupper(stringr::str_extract(string = input$species_data$datapath,
                                                                          pattern = "(?<=\\.).{1,5}$"))
                 species_are_csv <- species_upload_extension == "CSV"
                 
                 if (species_are_csv) {
                   message("Reading in species data from uploaded CSV")
                   workspace[["species_data"]] <- read.csv(input$species_data$datapath,
                                                           stringsAsFactors = FALSE)
                   
                   # Set this variable so we can handle the data appropriately based on source
                   message("Species source set to upload")
                   workspace$current_species_source <- "upload"
                 } else {
                   message("The uploaded species data are not a CSV. Doing nothing")
                   species_csv_error_message <- paste0("You have uploaded a ",
                                                       species_upload_extension,
                                                       " file. Please upload a CSV instead.")
                   showNotification(ui = species_csv_error_message,
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "species_csv_error",
                                    type = "error")
                 }
                 
               })
  
  # When input$species_source is set to the default, handle that
  observeEvent(eventExpr = input$species_source,
               handlerExpr = {
                 message("input$species_source has changed")
                 if (input$species_source == "default") {
                   message("Species source set to default. Reading in default species data.")
                   defaults_species_filepath <- paste0(workspace$original_directory,
                                                       "/",
                                                       workspace$default_species_filename)
                   workspace[["species_data"]] <- read.csv(defaults_species_filepath,
                                                           stringsAsFactors = FALSE)
                   # TODO
                   # Set workspace$species_data to the default list (which doesn't exist yet)
                   # Set this variable so we can handle the data appropriately based on source
                   workspace$current_species_source <- "default"
                 } else {
                   message("Species source is not default. Doing nothing")
                 }
               })
  
  ##### When workspace$species_data updates #####
  observeEvent(eventExpr = workspace$species_data,
               handlerExpr = {
                 message("workspace$species_data has updated!")
                 current_species_data_vars <- names(workspace$species_data)
                 
                 message("Attempting to update the selected variables in the species data")
                 # For the joining variable
                 if ("code" %in% current_species_data_vars) {
                   message("Found 'code' in the species data. Setting that as the species_joining_var")
                   selection <- "code"
                 } else {
                   message("Setting species_joining_var to ''")
                   selection <- ""
                 }
                 updateSelectInput(session = session,
                                   inputId = "species_joining_var",
                                   choices = c("",
                                               current_species_data_vars),
                                   selected = selection)
                 
                 # For the growth habit variable
                 # We'll guess at the two most common options before giving up
                 if ("GrowthHabitSub" %in% current_species_data_vars) {
                   message("Found 'GrowthHabitSub' in the species data. Setting that as growth_habit_var")
                   selection <- "GrowthHabitSub"
                 } else if ("growth_habit" %in% current_species_data_vars) {
                   message("Found 'growth_habit' in the species data. Setting that as growth_habit_var")
                   selection <- "growth_habit"
                 } else {
                   message("Setting growth_habit_var to ''")
                   selection <- ""
                 }
                 updateSelectInput(session = session,
                                   inputId = "growth_habit_var",
                                   choices = c("",
                                               current_species_data_vars),
                                   selected = selection)
                 
                 # For the duration variable
                 if ("Duration" %in% current_species_data_vars) {
                   message("Found 'Duration' in the species data. Setting that as duration_var")
                   selection <- "Duration"
                 } else if ("duration" %in% current_species_data_vars) {
                   message("Found 'duration' in the species data. Setting that as duration_var")
                   selection <- "duration"
                 } else {
                   message("Setting duration_Var to ''")
                   selection <- ""
                 }
                 updateSelectInput(session = session,
                                   inputId = "duration_var",
                                   choices = c("",
                                               current_species_data_vars),
                                   selected = selection)
                 
               })
  
  ##### Fetching data from the LDC #####
  observeEvent(eventExpr = input$fetch_data,
               handlerExpr = {
                 message("Fetch data button pressed!")
                 showNotification(ui = HTML("Fetching data!"),
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "downloading",
                                  type = "message")
                 
                 # Set this variable so we can handle the data appropriately based on source
                 # Since there are from the LDC, we'll also be looking for header info
                 workspace$current_data_source <- "ldc"
                 message("Data source set to LDC")
                 
                 if (input$query_method != "spatial") {
                   message("Nullifying workspace$mapping_polygons for mapping reasons")
                   workspace$mapping_polygons <- NULL
                   message("Querying by keys")
                   # Only do anything if there's at least one key
                   if (input$keys != "") {
                     # Handle multiple requested ecosites at once!
                     current_key_vector <- stringr::str_split(string = input$keys,
                                                              pattern = ",",
                                                              simplify = TRUE)
                     
                     # This will make it easy to check to see if any of these values
                     # weren't associated with data
                     current_key_vector <- trimws(current_key_vector)
                     
                     # fetch_ldc() can take a vector (slow, retrieves one at a time)
                     # or a string of values separated by commas (fast, retrieves all at once)
                     current_key_string <- paste(current_key_vector,
                                                 collapse = ",")
                     
                     
                     # The API queryable tables don't include ecosite, so we grab
                     # the header table and get primary keys from that
                     if (input$query_method == "EcologicalSiteID") {
                       message("key_type is EcologicalSiteID")
                       message("Retrieving headers")
                       current_headers <- tryCatch(fetch_ldc(keys = current_key_string,
                                                             key_type = input$query_method,
                                                             data_type = "header",
                                                             verbose = TRUE),
                                                   error = function(error){
                                                     gsub(x = error,
                                                          pattern = "^Error.+[ ]:[ ]",
                                                          replacement = "")
                                                   })
                       message(paste0("class(current_headers) is ",
                                      paste(class(current_headers),
                                            collapse = ", ")))
                       if ("character" %in% class(current_headers)) {
                         results <- NULL
                       } else {
                         current_headers_sf <- sf::st_as_sf(x = current_headers,
                                                            coords = c("Longitude_NAD83",
                                                                       "Latitude_NAD83"),
                                                            crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                         
                         # This'll be useful so I can make a map
                         workspace$mapping_header_sf <- current_headers_sf
                         current_primary_keys <- current_headers$PrimaryKey
                         
                         # current_key_chunk_count <- ceiling(length(current_primary_keys) / 100)
                         # 
                         # current_primary_keys <- sapply(X = 1:current_key_chunk_count,
                         #                                keys_vector = current_primary_keys,
                         #                                key_chunk_size = 100,
                         #                                key_count = length(current_primary_keys),
                         #                                FUN = function(X, keys_vector, key_chunk_size, key_count) {
                         #                                  min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                         #                                  max_index <- min(c(key_count, X * key_chunk_size))
                         #                                  indices <- min_index:max_index
                         #                                  paste(keys_vector[indices],
                         #                                        collapse = ",")
                         #                                })
                         
                         message("Retrieving data using PrimaryKey values from headers")
                         results <- tryCatch(fetch_ldc(keys = current_primary_keys,
                                                       key_type = "PrimaryKey",
                                                       data_type = input$data_type,
                                                       verbose = TRUE),
                                             error = function(error){
                                               gsub(x = error,
                                                    pattern = "^Error.+[ ]:[ ]",
                                                    replacement = "")
                                             })
                       }
                     } else {
                       message("key_type is not EcologicalSiteID")
                       message("Retrieving data using provided keys")
                       current_key_chunk_count <- ceiling(length(current_key_vector) / 100)
                       
                       current_keys_chunks <- sapply(X = 1:current_key_chunk_count,
                                                     keys_vector = current_key_vector,
                                                     key_chunk_size = 100,
                                                     key_count = length(current_key_vector),
                                                     FUN = function(X, keys_vector, key_chunk_size, key_count) {
                                                       min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                                                       max_index <- min(c(key_count, X * key_chunk_size))
                                                       indices <- min_index:max_index
                                                       paste(keys_vector[indices],
                                                             collapse = ",")
                                                     })
                       
                       results <- tryCatch(fetch_ldc(keys = current_keys_chunks,
                                                     key_type = input$query_method,
                                                     data_type = input$data_type,
                                                     verbose = TRUE),
                                           error = function(error){
                                             gsub(x = error,
                                                  pattern = "^Error.+[ ]:[ ]",
                                                  replacement = "")
                                           })
                     }
                     
                     
                     # So we can tell the user later which actually got queried
                     if (is.null(results)) {
                       message("No data from LDC!")
                       workspace$missing_keys <- current_key_vector
                       showNotification(ui = "No data were found associated with your keys.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "no_data_returned_warning")
                     } else if ("character" %in% class(results)) {
                       # If results is actually an error message, display it
                       showNotification(ui = results,
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "error",
                                        id = "api_error")
                       workspace$missing_keys <- NULL
                     } else {
                       # Let's get map info!
                       message("Getting current headers for mapping")
                       current_primarykeys <- unique(results$PrimaryKey)
                       current_primarykey_chunk_count <- ceiling(length(current_key_vector) / 100)
                       
                       current_primarykeys_chunks <- sapply(X = 1:current_primarykey_chunk_count,
                                                            keys_vector = current_primarykeys,
                                                            key_chunk_size = 100,
                                                            key_count = length(current_primarykeys),
                                                            FUN = function(X, keys_vector, key_chunk_size, key_count) {
                                                              min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                                                              max_index <- min(c(key_count, X * key_chunk_size))
                                                              indices <- min_index:max_index
                                                              paste(keys_vector[indices],
                                                                    collapse = ",")
                                                            })
                       message("Actually fetching current headers based on PrimaryKeys")
                       current_headers <- tryCatch(fetch_ldc(keys = current_primarykeys_chunks,
                                                             key_type = "PrimaryKey",
                                                             data_type = "header",
                                                             verbose = TRUE),
                                                   error = function(error){
                                                     gsub(x = error,
                                                          pattern = "^Error.+[ ]:[ ]",
                                                          replacement = "")
                                                   })
                       
                       message(paste0("class(current_headers) is: ",
                                      paste(class(current_headers),
                                            collapse = ", ")))
                       if ("character" %in% class(current_headers)) {
                         showNotification(ui = paste0("Encountered the following API error retrieving header info for mapping: ",
                                                      current_headers),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          type = "error",
                                          id = "api_headers_error")
                         workspace$mapping_header_sf <- NULL
                       } else if ("data.frame" %in% class(current_headers)) {
                         message("Converting header info to sf object")
                         current_headers_sf <- sf::st_as_sf(x = current_headers,
                                                            coords = c("Longitude_NAD83",
                                                                       "Latitude_NAD83"),
                                                            crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                         
                         # This'll be useful so I can make a map
                         workspace$mapping_header_sf <- current_headers_sf
                       }
                       
                       message("Determining if keys are missing.")
                       message(paste0("input$query_method is: ",
                                      paste(input$query_method,
                                            collapse = ", ")))
                       # Because ecosites were two-stage, we check in with headers
                       if (input$query_method %in% c("EcologicalSiteID")) {
                         workspace$queried_keys <- unique(current_headers[[input$query_method]])
                       } else {
                         workspace$queried_keys <- unique(results[[input$query_method]])
                       }
                       
                       workspace$missing_keys <- current_key_vector[!(current_key_vector %in% workspace$queried_keys)]
                     }
                     
                     message("Determining if workspace$missing_keys has length > 0")
                     if (length(workspace$missing_keys) > 0) {
                       message(paste0("The following key values are missing: ",
                                      paste(workspace$missing_keys,
                                            collapse = ", ")))
                       key_error <- paste0("The following keys did not have data associated with them: ",
                                           paste(workspace$missing_keys,
                                                 collapse = ", "))
                       showNotification(ui = key_error,
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "key_error")
                     } else {
                       message("No missing keys!")
                     }
                     
                     
                     # Only keep going if there are results!!!!
                     if (length(results) > 0 & "data.frame" %in% class(results)) {
                       message("Coercing variables to numeric.")
                       # Convert from character to numeric variables where possible
                       data_corrected <- lapply(X = names(results),
                                                data = results,
                                                FUN = function(X, data){
                                                  # Get the current variable values as a vector
                                                  vector <- data[[X]]
                                                  # Try to coerce into numeric
                                                  numeric_vector <- as.numeric(vector)
                                                  # If that works without introducing NAs, return the numeric vector
                                                  # Otherwise, return the original character vector
                                                  if (all(!is.na(numeric_vector))) {
                                                    return(numeric_vector)
                                                  } else {
                                                    return(vector)
                                                  }
                                                })
                       
                       # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
                       # so I'm resorting to using dplyr
                       data <- dplyr::bind_cols(data_corrected)
                       # Correct the names of the variables
                       names(data) <- names(results)
                       
                       # Put it in the workspace list
                       message("Setting data_fresh to TRUE because we just downloaded it")
                       workspace$data_fresh <- TRUE
                       workspace$raw_data <- data
                     } else {
                       workspace$raw_data <- NULL
                     }
                     
                     message("Data are from the LDC and include header info. Setting workspace$headers to NULL.")
                     workspace$headers <- NULL
                     
                   }
                 } else if (input$query_method == "spatial") {
                   message("Spatial query time!")
                   
                   if (input$polygon_source == "upload") {
                     if (input$polygons_layer == "") {
                       message("Currently expecting uploaded polygons but there are none selected.")
                       showNotification(ui = "Please upload and select polygons or drawn a polygon instead.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "no_polygons_yet_warning")
                     } else {
                       message("Reading in polygons")
                       if (workspace$polygon_filetype == "gdb") {
                         workspace$polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                                           layer = input$polygons_layer)
                       } else if (workspace$polygon_filetype == "shp") {
                         workspace$polygons <- sf::st_read(dsn = input$polygons_layer)
                       }
                       message("Making sure the polygons are in NAD83")
                       workspace$polygons <- sf::st_transform(workspace$polygons,
                                                              crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                       
                       # If they've asked us to "repair" polygons, buffer by 0
                       if (input$repair_polygons) {
                         message("Attempting to repair polygons by buffering by 0")
                         workspace$polygons <- sf::st_buffer(x = workspace$polygons,
                                                             dist = 0)
                       }
                     }
                   } else {
                     if (!is.null(workspace$drawn_polygon_sf)) {
                       message("Using drawn polygon")
                       workspace$polygons <- workspace$drawn_polygon_sf
                     }
                   }
                   message("Attempting to query spatially")
                   
                   message(paste0("Number of individual polygons in workspace$polygons is ",
                                  nrow(workspace$polygons)))
                   message("Adding unique_id variable to workspace$polygons")
                   workspace$polygons[["unique_id"]] <- 1:nrow(workspace$polygons)
                   
                   # For mapping purposes
                   message("Updating workspace$mapping_polygons")
                   workspace$mapping_polygons <- workspace$polygons
                   
                   if (is.null(workspace$headers)) {
                     message("Retrieving headers")
                     workspace$headers <- tryCatch(fetch_ldc(keys = NULL,
                                                             key_type = NULL,
                                                             data_type = "header",
                                                             verbose = TRUE),
                                                   error = function(error){
                                                     gsub(x = error,
                                                          pattern = "^Error.+[ ]:[ ]",
                                                          replacement = "")
                                                   })
                     message(paste0("class(workspace$headers) is ",
                                    paste(class(workspace$headers),
                                          collapse = ", ")))
                   }
                   
                   current_headers <- workspace$headers
                   
                   
                   # If there was an API error, display that
                   if ("character" %in% class(current_headers)) {
                     results <- NULL
                     showNotification(ui = paste0("API error retrieving headers for spatial query: ",
                                                  current_headers),
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "headers_for_sf_error",
                                      type = "error")
                   } else {
                     # If there was no error, proceed
                     message("Converting header info to sf object")
                     current_headers_sf <- sf::st_as_sf(x = current_headers,
                                                        coords = c("Longitude_NAD83",
                                                                   "Latitude_NAD83"),
                                                        crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                     
                     # This'll be useful so I can make a map, if that feature is added
                     workspace$header_sf <- current_headers_sf
                     workspace$mapping_header_sf <- current_headers_sf
                     
                     message("Performing sf_intersection()")
                     points_polygons_intersection <- tryCatch(sf::st_intersection(x = current_headers_sf[, "PrimaryKey"],
                                                                                  y = sf::st_transform(workspace$polygons[, "unique_id"],
                                                                                                       crs = sf::st_crs(current_headers_sf))),
                                                              error = function(error){"There was a geoprocessing error. Please try using the 'repair polygons' option."})
                     
                     if ("character" %in% class(points_polygons_intersection)) {
                       showNotification(ui = points_polygons_intersection,
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "error",
                                        id = "intersection_error")
                     } else {
                       current_primary_keys <- unique(points_polygons_intersection$PrimaryKey)
                       
                       if (length(current_primary_keys) < 1) {
                         message("No data were found")
                         showNotification(ui = paste0("No data were found within your polygons."),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "no_overlap",
                                          type = "warning")
                         results <- NULL
                       } else {
                         message("Primary keys found. Querying now.")
                         # current_key_chunk_count <- ceiling(length(current_primary_keys) / 100)
                         # 
                         # current_primary_keys <- sapply(X = 1:current_key_chunk_count,
                         #                                keys_vector = current_primary_keys,
                         #                                key_chunk_size = 100,
                         #                                key_count = length(current_primary_keys),
                         #                                FUN = function(X, keys_vector, key_chunk_size, key_count) {
                         #                                  min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                         #                                  max_index <- min(c(key_count, X * key_chunk_size))
                         #                                  indices <- min_index:max_index
                         #                                  paste(keys_vector[indices],
                         #                                        collapse = ",")
                         #                                })
                         
                         message("Retrieving data using PrimaryKey values from spatial intersection")
                         results <- tryCatch(fetch_ldc(keys = current_primary_keys,
                                                       key_type = "PrimaryKey",
                                                       data_type = input$data_type,
                                                       key_chunk_size = 100,
                                                       verbose = TRUE),
                                             error = function(error){
                                               gsub(x = error,
                                                    pattern = "^Error.+[ ]:[ ]",
                                                    replacement = "")
                                             })
                         message("Querying by primary key complete.")
                         message(paste0("Number of records retrieved: ",
                                        length(results)))
                       }
                       
                       # Only keep going if there are results!!!!
                       if (length(results) > 0 & "data.frame" %in% class(results)) {
                         message("Making workspace$mapping_header_sf")
                         workspace$mapping_header_sf <- current_headers_sf[current_headers_sf$PrimaryKey %in% results$PrimaryKey,]
                         
                         message("Coercing variables to numeric.")
                         # Convert from character to numeric variables where possible
                         data_corrected <- lapply(X = names(results),
                                                  data = results,
                                                  FUN = function(X, data){
                                                    # Get the current variable values as a vector
                                                    vector <- data[[X]]
                                                    # Try to coerce into numeric
                                                    numeric_vector <- as.numeric(vector)
                                                    # If that works without introducing NAs, return the numeric vector
                                                    # Otherwise, return the original character vector
                                                    if (all(!is.na(numeric_vector))) {
                                                      return(numeric_vector)
                                                    } else {
                                                      return(vector)
                                                    }
                                                  })
                         
                         # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
                         # so I'm resorting to using dplyr
                         data <- dplyr::bind_cols(data_corrected)
                         # Correct the names of the variables
                         names(data) <- names(results)
                         
                         # Put it in the workspace list
                         message("Setting data_fresh to TRUE because we just downloaded it")
                         workspace$data_fresh <- TRUE
                         workspace$raw_data <- data
                       } else if (length(results) == 0) {
                         message("No records found for those PrimaryKeys")
                         if (length(current_primary_keys) > 0) {
                           no_data_spatial_error_message <- paste0("Although sampling locations were found within your polygons, they did not have associated data of the type requested.")
                         } else {
                           no_data_spatial_error_message <- paste0("No sampling locations were found within your polygons.")
                         }
                         showNotification(ui = paste0(no_data_spatial_error_message,
                                                      results),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "no_data_spatial_error",
                                          type = "error")
                         workspace$raw_data <- NULL
                       } else {
                         showNotification(ui = paste0("API error retrieving data based on spatial query: ",
                                                      results),
                                          duration = NULL,
                                          closeButton = TRUE,
                                          id = "primarykey_spatial_error",
                                          type = "error")
                         workspace$raw_data <- NULL
                       }
                     }
                   }
                 }
                 removeNotification(id = "downloading")
               })
  
  ##### When raw_data updates #####
  observeEvent(eventExpr = workspace$raw_data,
               handlerExpr = {
                 message("workspace$raw_data has updated")
                 # If the data source is the LDC, just update workspace$data
                 # with workspace$raw_data because header info is already included
                 if (workspace$current_data_source == "ldc") {
                   message("Current data source is the LDC, using workspace$raw_data as workspace$data")
                   workspace$data <- workspace$raw_data
                   message("Setting data_fresh to TRUE")
                   workspace$data_fresh <- TRUE
                 } else if (workspace$current_data_source == "upload") {
                   message("Setting data_fresh to TRUE")
                   workspace$data_fresh <- TRUE
                   message("No headers needed. Writing workspace$raw_data to workspace$data")
                   workspace$data <- workspace$raw_data
                   # }
                 }
               })
  
  ##### When reset data button is pressed #####
  observeEvent(eventExpr = input$reset_data,
               handlerExpr = {
                 if (!is.null(workspace$raw_data)) {
                   message("Resetting data")
                   workspace$data <- workspace$raw_data
                   message("Setting data_fresh to TRUE")
                   workspace$data_fresh <- TRUE
                 }
               })
  
  ##### When workspace$data updates #####
  observeEvent(eventExpr = workspace$data,
               handlerExpr = {
                 message("workspace$data has updated")
                 # Display the data
                 # But we want to round to 2 decimal places for ease-of-reading
                 message("Prepping display data")
                 display_data <- workspace$data
                 if (is.null(display_data)) {
                   message("display_data is NULL")
                 } else {
                   message("display_Data is not NULL")
                   # Which indices are numeric variables?
                   numeric_var_indices <- which(sapply(X = display_data,
                                                       FUN = is.numeric))
                   # If any variables are numeric, round them to 2 decimal places
                   if (length(numeric_var_indices) > 0) {
                     message("Rounding numeric variables in display data")
                     # APPARENTLY dplyr::all_of() is for character vectors, not numeric vectors
                     numeric_var_names <- names(display_data)[numeric_var_indices]
                     display_data <- dplyr::mutate(.data = display_data,
                                                   dplyr::across(.cols = dplyr::all_of(numeric_var_names),
                                                                 .fns = round,
                                                                 digits = 2))
                   } else {
                     message("No numeric variables to round in display data")
                   }
                 }
                 
                 
                 message("Rendering display data")
                 output$data <- DT::renderDT(display_data,
                                             width = "100%",
                                             rownames = FALSE,
                                             options = list(pageLength = 10,
                                                            fixedHeader = TRUE,
                                                            scrollX = TRUE), 
                                             extensions = "FixedHeader")
                 
                 if (is.null(workspace$data)) {
                   # If the data aren't ready, there can't be variables selected
                   message("workspace$data is NULL.")
                   
                   # Time to nullify all the variables
                   message("Updating data_joining_var selectInput().")
                   updateSelectInput(session = session,
                                     inputId = "data_joining_var",
                                     choices = c(""),
                                     selected = "")
                   
                   message("Updating key variable selectInput()s.")
                   all_required_variables <- unique(unlist(workspace$required_vars))
                   
                   for (required_var in all_required_variables) {
                     inputid_string <- paste(tolower(required_var),
                                             "_var")
                     updateSelectInput(inputId = inputid_string,
                                       choices = c(""),
                                       selected = "")
                   }
                   
                   if (input$data_type %in% c("lpi", "height")) {
                     updateSelectInput(inputId = paste0(input$data_type,
                                                        "_grouping_vars"),
                                       choices = c(""),
                                       selected = "")
                   }
                   
                 } else {
                   message("workspace$data contains data.")
                   
                   # Update the variable options
                   current_data_vars <- names(workspace$data)
                   
                   message("Updating data_joining_var selectInput().")
                   expected_data_joining_var <- switch(input$data_type,
                                                       "lpi" = "code",
                                                       "height" = "Species",
                                                       "gap" = "",
                                                       "soilstability" = "")
                   message(paste0("expected_data_joining_var is ",
                                  expected_data_joining_var))
                   
                   if (expected_data_joining_var %in% current_data_vars) {
                     updateSelectInput(session = session,
                                       inputId = "data_joining_var",
                                       choices = c("",
                                                   current_data_vars),
                                       selected = expected_data_joining_var)
                   } else {
                     updateSelectInput(session = session,
                                       inputId = "data_joining_var",
                                       choices = c("",
                                                   current_data_vars),
                                       selected = "")
                   }
                   
                   # Time to update the variables if we can guess what they are
                   if (workspace$data_fresh) {
                     message("Data are fresh!")
                     all_required_variables <- unique(unlist(workspace$required_vars))
                     
                     for (required_var in all_required_variables) {
                       inputid_string <- paste0(tolower(required_var),
                                                "_var")
                       message(paste0("Currently updating selectInput(inputId = ",
                                      inputid_string,
                                      ")"))
                       
                       if (required_var %in% current_data_vars) {
                         selected_var <- required_var
                       } else {
                         selected_var <- ""
                       }
                       
                       message(paste0("Selected variable is '", selected_var, "'"))
                       
                       updateSelectInput(inputId = inputid_string,
                                         choices = current_data_vars,
                                         selected = selected_var)
                       message(paste0("Finished updating selectInput(inputId = ",
                                      inputid_string,
                                      ")"))
                       
                       message("Updating metadata variable options")
                       updateSelectInput(inputId = "additional_output_vars",
                                         choices = c("", current_data_vars[!(current_data_vars %in% input$primarykey_var)]),
                                         selected = c(""))
                       
                     }
                     
                     if (input$data_type %in% c("lpi", "height")) {
                       updateSelectInput(inputId = paste0(input$data_type,
                                                          "_grouping_vars"),
                                         choices = current_data_vars,
                                         selected = "")
                     }
                   }  else {
                     message("Data aren't fresh. selectInput()s will not be updated")
                   }
                   
                   message("Setting data_fresh to FALSE")
                   workspace$data_fresh <- FALSE
                   
                   # DATA SOUNDNESS CHECKS
                   # We need the variables required for the current data type
                   # Add in "LineKey" if it's called for
                   message("Checking for required variables.")
                   needs_linekey <- (input$data_type == "lpi" & input$lpi_unit == "line") | (input$data_type == "gap" & input$gap_unit == "line") | (input$data_type == "height" & input$height_unit == "line")
                   if (needs_linekey) {
                     message("Including 'LineKey' in required variables.")
                     current_required_vars <- unique(c(workspace$required_vars[[input$data_type]],
                                                       "LineKey"))
                   } else {
                     current_required_vars <- workspace$required_vars[[input$data_type]]
                   }
                   current_data_vars <- names(workspace$data)
                   missing_data_vars <- current_required_vars[!(current_required_vars %in% current_data_vars)]
                   
                   # Which required variable names are currently "" in the
                   # data configuration tab?
                   message("Getting required variable input index names")
                   current_required_vars_input_vars <- paste0(tolower(current_required_vars),
                                                              "_var")
                   message("Looking for undefined required XXX_var inputs")
                   message("current_required_vars_input_vars is: c(",
                           paste(current_required_vars_input_vars,
                                 collapse = ", "),
                           ")")
                   undefined_data_vars_indices <- unlist(sapply(X = current_required_vars_input_vars,
                                                                inputs = input,
                                                                current_available_vars = current_data_vars,
                                                                FUN = function(X, inputs, current_available_vars) {
                                                                  current_value <- eval(parse(text = paste0("inputs$",
                                                                                                            X)))
                                                                  current_value %in% c("")
                                                                }))
                   message("Determining if any required variables are missing/undefined.")
                   
                   missing_data_vars <- missing_data_vars[!(missing_data_vars %in% current_required_vars[!undefined_data_vars_indices])]
                   
                   if (length(missing_data_vars) > 0) {
                     message("Missing one or more required variables.")
                     updateCheckboxInput(session = session,
                                         inputId = "show_var_config",
                                         value = TRUE)
                     missing_vars_notification <- paste0("The following required variables are missing: ",
                                                         paste(missing_data_vars,
                                                               collapse = ", "))
                     showNotification(ui = missing_vars_notification,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error")
                   } else {
                     message("No variables were missing!")
                     # We're in the clear!
                   }
                 }
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Data")
               })
  
  ##### Updating available metadata variables when input$primarykey_var updates #####
  observeEvent(eventExpr = {input$primarykey_var},
               handlerExpr = {
                 message("input$primarykey_var updated. Working to update available metadata variables.")
                 if (!is.null(workspace$data) & input$primarykey_var %in% names(workspace$data)) {
                   message("workspace$data is not NULL; updating metadata vars")
                   # Figuring out which are valid, as in which don't have a
                   # many-to-one relationship to the unique IDs
                   current_data_vars <- names(workspace$data)
                   valid_metadata_var_indices <- sapply(X = current_data_vars,
                                                        unique_id_var = input$primarykey_var,
                                                        data = workspace$data,
                                                        FUN = function(X, unique_id_var, data) {
                                                          current_lut <- unique(data[, c(unique_id_var, X)])
                                                          !any(table(current_lut[[unique_id_var]]) > 1)
                                                        })
                   updateSelectInput(inputId = "additional_output_vars",
                                     choices = c("", current_data_vars[valid_metadata_var_indices & !(current_data_vars %in% input$primarykey_var)]),
                                     selected = c(""))
                 } else {
                   message("workspace$data is NULL; doing nothiing")
                 }
               })
  
  ##### When metadata variables change #####
  observeEvent(eventExpr = {list(input$primarykey_var,
                                 input$additional_output_vars)},
               handlerExpr = {
                 message("input$primarykey_var or input$additional_output_vars updated")
                 message("Building a metadata lookup table")
                 current_additional_output_vars <- input$additional_output_vars[!(input$additional_output_vars %in% c("", input$primarykey_var))]
                 
                 current_metadata_vars <- unique(c(input$primarykey_var,
                                                   current_additional_output_vars))
                 message(paste0("Variables for the lookup table are: ",
                                paste(current_metadata_vars,
                                      collapse = ", ")))
                 
                 if (!("" %in% current_metadata_vars) & length(current_metadata_vars) > 1 & !is.null(workspace$data)) {
                   message("current_metadata_vars doesn't contain '' and has a length greater than 1. Actually building the lookup table")
                   workspace$metadata_lut <- unique(workspace$data[, current_metadata_vars])
                   
                   # Find the many-to-one variables with a sapply here
                   many_to_one_additional_output_vars_indices <- sapply(X = current_additional_output_vars,
                                                                        unique_id_var = input$primarykey_var,
                                                                        data = workspace$data,
                                                                        FUN = function(X, unique_id_var, data) {
                                                                          lut <- unique(data[, c(unique_id_var, X)])
                                                                          any(table(lut[[unique_id_var]]) > 1)
                                                                        })
                   
                   if (any(many_to_one_additional_output_vars_indices)) {
                     many_to_one_vars <- current_additional_output_vars[many_to_one_additional_output_vars_indices]
                     showNotification(ui = paste0("Unable to include all selected additional metadata variables in the output because there is a one-to-many relationship between the variable ",
                                                  input$primarykey_var,
                                                  " and the following variables: ",
                                                  paste(many_to_one_vars,collapse = ", ")),
                                      duration = NULL,
                                      id = "one_to_many_metadata_error",
                                      closeButton = TRUE,
                                      type = "error")
                     workspace$metadata_lut <- NULL
                   }
                 } else {
                   message("Right now current_metadata_vars can't be used to make a lookup table")
                 }
               })
  
  
  ##### When adding generic/unknown species #####
  observeEvent(eventExpr = input$add_generic_species_button,
               handlerExpr = {
                 message("Generic species button was pressed!")
                 if (length(workspace$raw_data) < 1) {
                   showNotification(ui = "You must upload or download data before generic species can be added.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "no_data_for_generics_error",
                                    type = "error")
                 } else if (input$species_joining_var == "" | input$data_joining_var == "") {
                   showNotification(ui = "You must identify the variable containing species codes in both your data and species list before generic species can be added.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "no_data_for_generics_error",
                                    type = "error")
                 } else if (input$growth_habit_var == "" | input$duration_var == "") {
                   showNotification(ui = "You must specify the growth habit and duration variables in order to add generic species codes.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    type = "error",
                                    id = "undefined_unknown_vars_error")
                 } else {
                   # In case there are generic codes to accommodate
                   message("Getting ready to add generic codes")
                   message(paste0("length(workspace$data) is ",
                                  length(workspace$data)))
                   species_list_with_generics <- unique(terradactyl::generic_growth_habits(data = workspace$data,
                                                                                           data_code = input$data_joining_var,
                                                                                           species_list = workspace$species_data,
                                                                                           species_code = input$species_joining_var,
                                                                                           species_growth_habit_code = input$growth_habit_var,
                                                                                           species_duration = input$duration_var))
                   # Make sure that the growth habit and duration information is
                   # in the correct variables
                   # Which indices have the attributed generic codes?
                   unknown_indices <- is.na(species_list_with_generics[[input$growth_habit_var]]) & !is.na(species_list_with_generics[["GrowthHabitSub"]])
                   
                   # At those indices, write in the growth habit and duration info
                   # from the default variables to the user's selected variables
                   if (any(unknown_indices)) {
                     species_list_with_generics[[input$growth_habit_var]][unknown_indices] <- as.character(species_list_with_generics[["GrowthHabitSub"]][unknown_indices])
                     species_list_with_generics[[input$duration_var]][unknown_indices] <- as.character(species_list_with_generics[["Duration"]][unknown_indices])
                   }
                   
                   # Reduce to only the variables we had coming into this
                   species_list_with_generics <- select(species_list_with_generics,
                                                        names(workspace$species_data))
                   
                   workspace$species_data <- species_list_with_generics
                 }
               })
  
  ##### When join_species is clicked #####
  observeEvent(eventExpr = input$join_species,
               handlerExpr = {
                 message("Join species button was pressed!")
                 if (input$species_joining_var != "" & input$data_joining_var != "") {
                   message("Joining variable is defined for both the data and the lookup table")
                   # Check to see if there're repeat species, which is forbidden
                   species_summary_vector <- table(workspace$species_data[[input$species_joining_var]])
                   
                   if (any(species_summary_vector > 1)) {
                     duplicated_species_count <- sum(species_summary_vector > 1)
                     if (duplicated_species_count == 1) {
                       species_uniqueness_error <- paste0("Joining failed. There is ",
                                                          1,
                                                          " non-unique species code in your species data. Please resolve this and reupload to proceed.")
                     } else {
                       species_uniqueness_error <- paste0("Joining failed. There are ",
                                                          sum(species_summary_vector > 1),
                                                          " non-unique species codes in your species data. Please resolve this to proceed")
                     }
                     
                     showNotification(ui = species_uniqueness_error,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error")
                   } else {
                     message("Figuring out which species are in the data but not the lookup table")
                     current_data_species <- unique(workspace$data[[input$data_joining_var]])
                     current_list_species <- unique(workspace$species_data[[input$species_joining_var]])
                     
                     missing_species <- current_data_species[!(current_data_species %in% current_list_species)]
                     
                     if (length(missing_species) > 0) {
                       message("Codes/species found in the data which do not occur in the lookup table")
                       message("Making  data frame of missing codes/species")
                       
                       # Make a data frame with just the missing codes in it
                       missing_species_df <- data.frame("code" = missing_species)
                       names(missing_species_df) <- input$species_joining_var
                       
                       # Add in the unpopulated variables to match workspace$species_data
                       existing_species_vars <- names(workspace$species_data)
                       missing_species_vars <- existing_species_vars[!(existing_species_vars %in% names(missing_species_df))]
                       for (var in missing_species_vars) {
                         missing_species_df[[var]] <- NA
                       }
                       # Reorder variables to match
                       missing_species_df <- missing_species_df[, existing_species_vars]
                       
                       # Mash up the "empty" data frame and the one in workspace$species_data
                       workspace$species_data <- rbind(missing_species_df,
                                                       workspace$species_data)
                       
                       showNotification(ui = "Not all codes/species in the data appeared in the species table provided; see the table in the Data Configuration tab. You can download the species table with the added codes to populate as appropriate, reupload, and recalculate.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning")
                       
                     } else {
                       message("No missing species found")
                     }
                     
                     # Render the species list
                     output$species_lut <- DT::renderDT(workspace$species_data,
                                                        options = list(pageLength = 10,
                                                                       fixedHeader = TRUE,
                                                                       scrollX = TRUE), 
                                                        extensions = "FixedHeader")
                     
                     
                     message("Joining species information to data.")
                     by_vector <- c(input$species_joining_var)
                     names(by_vector) <- input$data_joining_var
                     workspace$data <- dplyr::left_join(x = workspace$data,
                                                        y = workspace$species_data,
                                                        by = by_vector)
                     if (input$data_type %in% c("lpi", "height")) {
                       message("Updating available grouping variables.")
                       updateSelectInput(inputId = paste0(input$data_type,
                                                          "_grouping_vars"),
                                         choices = names(workspace$data),
                                         selected = "")
                     }
                   }
                   
                   # Handle the downloading bit
                   # Starting by writing out the data
                   workspace$current_species_data_filename <- paste0("species_data_",
                                                                     paste(format(Sys.Date(),
                                                                                  "%Y-%m-%d"),
                                                                           format(Sys.time(),
                                                                                  "T%H%MZ",
                                                                                  tz = "GMT"),
                                                                           sep = "_"),
                                                                     ".csv")
                   message("Writing species data to:")
                   message(paste0(workspace$temp_directory,
                                  "/",
                                  workspace$current_species_data_filename))
                   write.csv(x = workspace$species_data,
                             file = paste0(workspace$temp_directory,
                                           "/",
                                           workspace$current_species_data_filename),
                             row.names = FALSE)
                   
                   # Then we prep the data for download
                   message("Running the downloadHandler() call.")
                   output$downloadable_species <- downloadHandler(
                     filename = workspace$current_species_data_filename,
                     content = function(file) {
                       file.copy(paste0(workspace$temp_directory,
                                        "/",
                                        workspace$current_species_data_filename), file)
                     })
                   message("downloadHandler() call complete.")
                   
                 } else {
                   showNotification(ui = "The joining variables must both be defined in order to join species information to the data.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "missing_species_join_vars",
                                    type = "warning")
                 }
                 
               })
  
  ##### Calculating #####
  observeEvent(eventExpr = input$calculate_button,
               handlerExpr = {
                 message("Prepping for calculation")
                 # So we can check to make sure they've defined variables
                 all_required_variables <- workspace$required_vars[[input$data_type]]
                 required_vars_input_variables <- paste0(tolower(all_required_variables),
                                                         "_var")
                 message("Getting current required variable names")
                 # Can't index reactivevalues with brackets????
                 # Hacky workaround!
                 current_variable_values <- unlist(sapply(X = required_vars_input_variables,
                                                          inputs = input,
                                                          FUN = function(X, inputs) {
                                                            eval(parse(text = paste0("inputs$",
                                                                                     X)))
                                                          }))
                 
                 if (is.null(workspace$data)) {
                   message("No data! Can't calculate.")
                   showNotification(ui = "No data found. Please upload or download data.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    type = "error")
                 } else if (any(current_variable_values == "")) {
                   message("Missing required variable names! Can't calculate.")
                   showNotification(ui = "Not all required variables are defined. Please update those under 'Data configuration'.",
                                    id = "missing_required_vars_error",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    type = "error")
                   
                 } else {
                   message("Should be good to go. Calculating!")
                   showNotification(ui = "Calculating!",
                                    id = "calculating",
                                    duration = NULL,
                                    closeButton = FALSE,
                                    type = "message")
                   
                   message("Copying workspace$data to workspace$calc_data")
                   workspace$calc_data <- workspace$data
                   message("Updating variables in workspace$calc_data to match terradactyl expectations")
                   # Let's update the variables in workspace$calc_data
                   # This just looks at the required variables for the current data type
                   all_required_variables <- workspace$required_vars[[input$data_type]]
                   
                   for (required_var in all_required_variables) {
                     inputid_string <- paste0(tolower(required_var),
                                              "_var")
                     current_var_value <- input[[inputid_string]]
                     
                     if (current_var_value != "") {
                       message(paste0("Writing contents of worskpace$calc_data$",
                                      current_var_value,
                                      " to workspace$data$",
                                      required_var))
                       workspace$calc_data[[required_var]] <- workspace$calc_data[[current_var_value]]
                     } else {
                       message(paste0("No variable identified for ",
                                      required_var))
                     }
                   }
                   
                   message("Calculating!")
                   switch(input$data_type,
                          "lpi" = {
                            message("Calculating cover from LPI.")
                            
                            if (input$lpi_hit %in% c("any", "first", "basal")) {
                              message("This is a generalized LPI calc and will use pct_cover()")
                              # Handle the grouping variables (if any)!
                              message("input$lpi_grouping_vars is:")
                              message(input$lpi_grouping_vars)
                              
                              current_lpi_grouping_vars <- input$lpi_grouping_vars
                              current_lpi_grouping_vars <- current_lpi_grouping_vars[!(current_lpi_grouping_vars %in% c(""))]
                              
                              if (length(current_lpi_grouping_vars) > 0) {
                                lpi_grouping_vars_vector <- current_lpi_grouping_vars
                                
                                message("There are grouping variables.")
                                current_lpi_vars <- names(workspace$calc_data)
                                missing_lpi_grouping_vars <- lpi_grouping_vars_vector[!(lpi_grouping_vars_vector %in% current_lpi_vars)]
                                available_lpi_grouping_vars <- lpi_grouping_vars_vector[lpi_grouping_vars_vector %in% current_lpi_vars]
                                
                                if (length(missing_lpi_grouping_vars) < 1) {
                                  message("No variables missing!")
                                  lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover(",
                                                             "lpi_tall = workspace$calc_data,",
                                                             "tall = input$lpi_output_format == 'long',",
                                                             "hit = input$lpi_hit,",
                                                             "by_line = input$lpi_unit == 'line',",
                                                             paste(lpi_grouping_vars_vector,
                                                                   collapse = ","),
                                                             "),error = function(error){error})")
                                } else {
                                  message("Missing one or more variables.")
                                  missing_lpi_grouping_vars_warning <- paste0("The following variables are missing: ",
                                                                              paste(missing_lpi_grouping_vars,
                                                                                    collapse = ", "),
                                                                              ". Results will be calculated without grouping.")
                                  showNotification(ui = missing_lpi_grouping_vars_warning,
                                                   duration = NULL,
                                                   closeButton = TRUE,
                                                   id = "missing_lpi_grouping_vars",
                                                   type = "warning")
                                  lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover(",
                                                             "lpi_tall = workspace$calc_data,",
                                                             "tall = input$lpi_output_format == 'long',",
                                                             "hit = input$lpi_hit,",
                                                             "by_line = input$lpi_unit == 'line'",
                                                             "),error = function(error){error})")
                                }
                                
                              } else {
                                message("No grouping variables.")
                                lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover(",
                                                           "lpi_tall = workspace$calc_data,",
                                                           "tall = input$lpi_output_format == 'long',",
                                                           "hit = input$lpi_hit,",
                                                           "by_line = input$lpi_unit == 'line'",
                                                           "),error = function(error){error})")
                              }
                            } else {
                              message("This is a specialized LPI call and will be using a wrapper for pct_cover()")
                              switch(input$lpi_hit,
                                     "species" = {
                                       lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover_species(",
                                                                  "lpi_tall = workspace$calc_data,",
                                                                  "tall = input$lpi_output_format == 'long',",
                                                                  "by_line = input$lpi_unit == 'line'",
                                                                  "),error = function(error){error})")
                                     },
                                     "bare_ground" = {
                                       lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover_bare_soil(",
                                                                  "lpi_tall = workspace$calc_data,",
                                                                  "tall = input$lpi_output_format == 'long',",
                                                                  "by_line = input$lpi_unit == 'line'",
                                                                  "),error = function(error){error})")
                                     },
                                     "litter" = {
                                       lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover_litter(",
                                                                  "lpi_tall = workspace$calc_data,",
                                                                  "tall = input$lpi_output_format == 'long',",
                                                                  "by_line = input$lpi_unit == 'line'",
                                                                  "),error = function(error){error})")
                                     },
                                     "between_plant" = {
                                       lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover_between_plant(",
                                                                  "lpi_tall = workspace$calc_data,",
                                                                  "tall = input$lpi_output_format == 'long',",
                                                                  "by_line = input$lpi_unit == 'line'",
                                                                  "),error = function(error){error})")
                                     },
                                     "total_foliar" = {
                                       lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover_total_foliar(",
                                                                  "lpi_tall = workspace$calc_data,",
                                                                  "tall = input$lpi_output_format == 'long',",
                                                                  "by_line = input$lpi_unit == 'line'",
                                                                  "),error = function(error){error})")
                                     },
                                     "nonplant_ground" = {
                                       lpi_cover_string <- paste0("tryCatch(terradactyl::pct_cover_all_ground(",
                                                                  "lpi_tall = workspace$calc_data,",
                                                                  "tall = input$lpi_output_format == 'long',",
                                                                  "by_line = input$lpi_unit == 'line'",
                                                                  "),error = function(error){error})")
                                     })
                            }
                            
                            message("The function call is:")
                            message(lpi_cover_string)
                            current_results <- eval(parse(text = lpi_cover_string))
                          },
                          "gap" = {
                            message("Calculating gaps.")
                            current_gap_breaks <- stringr::str_split(string = input$gap_breaks,
                                                                     pattern = ",",
                                                                     simplify = TRUE)
                            current_gap_breaks <- as.numeric(trimws(current_gap_breaks))
                            
                            if (any(is.na(current_gap_breaks))) {
                              message("At least one of the gap breakpoints isn't numeric!")
                              gap_results <- "One or more of the gap breakpoints is non-numeric. Please provide the gap breaks separated by commas."
                            } else if (length(input$gap_indicator_types) < 1) {
                              message("No indicator type selected")
                              gap_results <- "No indicator types selected to calculate."
                            } else {
                              message("Gap breaks are all good and at least one indicator type is selected")
                              message("Calcaulating gap")
                              gap_results <- tryCatch(terradactyl::gap_cover(gap_tall = workspace$calc_data,
                                                                             tall = input$gap_output_format == "long",
                                                                             breaks = current_gap_breaks,
                                                                             type = input$gap_type,
                                                                             by_line = (input$gap_unit == "line")),
                                                      error = function(error){
                                                        error
                                                      })
                              gap_results <- gap_results[input$gap_indicator_types]
                              message(paste0("Remaining names of gap results are: ",
                                             paste(names(gap_results),
                                                   collapse = ", ")))
                              message("Gaps calculated")
                            }
                            
                            # Only if we actually calculated anything!
                            if (!is.character(gap_results)) {
                              message("Gap results aren't character")
                              # Apparently gap_cover() returns a list of data frames
                              # when tall = FALSE so let's combine them
                              if (input$gap_output_format == "wide") {
                                # First up is to rename the variables using the stats
                                for (index in seq_len(length(gap_results))) {
                                  current_stat <- names(gap_results)[index]
                                  current_vars <- names(gap_results[[index]])
                                  gap_class_var_indices <- grepl(current_vars,
                                                                 pattern = "\\d|NoGap")
                                  gap_class_vars <- current_vars[gap_class_var_indices]
                                  names(gap_results[[index]])[gap_class_var_indices] <- paste0(gap_class_vars,
                                                                                               "_",
                                                                                               current_stat)
                                }
                                # Then mash them together
                                current_results <- Reduce(f = dplyr::full_join,
                                                          x = gap_results)
                              } else {
                                # If the results are long, we're already ready to go
                                current_results <- do.call(rbind,
                                                           gap_results)
                              }
                            } else {
                              message("Gap results are an error message")
                              current_results <- gap_results
                            }
                            
                          },
                          "height" = {
                            message("Handling grouping variables")
                            # Handle the grouping variables (if any)!
                            height_grouping_vars_vector <- stringr::str_split(string = input$height_grouping_vars,
                                                                              pattern = ",",
                                                                              simplify = TRUE)
                            height_grouping_vars_vector <- as.vector(height_grouping_vars_vector)
                            height_grouping_vars_vector <- trimws(height_grouping_vars_vector)
                            
                            # Total bandaid
                            if (length(height_grouping_vars_vector) < 1) {
                              height_grouping_vars_vector <- ""
                            }
                            
                            message(paste0("Current height_grouping_vars_vector is: ",
                                           paste(height_grouping_vars_vector,
                                                 collapse = ", ")))
                            message(paste0("The length of height_grouping_vars_vector is ",
                                           length(height_grouping_vars_vector)))
                            
                            height_by_line <- input$height_unit == "line"
                            output_tall <- input$height_output_format == "long"
                            
                            if (!("" %in% height_grouping_vars_vector)) {
                              message("There are grouping variables!")
                              current_height_vars <- names(workspace$calc_data)
                              missing_height_grouping_vars <- height_grouping_vars_vector[!(height_grouping_vars_vector %in% current_height_vars)]
                              available_height_grouping_vars <- height_grouping_vars_vector[height_grouping_vars_vector %in% current_height_vars]
                              message(paste0("missing_height_grouping_vars is currently: ",
                                             paste(missing_height_grouping_vars,
                                                   collapse = ", ")))
                              
                              if (length(missing_height_grouping_vars) < 1) {
                                height_cover_string <- paste0("tryCatch(terradactyl::mean_height(",
                                                              "height_tall = workspace$calc_data,",
                                                              "method = 'mean',",
                                                              "omit_zero = input$height_omit_zero,",
                                                              "by_line = height_by_line,",
                                                              "tall = output_tall,",
                                                              paste(height_grouping_vars_vector,
                                                                    collapse = ","),
                                                              "),error = function(error){error})"
                                )
                              } else {
                                missing_height_grouping_vars_warning <- paste0("The following variables are missing: ",
                                                                               paste(missing_height_grouping_vars,
                                                                                     collapse = ", "),
                                                                               ". Results will be calculated without grouping.")
                                showNotification(ui = missing_height_grouping_vars_warning,
                                                 duration = NULL,
                                                 closeButton = TRUE,
                                                 id = "missing_height_grouping_vars",
                                                 type = "warning")
                                height_cover_string <- paste0("tryCatch(terradactyl::mean_height(",
                                                              "height_tall = workspace$calc_data,",
                                                              "method = 'mean',",
                                                              "omit_zero = input$height_omit_zero,",
                                                              "by_line = height_by_line,",
                                                              "tall = output_tall",
                                                              "),error = function(error){error})"
                                )
                              }
                              
                            } else {
                              message("No grouping vars!")
                              height_cover_string <- paste0("tryCatch(terradactyl::mean_height(",
                                                            "height_tall = workspace$calc_data,",
                                                            "method = 'mean',",
                                                            "omit_zero = input$height_omit_zero,",
                                                            "by_line = height_by_line,",
                                                            "tall = output_tall",
                                                            "),error = function(error){error})"
                              )
                            }
                            message("The function call is:")
                            message(height_cover_string)
                            message("Parsing")
                            current_results <- eval(parse(text = height_cover_string))
                            message("Parsed")
                            
                          },
                          "soilstability" = {
                            message("Calculating soil stability")
                            current_results <- tryCatch(terradactyl::soil_stability(soil_stability_tall = workspace$calc_data,
                                                                                    all = "all" %in% input$soil_covergroups,
                                                                                    cover = "covered" %in% input$soil_covergroups,
                                                                                    uncovered = "uncovered" %in% input$soil_covergroups,
                                                                                    all_cover_type = "by_type" %in% input$soil_covergroups,
                                                                                    tall = input$soil_output_format == "long"),
                                                        error = function(error){
                                                          error
                                                        })
                          })
                   
                   message(paste0("class(current_results) is: ",
                                  paste0(class(current_results),
                                         collapse = ", ")))
                   # This is where we do error handling
                   if ("character" %in% class(current_results)) {
                     showNotification(ui = paste0("The calculation produced the following error: ",
                                                  current_results),
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "results_error",
                                      type = "error")
                     workspace$results <- NULL
                   } else if ("data.frame" %in% class(current_results)) {
                     message("Results appear valid.")
                     workspace$results <- current_results
                   } else {
                     message("Something went deeply wrong and the calculation returned neither results nor an error")
                     message(paste0("Asking is.null(current_results) returns ", is.null(current_results)))
                     message(paste0("The current class of current_results is ", class(current_results)))
                     message("Pasting current_results results in:")
                     message(paste0(current_results))
                     showNotification(ui = "Something went very wrong but did not produce an error message.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "unknown_results_error",
                                      type = "error")
                     workspace$results <- NULL
                   }
                   
                   if (!is.null(workspace$results)) {
                     # Just in case there are row names (which shouldn't be there)
                     row.names(workspace$results) <- NULL
                     
                     # Let's update the variable names to reflect what they came in as
                     current_results_vars <- names(workspace$results)
                     current_required_vars <- workspace$required_vars[[input$data_type]]
                     
                     for (required_var in current_required_vars) {
                       input_variable_name <- paste0(tolower(required_var),
                                                     "_var")
                       incoming_variable_name <- input[[input_variable_name]]
                       names(workspace$results)[names(workspace$results) == required_var] <- incoming_variable_name
                     }
                     
                     # And add in the requested metadata variables
                     if (!is.null(workspace$metadata_lut)) {
                       if (any(table(workspace$metadata_lut[[input$primarykey_var]]) > 1)) {
                         showNotification(ui = "Metadata table includes multiple repeat unique IDs and will be ignored.",
                                          duration = NULL,
                                          close_button = TRUE,
                                          id = "bad_metadata_lut",
                                          type = "warning")
                       } else {
                         non_metadata_vars <- names(workspace$results)[!(names(workspace$results) %in% input$primarykey_var)]
                         metadata_vars <- names(workspace$metadata_lut)
                         current_results <- dplyr::left_join(x = workspace$results,
                                                             y = workspace$metadata_lut,
                                                             by = input$primarykey_var)
                         workspace$results <- current_results[, c(metadata_vars,
                                                                  non_metadata_vars)]
                       }
                     }
                     
                     message("Switching to Results tab")
                     updateTabsetPanel(session = session,
                                       inputId = "maintabs",
                                       selected = "Results")
                   }
                   
                   removeNotification(session = session,
                                      id = "calculating")
                 }
               })
  
  ##### When results update #####
  observeEvent(eventExpr = workspace$results,
               handlerExpr = {
                 message("The results have updated!")
                 if (!is.null(workspace$results)) {
                   message(paste0("The class of results is: ", paste(class(workspace$results), sep = ", ")))
                   message(paste0("The number of rows in results is: ", nrow(workspace$results)))
                   message(paste0("The number of columns in results is: ", ncol(workspace$results)))
                   
                   # But we want to round to 2 decimal places for ease-of-reading
                   display_results <- workspace$results
                   # Which indices are numeric variables at?
                   numeric_var_indices <- which(sapply(X = display_results,
                                                       FUN = is.numeric))
                   message(paste0("Numeric variable indices in display_results are: ",
                                  paste(numeric_var_indices,
                                        collapse = ", ")))
                   message(paste0("That's a total of ",
                                  length(numeric_var_indices),
                                  " variables out of ",
                                  ncol(display_results),
                                  " variables in display_results."))
                   # If there are numeric variables, round them to 2 decimal places
                   if (length(numeric_var_indices) > 0 & max(numeric_var_indices) <= ncol(display_results)) {
                     message("Attempting to round display_result values.")
                     # APPARENTLY dplyr::all_of() is for character vectors, not numeric vectors
                     numeric_var_names <- names(display_results)[numeric_var_indices]
                     display_results <- dplyr::mutate(.data = display_results,
                                                      dplyr::across(.cols = dplyr::all_of(numeric_var_names),
                                                                    .fns = round,
                                                                    digits = 2))
                   }
                   
                   output$results_table <- DT::renderDT(display_results,
                                                        options = list(pageLength = 10,
                                                                       fixedHeader = TRUE,
                                                                       scrollX = TRUE), 
                                                        extensions = "FixedHeader")
                   message("output$results_table rendered")
                   software_version_string <- paste0("These results were calculated using terradactyl v",
                                                     packageVersion("terradactyl"),
                                                     " and R v",
                                                     R.Version()$major, ".", R.Version()$minor,
                                                     " on ",
                                                     format(Sys.Date(),
                                                            "%Y-%m-%d"),
                                                     ".")
                   output$metadata_text <- renderText(software_version_string)
                   message("output$metadata_text has been rendered")
                   message("Switching to Results tab")
                   updateTabsetPanel(session = session,
                                     inputId = "maintabs",
                                     selected = "Results")
                   
                   # Handle the downloading bit
                   # Starting by writing out the data
                   workspace$current_results_filename <- paste0(input$data_type,
                                                                "_results_",
                                                                paste(format(Sys.Date(),
                                                                             "%Y-%m-%d"),
                                                                      format(Sys.time(),
                                                                             "T%H%MZ",
                                                                             tz = "GMT"),
                                                                      sep = "_"),
                                                                ".csv")
                   message("Writing results to:")
                   message(paste0(workspace$temp_directory,
                                  "/",
                                  workspace$current_results_filename))
                   write.csv(x = workspace$results,
                             file = paste0(workspace$temp_directory,
                                           "/",
                                           workspace$current_results_filename),
                             row.names = FALSE)
                   
                   # Then we prep the data for download
                   message("Running the downloadHandler() call.")
                   output$downloadable_data <- downloadHandler(
                     filename = workspace$current_results_filename,
                     content = function(file) {
                       file.copy(paste0(workspace$temp_directory,
                                        "/",
                                        workspace$current_results_filename), file)
                     })
                   message("downloadHandler() call complete.")
                   # Display download button only if there are data for download
                   output$download_button_ui <- renderUI(expr = {
                     downloadButton(outputId = 'downloadable_data',
                                    label = 'Download results')
                   })
                 } else {
                   message("workspace$results is NULL")
                   output$download_button_ui <- renderUI(expr = {return(NULL)})
                 }
               })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
