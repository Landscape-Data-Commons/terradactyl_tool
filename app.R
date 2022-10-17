library(shiny)
library(dplyr)
library(DT)
library(stringr)
library(tippy)
source("functions.R")

# Define UI for application
ui <- fluidPage(
  # Formatting for notification and error messages
  tags$head(
    tags$style(
      HTML(
        ".shiny-notification {
          position:fixed;
          top: calc(30%);
          left: calc(5%);
          width: calc(25%);
          opacity: 1;
          font-weight: bold;
          box-shadow: 0 0 0 rgba(181,181,181, 0.4);
          animation: pulse 2s infinite;
        }
        @-webkit-keyframes pulse {
          0% {
            -webkit-box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
          }
          70% {
            -webkit-box-shadow: 0 0 0 10px rgba(181,181,181, 0);
          }
          100% {
            -webkit-box-shadow: 0 0 0 0 rgba(181,181,181, 0);
          }
        }
        @keyframes pulse {
          0% {
            -moz-box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
            box-shadow: 0 0 0 0 rgba(181,181,181, 0.4);
          }
          70% {
            -moz-box-shadow: 0 0 0 10px rgba(181,181,181, 0);
            box-shadow: 0 0 0 10px rgba(181,181,181, 0);
          }
          100% {
            -moz-box-shadow: 0 0 0 0 rgba(181,181,181, 0);
            box-shadow: 0 0 0 0 rgba(181,181,181, 0);
          }
        }"
      )
    )
  ),
  
  #### Interface ###################################
  # Application title
  titlePanel(img(src = "combined_logos.png",
                 align = "right"),
             windowTitle = "Rangeland Indicator Calculator"),
  titlePanel(title = "Rangeland Indicator Calculator"),
  
  ##### Sidebar #####
  sidebarLayout(
    sidebarPanel(
      HTML("Powered by <a href='https://github.com/Landscape-Data-Commons/terradactyl'>terradactyl</a>"),
      hr(),
      selectInput(inputId = "data_type",
                  label = "Data type",
                  choices = c("Line-Point Intercept" = "lpi",
                              "Height" = "height",
                              "Gap" = "gap",
                              "Soil Stability" = "soilstability"),
                  selected = "lpi"),
      radioButtons(inputId = "data_source",
                   label = "Data source",
                   choices = c("Upload" = "upload",
                               "Query Landscape Data Commons" = "ldc")),
      conditionalPanel(condition = "input.data_source == 'upload'",
                       fileInput(inputId = "raw_data",
                                 label = "Data CSV",
                                 multiple = FALSE,
                                 accept = ".csv")
      ),
      
      conditionalPanel(condition = "input.data_source == 'ldc'",
                       selectInput(inputId = "key_type",
                                   label = "LDC search type",
                                   choices = c("By ecological site" = "EcologicalSiteID",
                                               "By PrimaryKey" = "PrimaryKey",
                                               "By ProjectKey" = "ProjectKey"),
                                   selected = "ecosite"),
                       textInput(inputId = "keys",
                                 label = "Search values",
                                 value = "",
                                 placeholder = "R042XB012NM"),
                       tippy_this(elementId = "keys",
                                  tooltip = "Separate multiple values with commas.",
                                  placement = "right",
                                  delay = c(200, 0)),
                       actionButton(inputId = "fetch_data",
                                    label = "Fetch data")),
      hr()
    ),
    
    
    ##### Tabs #####
    mainPanel(
      tabsetPanel(type = "tabs",
                  id = "maintabs",
                  tabPanel(title = "Instructions",
                           includeHTML("instructions.html")),
                  tabPanel(title = "Data configuration",
                           actionLink(inputId = "data_help",
                                      label = "What do these options mean?"),
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
                                                        choices = c(""))),
                           actionButton(inputId = "update_data_vars",
                                        label = "Update data variables"),
                           hr(),
                           # Species lookup table stuff
                           conditionalPanel(condition = "input.data_type == 'lpi' || input.data_type == 'height'",
                                            checkboxInput(inputId = "use_species",
                                                          label = "Add species information",
                                                          value = FALSE),
                                            conditionalPanel(condition = "input.use_species",
                                                             radioButtons(inputId = "species_source",
                                                                          label = "Species lookup table source",
                                                                          choices = c("Default USDA Plants" = "default",
                                                                                      "Upload" = "upload"),
                                                                          selected = 0),
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
                                                             conditionalPanel(condition = "input.data_joining_var != '' && input.species_joining_var != ''",
                                                                              actionButton(inputId = "join_species",
                                                                                           label = "Join species information to data"),
                                                                              conditionalPanel(condition = "input.join_species > 0",
                                                                                               DT::dataTableOutput(outputId = "species_lut"),
                                                                                               downloadButton(outputId = 'downloadable_species',
                                                                                                              label = 'Download current species information'))
                                                                              
                                                             )
                                                             
                                            )
                           )
                           
                  ),
                  tabPanel(title = "Indicator calculation",
                           actionLink(inputId = "indicator_help",
                                      label = "What do these options mean?"),
                           conditionalPanel(condition = "input.data_type == 'lpi'",
                                            radioButtons(inputId = "lpi_hit",
                                                         label = "Use first, any, or basal hit?",
                                                         choices = c("Any hit" = "any",
                                                                     "First hit" = "first",
                                                                     "Basal hit" = "basal")),
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
                                                       delay = c(200, 0)),
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
                                                       delay = c(200, 0)),
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
                           actionButton(inputId = "calculate_button",
                                        label = "Calculate!")
                  ),
                  tabPanel(title = "Data",
                           actionButton(inputId = "reset_data",
                                        label = "Reset data"),
                           DT::dataTableOutput(outputId = "data")),
                  tabPanel(title = "Results",
                           conditionalPanel(condition = "output.results_table !== null",
                                            downloadButton(outputId = 'downloadable_data',
                                                           label = 'Download results')),
                           textOutput(outputId = "metadata_text"),
                           DT::dataTableOutput(outputId = "results_table")),
                  tabPanel(title = "Help",
                           includeHTML("help.html"))
      )
    )
  )
)

#### Server ######################
server <- function(input, output, session) {
  ##### Intialization #####
  # Allow for wonking big files
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  # Our workspace list for storing stuff
  workspace <- reactiveValues(temp_directory = tempdir(),
                              original_directory = getwd(),
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
                                                   "soil" = c("PrimaryKey",
                                                              "Rating",
                                                              "Veg")))
  
  ##### Directing to help #####
  observeEvent(eventExpr = input$indicator_help,
               handlerExpr = {
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Help")
               })
  observeEvent(eventExpr = input$data_help,
               handlerExpr = {
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Help")
               })
  
  
  ##### CSV upload handling #####
  # When input$raw_data updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$raw_data,
               handlerExpr = {
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
                 if (input$species_source == "default") {
                   message("Species source set to default")
                   defaults_species_filepath <- paste0(workspace$original_directory,
                                                       "/",
                                                       workspace$default_species_filename)
                   workspace[["species_data"]] <- read.csv(defaults_species_filepath,
                                                           stringsAsFactors = FALSE)
                   # TODO
                   # Set workspace$species_data to the default list (which doesn't exist yet)
                   # Set this variable so we can handle the data appropriately based on source
                   workspace$current_species_source <- "default"
                 }
               })
  
  ##### When workspace$species_data updates #####
  observeEvent(eventExpr = workspace$species_data,
               handlerExpr = {
                 current_species_data_vars <- names(workspace$species_data)
                 
                 if ("code" %in% current_species_data_vars) {
                   selection <- "code"
                 } else {
                   selection <- ""
                 }
                 
                 
                 updateSelectInput(session = session,
                                   inputId = "species_joining_var",
                                   choices = c("",
                                               current_species_data_vars),
                                   selected = selection)
               })
  
  ##### Fetching data from the LDC #####
  observeEvent(eventExpr = input$fetch_data,
               handlerExpr = {
                 showNotification(ui = "Downloading data from the LDC. Please wait.",
                                  duration = NULL,
                                  closeButton = FALSE,
                                  id = "downloading",
                                  type = "message")
                 
                 # Set this variable so we can handle the data appropriately based on source
                 # Since there are from the LDC, we'll also be looking for header info
                 workspace$current_data_source <- "ldc"
                 message("Data source set to LDC")
                 
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
                   if (input$key_type == "EcologicalSiteID") {
                     message("key_type is EcologicalSiteID")
                     message("Retrieving headers")
                     current_headers <- fetch_ldc(keys = current_key_string,
                                                  key_type = input$key_type,
                                                  data_type = "header",
                                                  verbose = TRUE)
                     
                     current_primary_keys <- paste(current_headers$PrimaryKey,
                                                   collapse = ",")
                     message("Retrieving data using PrimaryKey values from headers")
                     results <- fetch_ldc(keys = current_primary_keys,
                                          key_type = "PrimaryKey",
                                          data_type = input$data_type,
                                          verbose = TRUE)
                   } else {
                     message("key_type is not EcologicalSiteID")
                     message("Retrieving data using provided keys")
                     results <- fetch_ldc(keys = current_key_string,
                                          key_type = input$key_type,
                                          data_type = input$data_type,
                                          verbose = TRUE)
                   }
                   
                   
                   # So we can tell the user later which actually got queried
                   if (is.null(results)) {
                     message("No data from LDC!")
                     workspace$missing_keys <- current_key_vector
                   } else {
                     message("Determining if keys are missing.")
                     
                     # Because ecosites were two-stage, we check in with headers
                     if (input$key_type == "EcologicalSiteID") {
                       workspace$queried_keys <- unique(current_headers[[input$key_type]])
                     } else {
                       workspace$queried_keys <- unique(results[[input$key_type]])
                     }
                     
                     workspace$missing_keys <- current_key_vector[!(current_key_vector %in% workspace$queried_keys)]
                   }
                   
                   if (length(workspace$missing_keys) > 0) {
                     message(paste0("The following key values are missing: ",
                                    paste(workspace$missing_keys,
                                          collapse = ", ")))
                     key_error <- paste0("Data could not be retrieved from the LDC for the following keys: ",
                                         paste(workspace$missing_keys,
                                               collapse = ", "))
                     showNotification(ui = key_error,
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "warning",
                                      id = "key_error")
                   }
                   
                   
                   # Only keep going if there are results!!!!
                   if (length(results) > 0) {
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
                 # Display the data
                 # But we want to round to 2 decimal places for ease-of-reading
                 display_data <- workspace$data
                 # Which indices are numeric variables?
                 numeric_var_indices <- which(sapply(X = display_data,
                                                     FUN = is.numeric))
                 # If any variables are numeric, round them to 2 decimal places
                 if (length(numeric_var_indices) > 0) {
                   # APPARENTLY dplyr::all_of() is for character vectors, not numeric vectors
                   numeric_var_names <- names(display_data)[numeric_var_indices]
                   display_data <- dplyr::mutate(.data = display_data,
                                                 dplyr::across(.cols = dplyr::all_of(numeric_var_names),
                                                               .fns = round,
                                                               digits = 2))
                 }
                 
                 output$data <- DT::renderDataTable(display_data,
                                                    options = list(pageLength = 100))
                 
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
                       
                       message(paste0("Selected variable is ", selected_var))
                       
                       updateSelectInput(inputId = inputid_string,
                                         choices = current_data_vars,
                                         selected = selected_var)
                       message(paste0("Finished updating selectInput(inputId = ",
                                      inputid_string,
                                      ")"))
                       
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
                   
                   if (length(missing_data_vars) > 0) {
                     message("Missing one or more required variables.")
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
  
  ##### When join_species is clicked #####
  observeEvent(eventExpr = input$join_species,
               handlerExpr = {
                 if (input$species_joining_var != "" & input$data_joining_var != "") {
                   
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
                     output$species_lut <- DT::renderDataTable(workspace$species_data,
                                                               options = list(pageLength = 100))
                     
                     
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
  
  
  ##### When update vars button is pressed #####
  # Vestigial, but annoying to recreate if I need it, so storing it here
  # c(input$primarykey_var,
  #   input$linekey_var,
  #   input$code_var,
  #   input$pointnbr_var,
  #   input$layer_var,
  #   input$linelengthamount_var,
  #   input$measure_var,
  #   input$rectype_var,
  #   input$gap_var,
  #   input$height_var,
  #   input$species_var,
  #   input$rating_var,
  #   input$veg_var)
  
  
  observeEvent(eventExpr = input$update_data_vars,
               handlerExpr = {
                 # Let's update the variables in workspace$data
                 # This just looks at the required variables for the current data type
                 all_required_variables <- workspace$required_vars[[input$data_type]]
                 
                 for (required_var in all_required_variables) {
                   inputid_string <- paste0(tolower(required_var),
                                            "_var")
                   current_var_value <- input[[inputid_string]]
                   
                   if (current_var_value != "") {
                     message(paste0("Writing contents of worskpace$data$",
                                    current_var_value,
                                    " to workspace$data$",
                                    required_var))
                     workspace$data[[required_var]] <- workspace$data[[current_var_value]]
                   } else {
                     message(paste0("No variable identified for ",
                                    required_var))
                   }
                 }
               })
  
  
  
  ##### Calculating #####
  observeEvent(eventExpr = input$calculate_button,
               handlerExpr = {
                 
                 if (is.null(workspace$data)) {
                   showNotification(ui = "Data are not yet ready for calculation.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    type = "error")
                 } else {
                   showNotification(ui = "Calculating!",
                                    id = "calculating",
                                    duration = NULL,
                                    closeButton = FALSE,
                                    type = "message")
                 }
                 message("Calculating!")
                 switch(input$data_type,
                        "lpi" = {
                          message("Calculating cover from LPI.")
                          # Handle the grouping variables (if any)!
                          message("input$lpi_grouping_vars is:")
                          message(input$lpi_grouping_vars)
                          
                          current_lpi_grouping_vars <- input$lpi_grouping_vars
                          current_lpi_grouping_vars <- current_lpi_grouping_vars[!(current_lpi_grouping_vars %in% c(""))]
                          
                          if (length(current_lpi_grouping_vars) > 0) {
                            lpi_grouping_vars_vector <- current_lpi_grouping_vars
                            
                            message("There are grouping variables.")
                            current_lpi_vars <- names(workspace$data)
                            missing_lpi_grouping_vars <- lpi_grouping_vars_vector[!(lpi_grouping_vars_vector %in% current_lpi_vars)]
                            available_lpi_grouping_vars <- lpi_grouping_vars_vector[lpi_grouping_vars_vector %in% current_lpi_vars]
                            
                            if (length(missing_lpi_grouping_vars) < 1) {
                              message("No variables missing!")
                              lpi_cover_string <- paste0("terradactyl::pct_cover(",
                                                         "lpi_tall = workspace$data,",
                                                         "tall = input$lpi_output_format == 'long',",
                                                         "hit = input$lpi_hit,",
                                                         "by_line = input$lpi_unit == 'line',",
                                                         paste(lpi_grouping_vars_vector,
                                                               collapse = ","),
                                                         ")")
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
                              lpi_cover_string <- paste0("terradactyl::pct_cover(",
                                                         "lpi_tall = workspace$data,",
                                                         "tall = input$lpi_output_format == 'long',",
                                                         "hit = input$lpi_hit,",
                                                         "by_line = input$lpi_unit == 'line'",
                                                         ")")
                            }
                            
                          } else {
                            message("No grouping variables.")
                            lpi_cover_string <- paste0("terradactyl::pct_cover(",
                                                       "lpi_tall = workspace$data,",
                                                       "tall = input$lpi_output_format == 'long',",
                                                       "hit = input$lpi_hit,",
                                                       "by_line = input$lpi_unit == 'line'",
                                                       ")")
                          }
                          
                          message("The function call is:")
                          message(lpi_cover_string)
                          workspace$results <- eval(parse(text = lpi_cover_string))
                        },
                        "gap" = {
                          message("Calculating gaps.")
                          current_gap_breaks <- stringr::str_split(string = input$gap_breaks,
                                                                   pattern = ",",
                                                                   simplify = TRUE)
                          current_gap_breaks <- as.numeric(trimws(current_gap_breaks))
                          
                          if (any(is.na(current_gap_breaks))) {
                            message("At least one of the gap breakpoints isn't numeric!")
                            showNotification(ui = "One or more of the gap breakpoints is non-numeric. Please provide the gap breaks separated by commas.",
                                             duration = NA,
                                             closeButton = TRUE,
                                             id = "bad_gap_breaks")
                          } else {
                            message("Gap breaks are all good!")
                            message("Calcaulating gap")
                            gap_results <- terradactyl::gap_cover(gap_tall = workspace$data,
                                                                  tall = input$gap_output_format == "long",
                                                                  breaks = current_gap_breaks,
                                                                  type = input$gap_type,
                                                                  by_line = (input$gap_unit == "line"))
                          }
                          
                          message("Gaps calculated")
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
                            workspace$results <- Reduce(f = dplyr::full_join,
                                                        x = gap_results)
                          } else {
                            # If the results are long, we're already ready to go
                            workspace$results <- gap_results
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
                          
                          if (height_grouping_vars_vector != "") {
                            message("There are grouping variables!")
                            current_height_vars <- names(workspace$data)
                            missing_height_grouping_vars <- height_grouping_vars_vector[!(height_grouping_vars_vector %in% current_height_vars)]
                            available_height_grouping_vars <- height_grouping_vars_vector[height_grouping_vars_vector %in% current_height_vars]
                            message(paste0("missing_height_grouping_vars is currently: ",
                                           paste(missing_height_grouping_vars,
                                                 collapse = ", ")))
                            
                            if (length(missing_height_grouping_vars) < 1) {
                              height_cover_string <- paste0("terradactyl::mean_height(",
                                                            "height_tall = workspace$data,",
                                                            "method = 'mean',",
                                                            "omit_zero = input$height_omit_zero,",
                                                            "by_line = height_by_line,",
                                                            "tall = output_tall,",
                                                            paste(height_grouping_vars_vector,
                                                                  collapse = ","),
                                                            ")"
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
                              height_cover_string <- paste0("terradactyl::mean_height(",
                                                            "height_tall = workspace$data,",
                                                            "method = 'mean',",
                                                            "omit_zero = input$height_omit_zero,",
                                                            "by_line = height_by_line,",
                                                            "tall = output_tall",
                                                            ")"
                              )
                            }
                            
                          } else {
                            message("No grouping vars!")
                            height_cover_string <- paste0("terradactyl::mean_height(",
                                                          "height_tall = workspace$data,",
                                                          "method = 'mean',",
                                                          "omit_zero = input$height_omit_zero,",
                                                          "by_line = height_by_line,",
                                                          "tall = output_tall",
                                                          ")"
                            )
                          }
                          message("The function call is:")
                          message(height_cover_string)
                          message("Parsing")
                          workspace$results <- eval(parse(text = height_cover_string))
                          message("Parsed")
                          
                        },
                        "soilstability" = {
                          message("Calculating soil stability")
                          workspace$results <- terradactyl::soil_stability(soil_stability_tall = workspace$data,
                                                                           all = "all" %in% input$soil_covergroups,
                                                                           cover = "covered" %in% input$soil_covergroups,
                                                                           uncovered = "uncovered" %in% input$soil_covergroups,
                                                                           all_cover_type = "by_type" %in% input$soil_covergroups,
                                                                           tall = input$soil_output_format == "long")
                        })
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
                 
                 
                 removeNotification(session = session,
                                    id = "calculating")
                 
                 message("Switching to Results tab")
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Results")
               })
  
  ##### When results update #####
  observeEvent(eventExpr = workspace$results,
               handlerExpr = {
                 message("The results have updated!")
                 message(head(workspace$results))
                 
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
                 
                 output$results_table <- DT::renderDataTable(display_results,
                                                             options = list(pageLength = 100))
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
               })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
