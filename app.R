library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
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
             windowTitle = "Terradactyl Indicator Calculator"),
  titlePanel(title = "Terradactyl Indicator Calculator"),
  
  ##### Sidebar #####
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "data_type",
                  label = "Data type",
                  choices = c("Line-Point Intercept" = "lpi",
                              "Height" = "height",
                              "Gap" = "gap",
                              "Soil Stability" = "soil"),
                  selected = "lpi"),
      radioButtons(inputId = "data_source",
                   label = "Data source",
                   choices = c("Upload" = "upload",
                               "Query Landscape Data Commons" = "ldc")),
      conditionalPanel(condition = "input.data_source == 'upload'",
                       fileInput(inputId = "raw_data",
                                 label = "Data CSV",
                                 multiple = FALSE,
                                 accept = "CSV"),
                       checkboxInput(inputId = "needs_header",
                                     label = "Need to upload header information",
                                     value = FALSE),
                       conditionalPanel(condition = "input.needs_header",
                                        fileInput(inputId = "header_data",
                                                  label = "Header CSV",
                                                  multiple = FALSE,
                                                  accept = "CSV")
                       )
      ),
      
      # conditionalPanel(condition = "input.data_source == 'upload'",
      #                  
      #                  ),
      conditionalPanel(condition = "input.data_source == 'ldc'",
                       selectInput(inputId = "key_type",
                                   label = "LDC search type",
                                   choices = c("By ecological site" = "ecosite",
                                               "By primarykey" = "primarykey",
                                               "By projectkey" = "projectkey"),
                                   selected = "ecosite"),
                       textInput(inputId = "keys",
                                 label = "Search values",
                                 value = "",
                                 placeholder = "R042XB012NM"),
                       actionButton(inputId = "fetch_data",
                                    label = "Fetch data")),
      radioButtons(inputId = "species_source",
                   label = "Species list source",
                   choices = c("Default USDA Plants" = "default",
                               "Upload" = "upload"),
                   selected = 0),
      conditionalPanel(condition = "input.species_source == 'upload'",
                       fileInput(inputId = "species_data",
                                 label = "Species CSV",
                                 multiple = FALSE,
                                 accept = "CSV")),
      
      hr()
    ),
    
    
    ##### Tabs #####
    mainPanel(
      tabsetPanel(type = "tabs",
                  id = "maintabs",
                  tabPanel(title = "Data configuration",
                           selectInput(inputId = "primarykey_var",
                                       label = "Variable containing primary key values",
                                       choices = c("")),
                           conditionalPanel(condition = "input.data_type == 'gap' || input.lpi_unit == 'line' || input.height_unit == 'line'",
                                            selectInput(inputId = "linekey_var",
                                                        label = "Variable containing line key values",
                                                        choices = c("")))
                           
                  ),
                  tabPanel(title = "Indicator calculation",
                           actionLink(inputId = "help",
                                      label = "What do these options mean?"),
                           conditionalPanel(condition = "input.data_type == 'lpi'",
                                            radioButtons(inputId = "lpi_hit",
                                                         label = "Use first hit or any hit?",
                                                         choices = c("Any hit" = "any",
                                                                     "First hit" = "first")),
                                            selectInput(inputId = "lpi_grouping_vars",
                                                        label = "Variables to group by",
                                                        multiple = TRUE,
                                                        choices = c("")),
                                            selectInput(inputId = "lpi_unit",
                                                        label = "Summarization unit",
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
                                                        label = "Summarization unit",
                                                        choices = c("Plot" = "plot",
                                                                    "Line" = "line"),
                                                        selected = "plot"),
                                            selectInput(inputId = "gap_output_format",
                                                        label = "Output format",
                                                        choices = c("Wide" = "wide",
                                                                    "Long" = "long"))
                           ),
                           conditionalPanel(condition = "input.data_type == 'height'",
                                            selectInput(inputId = "height_stat",
                                                        label = "Statistic",
                                                        choices = c("Mean" = "mean",
                                                                    "Maximum" = "max"),
                                                        selected = "mean"),
                                            checkboxInput(inputId = "height_omit_zero",
                                                          label = "Omit heights of 0 from calculation"),
                                            selectInput(inputId = "height_grouping_vars",
                                                        label = "Variables to group by",
                                                        multiple = TRUE,
                                                        choices = c("")),
                                            selectInput(inputId = "height_unit",
                                                        label = "Summarization unit",
                                                        choices = c("Plot" = "plot",
                                                                    "Line" = "line"),
                                                        selected = "plot"),
                                            selectInput(inputId = "height_output_format",
                                                        label = "Output format",
                                                        choices = c("Wide" = "wide",
                                                                    "Long" = "long"))
                           ),
                           conditionalPanel(condition = "input.data_type == 'soil'",
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
                  tabPanel(title = "Results",
                           conditionalPanel(condition = "output.results_table !== null",
                                            downloadButton(outputId = 'downloadable_data',
                                                           label = 'Download results')),
                           DT::dataTableOutput(outputId = "results_table")),
                  hr(),
                  tabPanel(title = "Help")
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
  observeEvent(eventExpr = input$help,
               handlerExpr = {
                 updateTabsetPanel(session = session,
                                   inputId = "maintabs",
                                   selected = "Help")
               })
  
  
  ##### CSV upload handling #####
  # When input$raw_data updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$raw_data,
               handlerExpr = {
                 message("Reading in raw data from uploaded CSV")
                 workspace[["raw_data"]] <- read.csv(input$raw_data$datapath,
                                                     stringsAsFactors = FALSE)
                 
                 # Set this variable so we can handle the data appropriately based on source
                 # Since these are uploaded data, we're not looking for header info
                 message("Data source set to upload")
                 workspace$current_data_source <- "upload"
               })
  
  # When input$species_data updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$header_data,
               handlerExpr = {
                 message("Reading in header info from uploaded CSV")
                 workspace[["headers"]] <- read.csv(input$header_data$datapath,
                                                    stringsAsFactors = FALSE)
               })
  
  # When input$species_data updates, look at its filepath and read in the CSV
  observeEvent(eventExpr = input$species_data,
               handlerExpr = {
                 message("Reading in species data from uploaded CSV")
                 workspace[["species_data"]] <- read.csv(input$species_data$datapath,
                                                         stringsAsFactors = FALSE)
                 
                 # Set this variable so we can handle the data appropriately based on source
                 message("Species source set to upload")
                 workspace$current_species_source <- "upload"
               })
  
  # When input$species_source is set to the default, handle that
  observeEvent(eventExpr = input$species_source,
               handlerExpr = {
                 if (input$species_source == "default") {
                   message("Species source set to default")
                   # TODO
                   # Set workspace$species_data to the default list (which doesn't exist yet)
                   # Set this variable so we can handle the data appropriately based on source
                   workspace$current_species_source <- "default"
                 }
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
                 
                 # Update the data format since we know these're long
                 # updateRadioButtons(session = session,
                 #                    inputId = "data_format",
                 #                    selected = "long")
                 
                 
                 
                 # Only do anything if there's an ecosite ID
                 if (input$keys != "") {
                   # Handle multiple requested ecosites at once!
                   current_key_vector <- stringr::str_split(string = input$keys,
                                                           pattern = ",",
                                                           simplify = TRUE)
                   current_key_vector <- trimws(current_key_vector)
                   
                   results <- fetch_ldc(keys = current_key_vector,
                                        key_type = input$key_type,
                                        data = input$data_type,
                                        verbose = TRUE)
                   
                   # So we can tell the user later which actually got queried
                   if (is.null(results)) {
                     message("No data from LDC!")
                     workspace$missing_keys <- current_key_vector
                   } else {
                     message("Determining if ecosites are missing.")
                     key_var <- switch(input$key_type,
                                       "ecosite" = {"EcologicalSiteId"},
                                       "primarykey" = {"PrimaryKey"},
                                       "projectkey" = {"ProjectKey"})
                     
                     workspace$queried_keys <- unique(results[[key_var]])
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
                 } else if (workspace$current_data_source == "upload") {
                   # If the data are uploaded, join to the header info first
                   if (!is.null(workspace$headers) & input$needs_header) {
                     # Get the number of observations so we can warn if we lose some
                     n_observations <- nrow(workspace$raw_data)
                     current_data <- dplyr::left_join(x = workspace$raw_data,
                                                      y = workspace$headers)
                     current_n_observations <- nrow(current_data)
                     # Warn the user if not all observations made it through the
                     # joining process
                     if (n_observations != current_n_observations) {
                       showNotification(ui = paste0("Joining the headers to the data results in the loss of ",
                                                    n_observations - current_n_observations,
                                                    " observations."),
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "dropped_data",
                                        type = "warning")
                     }
                     workspace$data <- current_data
                   } else if (is.null(workspace$headers) & input$needs_header) {
                     message("Needs header info, but the user has not supplied it")
                     workspace$data <- NULL
                     showNotification(ui = "Please supply header information.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      id = "needs_headers_upload",
                                      type = "warning")
                   } else if (!input$needs_header) {
                     message("No headers needed. Writing workspace$raw_data to workspace$data")
                     workspace$data <- workspace$raw_data
                   }
                 }
               })
  
  ##### When headers update #####
  observeEvent(eventExpr = workspace$headers,
               handlerExpr = {
                 if (workspace$current_data_source == "ldc") {
                   message("Data are from the LDC and don't actually need header info. Ignoring workspace$headers.")
                   showNotification(ui = "Data are from the LDC and the uploaded headers will be ignored because header information is already included with the data.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "unnecessary_headers",
                                    type = "message")
                 } else if (workspace$current_data_source == "upload") {
                   if (!is.null(workspace$raw_data)) {
                     # Get the number of observations so we can warn if we lose some
                     n_observations <- nrow(workspace$raw_data)
                     message("Joining data and headers.")
                     current_data <- dplyr::left_join(x = workspace$raw_data,
                                                      y = workspace$headers)
                     current_n_observations <- nrow(current_data)
                     if (n_observations != current_n_observations) {
                       showNotification(ui = paste0("Joining the headers to the data results in the loss of ",
                                                    n_observations - current_n_observations,
                                                    " observations."),
                                        duration = NULL,
                                        closeButton = TRUE,
                                        id = "dropped_data",
                                        type = "warning")
                     }
                     message("Updating workspace$data with the joined data")
                     workspace$data <- current_data
                   }
                 }
               })
  
  ##### When workspace$data updates #####
  observeEvent(eventExpr = workspace$data,
               handlerExpr = {
                 if (is.null(workspace$data)) {
                   # If the data aren't ready, there can't be variables selected
                   message("workspace$data is NULL.")
                   message("Updating key variable selectInput()s.")
                   updateSelectInput(inputId = "primarykey_var",
                                     choices = c(""),
                                     selected = "")
                   updateSelectInput(inputId = "linekey_var",
                                     choices = c(""),
                                     selected = "")
                   if (input$data_type %in% c("lpi", "height")) {
                     updateSelectInput(inputId = paste0(input$data_type,
                                                        "_grouping_vars"),
                                       choices = c(""),
                                       selected = "")
                   }
                   
                 } else {
                   message("workspace$data contains data.")
                   message("Updating key variable selectInput()s.")
                   # Update the variable options
                   current_data_vars <- names(workspace$data)
                   
                   updateSelectInput(inputId = "primarykey_var",
                                     choices = current_data_vars,
                                     selected = "")
                   updateSelectInput(inputId = "linekey_var",
                                     choices = current_data_vars,
                                     selected = "")
                   
                   if ("PrimaryKey" %in% current_data_vars) {
                     updateSelectInput(inputId = "primarykey_var",
                                       selected = "PrimaryKey")
                   }
                   if ("LineKey" %in% current_data_vars) {
                     updateSelectInput(inputId = "linekey_var",
                                       selected = "LineKey")
                   }
                   
                   if (input$data_type %in% c("lpi", "height")) {
                     updateSelectInput(inputId = paste0(input$data_type,
                                                        "_grouping_vars"),
                                       choices = current_data_vars,
                                       selected = "")
                   }
                   
                   # DATA SOUNDNESS CHECKS
                   # We need the variables required for the current data type
                   # Add in "LineKey" if it's called for
                   message("Checking for required variables.")
                   needs_linekey <- (input$data_type == "lpi" & input$lpi_unit == "line") | (input$data_type == "gap" & input$gap_unit == "line") | (input$data_type == "height" & input$height_unit == "line")
                   if (needs_linekey) {
                     message("Including 'LineKey' in required variables.")
                     current_required_vars <- c(workspace$required_vars[[input$data_type]],
                                                "LineKey")
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
               })
  
  ##### When input$primarykey_var changes #####
  observeEvent(eventExpr = input$primarykey_var,
               handlerExpr = {
                 if (input$primarykey_var != "") {
                   message(paste0("Updating workspace$data$PrimaryKey with values from workspace$data$",
                                  input$primarykey_var))
                   workspace$data[["PrimaryKey"]] <- workspace$data[[input$primarykey_var]]
                 } else {
                   message("No PrimaryKey variable provided (yet).")
                 }
               })
  
  ##### When input$linekey_var changes #####
  observeEvent(eventExpr = input$linekey_var,
               handlerExpr = {
                 if (input$primarykey_var != "") {
                   message(paste0("Updating workspace$data$LineKey with values from workspace$data$",
                                  input$linekey_var))
                   workspace$data[["LineKey"]] <- workspace$data[[input$linekey_var]]
                 } else {
                   message("No LineKey variable provided (yet).")
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
                                                            "method = input$height_stat,",
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
                                                            "method = input$height_stat,",
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
                                                          "method = input$height_stat,",
                                                          "omit_zero = input$height_omit_zero,",
                                                          "by_line = height_by_line,",
                                                          "tall = output_tall",
                                                          ")"
                            )
                          }
                          message("The function call is:")
                          message(height_cover_string)
                          workspace$results <- eval(parse(text = height_cover_string))
                          
                          # # Remove that hacky temp_grouping_var
                          # message("Removing temp_grouping_var from workspace$results")
                          # workspace$results <- dplyr::select(workspace$results,
                          #                                    -temp_grouping_var)
                        },
                        "soil" = {
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
                 removeNotification(session = session,
                                    id = "calculating")
               })
  
  ##### When results update #####
  observeEvent(eventExpr = workspace$results,
               handlerExpr = {
                 message("The results have updated!")
                 message(head(workspace$results))
                 output$results_table <- DT::renderDataTable(workspace$results)
                 message("output$results_table rendered")
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
