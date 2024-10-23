#### Getting data from the LDC #################################################
getting_data_tutorial <- Cicerone$new(
  id = "getting_data_tutorial",
  allow_close = TRUE,
  overlay_click_next = FALSE
)$step(
  el = "data_type_container",
  title = "First select a data type",
  description = "This determines which kinds of indicators you can calculate. To start, select Line-Point Intercept.",
  position = "right"
)$step(
  el = "data_source",
  title = "Select the data source",
  description = "Select 'Query the Landscape Data Commons' in order to obtain data directly from the public database. Then click 'Next'."
)$step(
  el = "query_method_container",
  title = "Select how to request data",
  description = "To retrieve data from the Landscape Data Commons from a specific area, select 'Spatial'.",
  position = "right"
)$step(
  el = "polygon_source_ui",
  title = "Select the source of the area",
  description = "In order to get data for an area drawn on the map, select 'Drawn'. Then click 'Next'."
)$step(
  el = "drawing_map_ui",
  title = "Draw the area to retrieve data for",
  description = HTML(paste0("Select the square tool from the left side of the map, zoom into northern New Mexico, and draw a rectangle around the San Luis Valley.",
                            br(),
                            img(src = "san_luis_valley_map_example.png",
                                height = "50%",
                                display = "block",
                                align = "center",
                                hspace = "5px",
                                vspace = "5px"))),
  position = "left"
)$step(
  el = "fetch_button_container",
  title = "Fetch the data for the polygon",
  description = "Click 'Fetch data' in order to request the data from the Landscape Data Commons. It might take a bit, depending on the size of your polygon.",
  position = "right"
)$step(
  el = "data_type_info",
  title = "Getting help",
  description = HTML(paste0("These buttons will provide quick help. Additional help can be found in the ",
                            a("Help tab",
                              onclick = "tabJump('Help')"),
                            "."))
)

#### Configuring data ##########################################################
configuring_data_tutorial <- Cicerone$new(
  id = "configuring_data_tutorial",
  allow_close = TRUE,
  overlay_click_next = FALSE
)$step(
  # tab = "Variable configuration",
  # tab_id = "data_config_tabs",
  el = "variable_config_options",
  title = "Tell the tool how the data are structured",
  description = "The tool makes a guess as to how your data are structured, but needs a variable name for each of these options. These options draw from your data, so they can only be selected after you have data in the tool.",
  position = "right"
)$step(
  el = "primarykey_var_container",
  title = "Variable selection",
  description = "For example, your data must have a variable containing unique identifiers, called a PrimaryKey in the Landscape Data Commons. If your data's unique identifier variable isn't called 'PrimaryKey' you'll have to select the correct variable.",
  position = "right"
)$step(
  el = "primarykey_var_info",
  title = "Help",
  description = HTML(paste0("These buttons will provide quick help regarding what kind of information a variable must contain. Additional help can be found in the ",
                            a("Help tab",
                              onclick = "tabJump('Help')"),
                            "."))
)$step(
  el = "[data-value='Species information']",
  title = "Species characteristics",
  description = "If you want to calculate indicators using species characteristics like growth habit, you'll need to add those to your data with a lookup table.",
  is_id = FALSE
)

#### Species info ##############################################################
species_info_nodata_tutorial <- Cicerone$new(
  id = "species_info_nodata_tutorial",
  allow_close = TRUE,
  overlay_click_next = FALSE
)$step(
  el = "species_source",
  title = "Optional additional species information",
  description = "If you want to calculate indicators that use species characteristics like growth habit, you'll need to add them with a lookup table. Select 'Default USDA Plants' and click 'Next'."
)$step(
  el = "add_generic_species_container",
  title = "Generic species",
  description = "If the data contain generic species codes as defined in the Monitoring Manual, the tool can add them to the lookup table with their growth habit and duration information."
)$step(
  el = "growth_and_duration_var_container",
  title = "Selecting variables",
  description = "Generic species information can only be added to a lookup table that already contains variables for growth habit and duration."
)$step(
  el = "species_joining_var_container",
  title = "Selecting variables",
  description = "You need to tell the tool which variables in the data and in the current lookup table contain the matching species codes before you can add the information to the data."
)

species_info_tutorial <- Cicerone$new(
  id = "species_info_tutorial",
  allow_close = TRUE,
  overlay_click_next = FALSE
)$step(
  el = "species_source",
  title = "Optional additional species information",
  description = "If you want to calculate indicators that use species characteristics like growth habit, you'll need to add them with a lookup table. Select 'Default USDA Plants' and click 'Next'."
)$step(
  el = "add_generic_species_container",
  title = "Generic species",
  description = "If the data contain generic species codes as defined in the Monitoring Manual, the tool can add them to the lookup table with their growth habit and duration information."
)$step(
  el = "growth_and_duration_var_container",
  title = "Selecting variables",
  description = "Generic species information can only be added to a lookup table that already contains variables for growth habit and duration."
)$step(
  el = "species_joining_var_container",
  title = "Selecting variables",
  description = "You need to tell the tool which variables in the data and in the current lookup table contain the matching species codes before you can add the information to the data."
)

#### Calculation guides ########################################################
calculate_lpi_nodata_tutorial <- Cicerone$new(
  id = "calculate_lpi_nodata_tutorial",
  allow_close = TRUE,
  overlay_click_next = FALSE
)$step(
  el = "lpi_hit_container",
  title = "Calculation type",
  description = "This determines the type of cover being calculated. 'Any hit' and 'First hit' are flexible while the others are shortcuts for common use cases."
)$step(
  el = "lpi_grouping_vars_container",
  title = "Grouping variables",
  description = "This determines how to categorize the values in the data. Indicators will be calculated for unique combinations of the grouping variables. For example if your data contain duration and growth habit information and you select those both as grouping variables, your cover will be calculated for categories like 'annual graminoid'."
)$step(
  el = "lpi_unit_container",
  title = "Summary unit",
  description = "The indicators can either be calculated per-plot (i.e., sampling location) or per-transect within plots. Summarizing by transect is not typically recommended."
)$step(
  el = "lpi_output_format_container",
  title = "Output format",
  description = "The calculated indicators can be formatted in two ways: wide or tall. The calculated values will not differ between the two, but one may be more convenient for your workflow. Wide data will have one row per summary unit and one column per indicator while tall data will have one row per combination of summary unit and indicator."
)$step(
  el = "additional_output_vars_container",
  title = "Metadata variables",
  description = "By default, your results will only include the unique identifier variable you selected. To include additional variables from your data in your results (e.g., plot names), select them here."
)$step(
  el = "lpi_hit_info",
  title = "Help",
  description = HTML(paste0("These buttons will provide quick help regarding what these options mean. Additional help can be found in the ",
                            a("Help tab",
                              onclick = "tabJump('Help')"),
                            "."))
)

#### A full walkthrough on calculating cover by growth habit and duration ######
full_tutorial <- Cicerone$new(
  id = "full_tutorial",
  allow_close = TRUE,
  overlay_click_next = FALSE
)$step(
  el = "data_type_container",
  title = "First select a data type",
  description = "This determines which kinds of indicators you can calculate. To start, select Line-Point Intercept.",
  position = "right"#,
  # on_next = "Shiny.setInputValue('data_source', 'ldc')"
)$step(
  el = "data_source",
  title = "Select the data source",
  description = "Select 'Query the Landscape Data Commons' in order to obtain data directly from the public database. Then click 'Next'.",
  on_highlighted = "function(){Shiny.setInputValue('data_source', 'ldc')}"
)$step(
  el = "query_method_container",
  title = "Select how to request data",
  description = "To retrieve data from the Landscape Data Commons from a specific area, select 'Spatial'.",
  position = "right",
  on_next = "function(){Shiny.setInputValue('polygon_source', 'draw');}"
)$step(
  el = "polygon_source_ui",
  title = "Select the source of the area",
  description = "In order to get data for an area drawn on the map, select 'Drawn'. Then click 'Next'."
)$step(
  el = "drawing_map_ui",
  title = "Draw the area to retrieve data for",
  description = HTML(paste0("Select the square tool from the left side of the map, zoom into northern New Mexico, and draw a rectangle around the San Luis Valley.",
                            br(),
                            img(src = "san_luis_valley_map_example.png",
                                height = "50%",
                                display = "block",
                                align = "center",
                                hspace = "5px",
                                vspace = "5px"))),
  position = "left"
)$step(
  el = "fetch_button_container",
  title = "Fetch the data for the polygon",
  description = "Click 'Fetch data' in order to request the data from the Landscape Data Commons. It might take a bit, depending on the size of your polygon.",
  position = "right"
)$step(
  el = "fetch_and_busy_container",
  title = "Retrieving data",
  description = "While the tool is requesting and receiving data, a busy message will display. Please wait while it works and click 'Next' once it's finished.",
  position = "top-center"
# )$step(
#   el = "data_type_info",
#   title = "Getting help",
#   description = HTML(paste0("These buttons will provide quick help. Additional help can be found in the ",
#                             a("Help tab",
#                               onclick = "tabJump('Help')"),
#                             "."))
# )
)$step(
  el = "[data-value='Configure Data']",
  is_id = FALSE,
  title = "Configuring data",
  description = "The next step is to move to the Configure Data tab to make sure the tool can parse your data. Click 'Configure Data' to jump to the tab.",
  on_highlighted = "function(){Shiny.updateTab()}"
)$step(
  el = "variable_config_options",
  title = "Data structure",
  description = "The tool makes a guess as to how your data are structured, but needs a variable name for each of these options. These options draw from your data, so they can only be selected after you have data in the tool.",
  position = "right"
)$step(
  el = "primarykey_var_container",
  title = "Variable selection",
  description = "For example, your data must have a variable containing unique identifiers, called a PrimaryKey in the Landscape Data Commons. If your data's unique identifier variable isn't called 'PrimaryKey' you'll have to select the correct variable.",
  position = "right"
)$step(
  el = "[data-value='Species information']",
  title = "Species characteristics",
  description = "If you want to calculate indicators using species characteristics like growth habit, you'll need to add those to your data with a lookup table. Click 'Species Information' to continue with that process.",
  is_id = FALSE
)$step(
  el = "species_source",
  title = "Species information source",
  description = "You can use the included lookup table derived from the USDA Plants database or upload a custom one. Select 'Default USDA Plants' and click 'Next'."
)$step(
  el = "add_generic_species_container",
  title = "Generic species",
  description = "Data may contain specially formatted generic species codes for plants which weren't identified to species. This option makes sure that they are included in the lookup table."
)$step(
  el = "growth_and_duration_container",
  title = "Generic species traits",
  description = "When adding generic species codes found in the data to the lookup table, you must specify which variables contain the growth habit and duration information in the lookup table."
)$step(
  el = "species_joining_var_container",
  title = "Species code variables",
  description = "In order to add species information, the tool needs to know which variable contains species codes in both your data and lookup table."
)$step(
  el = "join_species",
  title = "Adding information",
  description = "Once everything is configured, you can add the information from the lookup table to the data. Click 'Join species information to data' and then 'Next'."
)$step(
  el = "downloadable_species",
  title = "Downloading the lookup table",
  description = "You can download the current lookup table (including any added generic species codes) to archive or, if there are missing codes, to modify and use as an uploaded lookup table."
)$step(
  el = "[data-value='Calculate Indicators']",
  title = "Calculating indicators",
  description = "Now that the data are fully configured, it's time to move on to calculating the indicators. Click 'Calculate Indicators' to jump to the tab then click 'Next'.",
  is_id = FALSE
)$step(
  el = "calculation_config_container",
  title = "Calculation options",
  description = "In order to get the indicators you want, you'll need to tell the tool how to go about using the data.",
  is_id = FALSE
)$step(
  el = "lpi_hit_container",
  title = "Indicator type",
  description = "There are a few general ways to calculate cover and a number of specialized options; 'Any hit' will use data from every layer and not just the first. Select 'Any hit' and click 'Next'."
)