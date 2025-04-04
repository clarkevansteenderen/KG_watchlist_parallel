options(repos = c(CRAN = "https://cloud.r-project.org/"))

# load these packages locally the first time, then comment out when uploading to the shiny apps server
# mypackages <- c("shiny", "shinyhelper", "magrittr", "shinyFiles", "svglite", "shinyalert", "tidyverse",
#                 "MASS", "cowplot", "purrr", "shinybusy", "janitor", "sn",
#                 "raster", "DHARMa", "TMB", "ggeffects", "plotly", "DT", "rnaturalearthdata", "terra", "rnaturalearth")
# 
# checkpkg <- mypackages[!(mypackages %in% installed.packages()[,"Package"])]
# if(length(checkpkg)) install.packages(checkpkg, dependencies = TRUE)

library(tidyverse)
library(rlang)
library(cowplot)
library(svglite)
library(shinyhelper)
library(shiny)
library(magrittr)
library(shinyFiles)
library(shinybusy)
library(janitor)
library(sn)
library(shinyalert)
library(raster)
library(DHARMa)
library(TMB)
library(ggeffects)
library(plotly)
library(DT)
library(stringdist)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)

ggthemes = list("Classic" = ggplot2::theme_classic(),
                "Dark" = ggplot2::theme_dark(),
                "Minimal" = ggplot2::theme_minimal(),
                "Grey" = ggplot2::theme_grey(),
                "Light" = ggplot2::theme_light(),
                "Black/White" = ggplot2::theme_bw(),
                "Void" = ggplot2::theme_void())

iso_data = read.csv("iso_codes/iso_codes.csv", fileEncoding = "ISO-8859-1")
iso_countries = iso_data$Name

############################################################################################################

shinyUI(fluidPage(
  #add_busy_spinner(spin = "fading-circle"),
  #img(src='coldbug.jpg',  height = '250px', width = '200px', style = 'float:right;'),
  
  tags$style(HTML("
                  .tabbable > .nav > li > a                  {font-weight: bold; background-color: lightsteelblue;  color:black}
                  .tabbable > .nav > li[class=active]    > a {background-color: lightgreen; color:black}
                  ")),
  
  # Application title
  titlePanel(h1(strong("WatchListR",style = "color:black; font-size:85%"))),
  titlePanel(h3(strong("Create invasive species watchlists",style = "color:black; font-size:85%"))),
  br(), br(),
  # sidebarLayout(
  # 
  #     sidebarPanel(
  #         strong("Click the i symbol to view the help file:", style = "color:darkblue; font-size:120%")
  #         %>% helper(type = "markdown", content = "thermalsampler_manual", colour = "darkblue", icon = "info-circle")
  #         
  #         
  #     ),
  
  mainPanel(width = 12,
            tabsetPanel(
              
              ############################################################################################################
              # HOME PANEL
              ############################################################################################################
              tabPanel(strong(icon("house", "fa-solid", style = "color: black;", size = "1x"), "HOME"),
                       br(), br(),
                                tags$head(
                                  tags$style(HTML("
      /* Glowing effect for file input button */
      .glow-file-input {
        animation: glow 1s infinite alternate;
      }

      @keyframes glow {
        from {
          box-shadow: 0 0 10px lightgreen;
        }
        to {
          box-shadow: 0 0 20px lightgreen;
        }
      }
    ")),
                                  
                                  # JavaScript to remove glow effect when a file is uploaded
                                  tags$script(HTML("
      Shiny.addCustomMessageHandler('removeGlow', function(id) {
        document.getElementById(id).classList.remove('glow-file-input');
      });
    "))
                                ),
                                
                                div(id = "file-container", class = "glow-file-input",
                                    fileInput("inFile", "UPLOAD THE FINAL WATCHLIST FILE", accept = c(".csv"))
                                ),
                                htmlOutput("spp_num"), tags$head(tags$style("#spp_num{color: forestgreen; font-size: 16px; font-weight: bold;}")),
                                htmlOutput("spp_incountry"), tags$head(tags$style("#spp_incountry{color: forestgreen; font-size: 16px; font-weight: bold;}")),
                       br(), br(), br(),
                       div( img(src='watchlistr_logo.png',  height = '500px', width = '450px'), style="display: block; margin-left: auto; margin-right: auto; text-align: center;" ),
                       br(), br(),
                       wellPanel(
                         h3(strong("ABOUT")),
                         br(),
                         h5(strong("Navigate using the tabs at the top of the window."), br(),
                            h5(strong("")),
                            #HTML("<p align = 'left'><img src = 'GitHub.png' width = '20px' height = 'auto'> <a target='_blank' rel='noopener noreferrer' href='https://github.com/clarkevansteenderen/biostatR'> GitHub Link </a></p>"),
                            h5(strong("")), br(),
                         ), # end of wellpanel
                       ),
                       
              ),
              
              ############################################################################################################
              # CREATE INPUT PANEL
              ############################################################################################################
              
              tabPanel(strong(icon("seedling", "fa-solid", style = "color: black;", size = "1x"), "CREATE INPUT FILE"),
                       br(), br(),
                       tags$div(
                         style = "background-color: lightsteelblue; padding: 10px;",
                         tags$strong(icon("lightbulb", "fa-solid", style = "color: yellow;", size = "1x"),
                                     "Explanation here",
                                     style = "color: black;")
                       ),
                       br(), br(),
                       fluidRow(
                         column(3,
                                
                                h4(icon("circle", "fa-solid", style = "color: royalblue;", size = "2x"), strong("1. Project Details")),
                                br(),
                                h4(icon("circle", style = "color: royalblue;", size = "1x"), strong("Country")),
                                selectInput("target_country", "Select the target country:", choices = iso_countries, selected = ""),
                                br(),
                                h4(icon("circle", style = "color: royalblue;", size = "1x"), strong("Kingdom")),
                                selectInput("kingdom", "Select the kingdom:", choices = c("Animalia", "Bacteria", "Chromista", "Fungi", "Plantae", "Viruses"), selected = ""),
                                br(),
                                h4(icon("circle", style = "color: royalblue;", size = "1x"), strong("High-risk Countries")),
                                h5(strong("E.g. prominent trading partners")),
                                selectInput("high_risk_countries", "Select high risk countries:", choices = iso_countries, selected = "", multiple = TRUE),
                                br(),
                                h4(icon("circle", style = "color: royalblue;", size = "1x"), strong("Species Files to remove")),
                                h5(strong("E.g. endemics, pests, agricultural crops")),
                                numericInput("num_files", "Number of files to upload:", 1, min = 1, max = 15),
                                uiOutput("file_inputs"),  # Placeholder for dynamically generated file inputs
                                uiOutput("column_selects"),  # UI placeholder for column dropdowns
                                br(),
                                textInput("combo_sp_file_name", "File Name for Combined List:", value = "exclude/combined_species_to_remove"),
                                br(),
                                downloadButton("download_combined_files", "Combine and Download", style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                                br(), br(),
                                h4(icon("circle", style = "color: royalblue;", size = "1x"), strong("List of GRIIS species to check through")),
                                h5(strong("Global Register of Introduced and Invasive Species")),
                                textInput("species_file", "Enter the path to the GRISS list:", value = "griis_data/griis_full_database.csv"),
                                br(),
                                textInput("species_nameCol", "Enter the species name column:", value = "accepted_name.species"),
                                br(),
                         ),
                         column(3,
                                h4(icon("circle", "fa-solid", style = "color: darkorange;", size = "2x"), strong("2. GBIF details")),
                                br(), 
                                textInput("gbif_username", "Enter your GBIF username:"),
                                br(),
                                textInput("gbif_email", "Enter your GBIF email:"),
                                br(),
                                textInput("gbif_password", "Enter your GBIF password:"),
                                br()
                         ),
                         # column(3,
                         #        ),
                         column(3,
                                h4(icon("circle", "fa-solid", style = "color: forestgreen;"), strong("3. Click the Create button when ready")),
                                br(), 
                                actionButton("go", strong("Create"), style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                                br(), br(), br(),
                                tags$head(tags$style("#confirm_file_info { 
                                font-size: 16px; 
                                width: 600px; 
                                height: 350px; 
                                overflow-y: auto; 
                                border: 1px solid #ccc;
                            }")),
                                verbatimTextOutput("confirm_file_info"),
                                br(), br(),
                                downloadButton("download_inputfile", label = "Download File",
                                               style="color: #fff; background-color: darkblue; border-color: white; font-size:120%")
                         ),
                       )#fluidrow
              ),
              
              ############################################################################################################
              # KOPPEN-GEIGER MAP PANEL
              ############################################################################################################  
              
              tabPanel(
                strong(icon("map", "fa-solid", style = "color: black;", size = "1x"), "KOPPEN-GEIGER MAP"),
                br(), br(),
                tags$div(
                  style = "background-color: lightsteelblue; padding: 10px;",
                  tags$strong(icon("lightbulb", "fa-solid", style = "color: yellow;", size = "1x"),
                              "Explanation here",
                              style = "color: black;")
                ),
                br(), br(),
                fluidRow(
                  column(3,
                         h4(icon("circle", "fa-solid", style = "color: royalblue;", size = "2x"), strong("KG Map")),
                         br(),
                         selectInput("target_country_KG_map", "Select the target country:", choices = iso_countries, selected = ""),
                         selectInput("ggtheme_KGmap", "Select ggplot Theme:", choices = names(ggthemes), selected = ggthemes["Classic"]),
                         br(),
                         actionButton("plotKGmap", "Plot KG map", style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                         br(), br(), br(), br(),
                         plotOutput("plot_KG", width = 700, height = 450)
                  ),
                  column(3,
                         numericInput("KG_xmin", "Min. longitude", value = NA),
                         numericInput("KG_xmax", "Max. longitude", value = NA),
                         textInput("KGmap_title", "Map title", value = ""),
                         sliderInput("KG_title_size", "Title size", min = 0, max = 20, value = 12),
                         br(), br(),
                         
                  ),
                  column(3,
                         numericInput("KG_ymin", "Min. latitude", value = NA),
                         numericInput("KG_ymax", "Max. latitude", value = NA),
                         sliderInput("KG_labsize", "Axis label size", min = 0, max = 20, value = 12),
                         sliderInput("KG_axisvaluesize", "Axis value size", min = 0, max = 20, value = 12),
                         br(), br(),
                  ),
                  column(3,
                         numericInput("label_KG_orient", "x-axis label orientation:", min = -360, max = 360, value = 0, width = "50%")
                  )
                ),  # Closing fluidRow()
                
                br(), br(),
                
                wellPanel(
                  textInput("file_name_mortality_plot", "File name: ", "koppen_geiger_map"),
                  
                  fluidRow(
                    column(width = 2,
                           selectInput("plot_format_KGmap", "Image format:", choices = c("pdf", "png", "svg"), width = "150px")
                    ),
                    column(width = 2,
                           textInput("w_KGmap_plot", "Width: ", 20, width = "150px")
                    ),
                    column(width = 2,
                           textInput("h_KGmap_plot", "Height: ", 15, width = "150px")
                    ),
                    column(width = 2,
                           selectInput("unit_KGmap_plot", "Unit: ", choices=c("cm", "in"), width = "150px")
                    ),
                    column(width = 2,
                           conditionalPanel(
                             condition = "input.plot_format_KGmap == 'png'",
                             textInput("res_KGmap_plot", "Res (dpi): ", 300)
                           )
                    )
                  ),  # Closing fluidRow()
                  
                  downloadButton("downloadplot_KGmap", "Download Plot", style="color: #fff; background-color: darkblue; border-color: white; font-size:120%"),
                  br(), br(),
                ), # wellpanel
              ),  # Closing tabPanel()
              
              ############################################################################################################
              # VISUALISE FINAL WATCHLISTR OUTPUT PANEL: KG ZONES FOR SPECIES
              ############################################################################################################
              
              tabPanel(strong(icon("eye", "fa-solid", style = "color: black;", size = "1x"), "VISUALISE OUTPUT"),
                       br(), br(),
                       tags$div(
                         style = "background-color: lightsteelblue; padding: 10px;",
                         tags$strong(icon("lightbulb", "fa-solid", style = "color: yellow;", size = "1x"),
                                     "Explanation here",
                                     style = "color: black;")
                       ),
                       br(), br(),
                       
                       fluidRow(
                         column(3,
                                h4(icon("circle", "fa-solid", style = "color: darkorange;", size = "2x"), strong("1. Select columns")),
                                br(), 
                                selectizeInput("col.species", "Select species (showing first 500):", 
                                               choices = NULL,  # No species are preloaded
                                               multiple = TRUE,  
                                               options = list(
                                                 placeholder = "Type to search species...",  # Helps users understand
                                                 maxOptions = 500,  # Limits how many are displayed at once
                                                 hideSelected = TRUE
                                               )),
                                selectInput("col.metrics", "Select metric(s) to plot:", choices = NULL, multiple = TRUE),
                                selectInput("spp_colours", "Select colours:", choices = c(colors()), multiple = TRUE),
                                sliderInput("spp_alpha", "Colour Transparency:", min = 0, max = 1, value = 0.65, width = "50%"),
                                checkboxInput("spp_add_border", "Add borders around bars?", value = FALSE)
                         ),
                         
                         column(3,
                                h4(icon("circle", "fa-solid", style = "color: darkgreen;", size = "2x"), strong("2. Customise")),
                                br(), 
                                
                                selectInput("spp_ggtheme", "Select ggplot Theme:", choices = names(ggthemes), selected = ggthemes["Classic"]),
                                selectInput("spp_legend_pos", "Legend Position:", choices = c("top", "right", "bottom", "left", "none"), selected = "top", width = "50%"),
                                selectInput("spp_ggtheme", "Select ggplot Theme:", choices = names(ggthemes), selected = ggthemes["Classic"], width = "50%"),
                                
                                br(),
                                
                         ), #column 4
                         column(3,
                                textInput("spp_xlab", "x-axis label:", value = "Species"),
                                textInput("spp_ylab", "y-axis label:", value = "Number"),
                                numericInput("spp_label_orient", "x-axis label orientation:", min = -360, max = 360, value = 0, width = "50%"),
                                numericInput("ymaxrange", "Maximum y-value", min = 0, max = Inf, value = NA)
                                
                         ), # column 4
                         
                         column(3,
                                #h4(icon("circle", "fa-solid", style = "color: darkred;", size = "2x"), strong("Species summaries")),
                                br(),
                                
                                actionButton("plot_spp_summaries", strong("PLOT"), style="color: #fff; background-color: darkblue; border-color: white; font-size:120%",  icon("check")),
                         ), #column 3
                         
                       ), # fluidrow
                       
                       plotOutput("watchlist_plot", width = 700, height = 450), 
                       br(),br(),
                       
                       # plot download options
                       h3(strong("DOWNLOAD")),
                       wellPanel(
                         
                         textInput("file_name_spp_plot", "File name: ", "spp_plot"),
                         
                         fluidRow(
                           column(width = 2,
                                  selectInput("plot_format_spp", "Image format:", choices = c("pdf", "png", "svg"), width = "150px"),
                           ),
                           column(width = 2,
                                  textInput("w_spp_plot", "Width: ", 20, width = "150px"),
                           ),
                           column(width = 2,
                                  textInput("h_spp_plot", "Height: ", 15, width = "150px"),
                           ),
                           column(width = 2,
                                  selectInput("unit_spp_plot", "Unit: ", choices=c("cm", "in"), width = "150px"),
                           ),
                           column(width = 2,
                                  conditionalPanel(
                                    condition = "input.plot_format_spp == 'png'",
                                    textInput("res_spp_plot", "Res (dpi): ", 300), width = "150px")
                           ),
                         ),
                         
                         downloadButton("downloadplot_spp", "Download Plot", style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                         #downloadButton("download_mortality", "Download Summary Table", style="color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                       ), # end of wellpanel
              ), #end tabPanel
              
              ############################################################################################################
              # VISUALISE SUMMARIES TAB PANEL
              ############################################################################################################
              
              tabPanel(
                strong(icon("leaf", class = "fa-solid", style = "color: black;"), "SUMMARIES"),
                br(), br(),
                
                # Explanation Box
                tags$div(
                  style = "background-color: lightsteelblue; padding: 10px;",
                  tags$strong(
                    icon("lightbulb", class = "fa-solid", style = "color: yellow;"),
                    " Explanation here",
                    style = "color: black;"
                  )
                ),
                br(), br(),
                
                # Input Fields
                fluidRow(column(3,
                                numericInput("KG_thresh", "Threshold for the proportion of species records in the target country's Koppen-Geiger climate zones", min = 0, max = 100, value = 95),
                                numericInput("num_occurences", "Minimum number of species occurrence records", min = 0, max = Inf, value = 50),
                                checkboxInput("summary_add_border", "Add borders around bars?", value = FALSE)
                ),
                column(3,
                       #selectInput("summary_colours", "Select colours:", choices = colors(), multiple = TRUE),
                       sliderInput("summary_alpha", "Colour Transparency:", min = 0, max = 1, value = 0.65, width = "50%"),
                       selectInput("summary_ggtheme", "Select ggplot Theme:", choices = names(ggthemes), selected = ggthemes["Classic"]),
                       selectInput("summary_legend_pos", "Legend Position:", choices = c("top", "right", "bottom", "left", "none"), selected = "top", width = "50%")
                ),
                column(3,
                       textInput("summary_xlab", "x-axis label:", value = "Species"),
                       textInput("summary_ylab", "y-axis label:", value = "Number"),
                       #checkboxInput("summary_add_border", "Add borders around bars?", value = TRUE),
                       numericInput("summary_label_orient", "x-axis label orientation:", min = -360, max = 360, value = 0, width = "50%")
                ),
                column(3,
                       actionButton("plot_thresholded_spp", "PLOT", style="color: #fff; background-color: darkblue; border-color: white; font-size:120%",  icon("check")),
                ),
                ), # End fluidRow
                br(),br(),
                plotOutput("summary_plot", width = 800, height = 550), 
                br(),br(),
                
                # Plot download options
                h3(strong("DOWNLOAD")),
                wellPanel(
                  
                  textInput("file_name_summary_plot", "File name: ", "summary_plot"),
                  
                  fluidRow(
                    column(width = 2,
                           selectInput("plot_format_summary", "Image format:", choices = c("pdf", "png", "svg"), width = "150px")
                    ),
                    column(width = 2,
                           textInput("w_summary_plot", "Width: ", 20, width = "150px")
                    ),
                    column(width = 2,
                           textInput("h_summary_plot", "Height: ", 15, width = "150px")
                    ),
                    column(width = 2,
                           selectInput("unit_summary_plot", "Unit: ", choices = c("cm", "in"), width = "150px")
                    ),
                    column(width = 2,
                           conditionalPanel(
                             condition = "input.plot_format_summary == 'png'",
                             textInput("res_summary_plot", "Res (dpi): ", 300, width = "150px")
                           )
                    )
                  ),
                  
                  fluidRow(
                    column(width = 2,
                           downloadButton("downloadplot_summary", "Download Plot", style = "color: black; background-color: lightgrey; border-color: black; font-size:100%"),
                    ),
                    column(width = 2,
                           downloadButton("download_summary_data", "Download Threshold Data", style = "color: black; background-color: lightgrey; border-color: black; font-size:100%")
                    )
                  )#fluidRow
                )
                
              ) # End tabPanel
              
            ))
  
))#shinyUI