
ggthemes = list("Classic" = theme_classic(),
                "Dark" = theme_dark(),
                "Minimal" = theme_minimal(),
                "Grey" = theme_grey(),
                "Light" = theme_light(),
                "Black/White" = theme_bw(),
                "Void" = theme_void())

# Change ggplot theme
theme_set(
  theme_classic() +
    theme(
      panel.border = element_rect(colour = "black",
                                  fill = NA),
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(margin = unit(c(2, 0, 0, 0),
                                                "mm")),
      axis.title.y = element_text(margin = unit(c(0, 4, 0, 0),
                                                "mm")),
      legend.position = "none"
    )
)

# Define a custom color palette for Köppen-Geiger zones (adjust as needed)
climate_colors <- c(
  "Af (1)" = "#1f78b4",  # Blue
  "Am (2)" = "#33a02c",  # Green
  "Aw (3)" = "#e31a1c",  # Red
  "BWh (4)" = "#ff7f00",  # Orange
  "BWk (5)" = "#6a3d9a",  # Purple
  "BSh (6)" = "#b15928",  # Brown
  "BSk (7)" = "#cab2d6",  # Light Purple
  "Csa (8)" = "#ffff99",  # Yellow
  "Csb (9)" = "#a6cee3",  # Light Blue
  "Csc (10)" = "#8c8c8c",  # Gray
  "Cwa (11)" = "#fb9a99",  # Pink
  "Cwb (12)" = "#fdbf6f",  # Peach
  "Cwc (13)" = "#9c755f",  # Coffee
  "Cfa (14)" = "#66c2a5",  # Teal
  "Cfb (15)" = "#b2df8a",  # Light Green
  "Cfc (16)" = "#377eb8",  # Dark Blue
  "Dsa (17)" = "#e41a1c",  # Dark Red
  "Dsb (18)" = "#984ea3",  # Violet
  "Dsc (19)" = "#ff33cc",  # Pinkish Purple
  "Dsd (20)" = "#54278f",  # Dark Purple
  "Dwa (21)" = "#8c510a",  # Dark Brown
  "Dwb (22)" = "#d95f02",  # Orange-Red
  "Dwc (23)" = "#7570b3",  # Bluish Purple
  "Dwd (24)" = "#d73027",  # Bright Red
  "Dfa (25)" = "#c49c94",  # Light Brown
  "Dfb (26)" = "#2166ac",  # Navy Blue
  "Dfc (27)" = "#7b3294",  # Deep Purple
  "Dfd (28)" = "#4d004b",  # Dark Violet
  "ET (29)" = "#a6611a",  # Dark Orange
  "EF (30)" = "#543005"   # Dark Brown
)

shinyServer(function(session, input, output) {
  
  observe_helpers(help_dir = "helpfile")
  
  observe({ # start of first observe
    
    ######################################################################################
    # read in species list files for endemics, agricultural species, pests, etc.
    output$file_inputs <- renderUI({
      req(input$num_files)  # Ensure input exists
      lapply(1:input$num_files, function(i) {
        fileInput(paste0("file", i), paste("Upload Species File", i))
      })
    })
    
    output$column_selects <- renderUI({
      req(input$num_files)
      
      lapply(1:input$num_files, function(i) {
        file_data <- reactive({
          req(input[[paste0("file", i)]])  # Ensure file is uploaded
          df <- read.csv(input[[paste0("file", i)]]$datapath)  # Read the uploaded file
          return(df)
        })
        
        selectInput(
          paste0("column", i), 
          paste("Select Name Column from File", i),
          choices = names(file_data()),  # Use column names as choices
          selected = NULL
        )
      })
    })
    
    
    output$download_combined_files <- downloadHandler(
      filename = function() {
        paste0(input$combo_sp_file_name, ".csv")
      },
      content = function(file) {
        all_data <- lapply(1:input$num_files, function(i) {
          req(input[[paste0("file", i)]], input[[paste0("column", i)]])  # Ensure both file and column selection exist
          
          df <- read.csv(input[[paste0("file", i)]]$datapath)  # Read the file
          col_name <- input[[paste0("column", i)]]  # Get selected column
          
          df_selected <- df %>%
            select(!!sym(col_name)) %>%  # Select only the chosen column
            rename(Species = !!sym(col_name))  # Rename to "Species"
          
          return(df_selected)
        })
        
        # Stack data correctly
        if (input$num_files > 1) {
          final_df <- dplyr::bind_rows(all_data)  # Combine all data
        } else {
          final_df <- all_data[[1]]  # Extract first dataframe if only one file
        }
        
        # Write to temporary file
        write.csv(final_df, file, row.names = FALSE)
      }
    )#downloadhandler
    
    
    ######################################################################################
    
    ######################################################################################    
    # read in the final data csv file from the user's local PC --> ie. the product of watchlistR
    file1 = input$inFile
    if (is.null(file1)) {
      return(NULL)
    } # stops the app crashing while the user hasn't uploaded anything yet
    
    
    if (!is.null(input$inFile)) {
      session$sendCustomMessage("removeGlow", "file-container")
    }
    
    watchlist.data = read.csv(file1$datapath, na.strings = "")
    watchlist.data.sp.names = watchlist.data[1]
    watchlist.data.metrics = colnames(watchlist.data)[2:ncol(watchlist.data)]
    
    # update the drop-down selection menus to show the column names in the uploaded data
    #updateSelectInput(session,"col.species", choices=watchlist.data.sp.names) # first column with species names
    observe({
      req(watchlist.data)  
      species_list <- unique(watchlist.data$species)
      updateSelectizeInput(session, "col.species", 
                           choices = species_list, 
                           server = TRUE)
      updateSelectizeInput(session, "col.species.highrisk", 
                           choices = species_list, 
                           server = TRUE)
    })
    
    updateSelectInput(session, "col.metrics", choices=watchlist.data.metrics) # second col to the last, with stats
    updateSelectInput(session, "col.metrics.highrisk", choices=watchlist.data.metrics) # second col to the last, with stats
    
    # create a separate DF for all the records that are already in the target country
    records_in_target <- watchlist.data %>%
      dplyr::filter(total_records_in_target_country > 0)
    
    output$spp_num = renderText(paste("There are ", nrow(watchlist.data), " species in this watchlist ", sep = ""))
    output$spp_incountry = renderText(paste("There are ", nrow(records_in_target),
                                            " species already in the target country ", sep = ""))
    
    ######################################################################################
    
    #######################################################################################################################
    # Summary species statistics from WatchlistR output
    #######################################################################################################################
    
    observeEvent(input$plot_spp_summaries, {
      
      # Make the plot a reactive expression
      spp_plot <- reactive({
        
        # Filter for selected species
        df_filtered = watchlist.data %>%
          filter(!!sym(colnames(watchlist.data)[1]) %in% req(input$col.species)) %>%
          pivot_longer(cols = all_of(req(input$col.metrics)), names_to = "Metric", values_to = "Value") %>%
          rename(Species = !!sym(colnames(watchlist.data)[1])) %>%  # Rename first column to "Species"
          mutate(Metric = factor(Metric, levels = unique(Metric)))
        
        # Plot grouped bars for multiple species
        plot = ggplot(df_filtered, aes(x = Species, y = Value, fill = Metric)) +
          geom_bar(stat = "identity", position = "dodge", alpha = input$spp_alpha, 
                   color = if (input$spp_add_border) "black" else NA) +  # Dodge for side-by-side bars
          labs(x = input$spp_xlab, y = input$spp_ylab) +
          ggthemes[[input$spp_ggtheme]] +
          theme(
                axis.text.x = element_text(angle = req(input$spp_label_orient), hjust = 1, face = "italic"),
                axis.text = element_text(colour = "black", size = 12),
                axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm"), size = 14),
                axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm"), size = 14),
                legend.position = input$spp_legend_pos,
                legend.text = element_text(size = 12)
          ) +  # Rotate x-axis labels
          labs(x = input$spp_xlab,
               y = input$spp_ylab)
        
        # Apply custom colors if specified
        if(!is.null(input$spp_colours) && length(input$spp_colours) == length(levels(df_filtered$Metric))){
          plot = plot + scale_fill_manual(values = c(input$spp_colours))
        }
        
        # Set ymax range if specified
        if(!is.na(input$ymaxrange)){
          plot = plot + scale_y_continuous(limits = c(0, input$ymaxrange))
        }
        
        plot
      })
      
      # Render the plot
      output$watchlist_plot <- renderPlot({
        spp_plot()
      })
      
      #############
      # Download
      #############
      output$downloadplot_spp <- downloadHandler(
        filename = function () {
          paste(input$file_name_spp_plot, input$plot_format_spp, sep = '.')
        },
        content = function(file) {
          
          width = as.numeric(input$w_spp_plot)
          height = as.numeric(input$h_spp_plot)
          dpi = as.numeric(input$res_spp_plot)
          units = input$unit_spp_plot
          
          # Save the plot using ggsave
          ggsave(file, plot = spp_plot(), width = width, height = height, dpi = dpi, units = units)
        } 
      )#output$downloadplot_spp <- downloadHandler(
      
    }) #observeEvent(input$plot_spp_summaries, {
    
    
    #######################################################################################################################
    # Threshold species --> prioritise the potential worst invaders
    #######################################################################################################################
    
    observeEvent(input$plot_thresholded_spp, {
      
      # Make the plot a reactive expression
      climate_plot <- reactive({
        
        watchlist.data.reduced = watchlist.data %>%
          dplyr::filter(prop_records_in_kg >= req(input$KG_thresh) & total_n >= req(input$num_occurences)) %>%
          dplyr::select(species, matches("^prop_records_in_\\d+$"), -prop_records_in_kg) # make sure prop_records_in ends on a number not a letter
        
        num.spp.thresh = nrow(watchlist.data.reduced)
        
        # Extract the KG zone numbers from the column names
        KG_num <- watchlist.data.reduced %>%
          colnames() %>%
          str_extract("\\d+$") %>%
          as.numeric() %>%
          na.omit()
        
        matching_clim_colours = unname(climate_colors[KG_num])
        
        # Prepare for ggplot
        watchlist.data.reduced.long = reshape2::melt(watchlist.data.reduced) %>%
          mutate(variable = str_replace(variable, "prop_records_in_(\\d+)", "KG \\1")) %>%
          mutate(variable = fct_reorder(variable, as.numeric(str_extract(variable, "\\d+"))))
        
        # Create the ggplot
        ggplot(data = watchlist.data.reduced.long, 
               aes(x = reorder(species, -value), y = value, fill = variable)) +
          geom_bar(stat = "identity", position = "stack", alpha = input$summary_alpha,
                   color = if (input$summary_add_border) "black" else NA) +
          labs(title = paste0("KG similarity >= ", req(input$KG_thresh), "% (n = ", num.spp.thresh, ")"),
               x = "Species",
               y = "Proportion of Records (%)",
               fill = "KG climate zone") +
          ggthemes[[input$summary_ggtheme]] +
          scale_fill_manual(values = matching_clim_colours) +
          theme(
            axis.text.x = element_text(angle = req(input$summary_label_orient), hjust = 1),
            axis.text.y = element_text(face = "italic"),
            axis.text = element_text(colour = "black", size = 12),
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm"), size = 14),
            axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm"), size = 14),
            legend.position = req(input$summary_legend_pos),
            legend.text = element_text(size = 12)
          ) +
          labs(x = input$summary_xlab, y = input$summary_ylab) +
          coord_flip()
      })#reactive
      
      # Render the plot
      output$summary_plot <- renderPlot({
        climate_plot()
      })
      
      #############
      # Download
      #############
      output$downloadplot_summary <- downloadHandler(
        filename = function () {
          paste(input$file_name_summary_plot, input$plot_format_summary, sep = '.')
        },
        content = function(file) {
          
          width = as.numeric(input$w_summary_plot)
          height = as.numeric(input$h_summary_plot)
          dpi = as.numeric(input$res_summary_plot)
          units = input$unit_summary_plot
          
          # Save the plot using ggsave
          ggsave(file, plot = climate_plot(), width = width, height = height, dpi = dpi, units = units)
        } 
      )#output$downloadplot_summary <- downloadHandler(
      
      #####################
      # download CSV file
      ####################
      output$download_summary_data <- downloadHandler(
        
        filename = function() {
          paste(input$file_name_summary_plot, ".csv", sep = '')
        },
        
        content = function(file) {
          # Save the CSV file
          watchlist.data.save = watchlist.data %>%
            dplyr::filter(prop_records_in_kg >= req(input$KG_thresh) & total_n >= req(input$num_occurences))
          
          write.csv(watchlist.data.save, file = file, row.names = FALSE)
        }
      )#output$download_summary_data <- downloadHandler(
      
    }) #observeEvent(input$plot_thresholded_spp, {
    
    
  }) # end of first observe
  
  #######################################################################################################################
  # Do the following when the "Upload" button is clicked
  #######################################################################################################################
  
  observeEvent(input$go, {
    
    output$confirm_file_info <- renderText({
      paste(
        "SPECIES LIST PATH:", input$species_file, 
        "\nSPECIES NAME COLUMN:", input$species_nameCol, 
        "\nTARGET COUNTRY:", input$target_country, 
        "\nKINGDOM:", input$kingdom, 
        "\nSPECIES TO EXCLUDE PATH:", paste0(input$combo_sp_file_name, ".csv"), 
        "\nHIGH RISK COUNTRY/IES:", paste(input$high_risk_countries, collapse = ", "), 
        "\nGBIF USERNAME:", input$gbif_username, 
        "\nGBIF EMAIL:", input$gbif_email, 
        "\nGBIF PASSWORD:", input$gbif_password
      )
    })
    
    output$download_inputfile = downloadHandler(
      filename = function() {
        "WATCHLIST_INPUT_FILE.txt"
      },
      content = function(file) {
        writeLines(c(
          "***WATCHLISTR INPUT FILE***",        # Header
          "",                                  # Empty line (space after header)
          paste("File created on\t", format(Sys.time(), "%Y-%m-%d %H:%M")), # Date and time without seconds
          "",                                  # Empty line (space after date and time)
          paste("SPECIES LIST PATH\t", input$species_file),
          paste("SPECIES NAME COLUMN\t", input$species_nameCol),
          paste("TARGET COUNTRY\t", input$target_country),
          paste("KINGDOM\t", input$kingdom),
          paste("SPECIES TO EXCLUDE PATH\t", paste0(input$combo_sp_file_name, ".csv")),
          paste("HIGH RISK COUNTRY/IES\t", paste(input$high_risk_countries, collapse = ", ")),
          paste("GBIF USERNAME\t", input$gbif_username),
          paste("GBIF EMAIL\t", input$gbif_email),
          paste("GBIF PASSWORD\t", input$gbif_password)
        ), file)
      }
    )
    
    ########################
    
  }) # observeEvent(input$go, {
  
  #######################################################################################################################
  observeEvent(input$plotKGmap, {
    
    KG.map <- reactive({
    
    kg_map = terra::rast("koppen_geiger/Beck_KG_V1_present_0p0083.tif")
    
    # Set the CRS projection for the current climate layers 
    # - Use the correct wkt CRS format 
    terra::crs(kg_map) = "epsg:4326"
    terra::crs(kg_map, describe = T)
    
    country.name = input$target_country_KG_map
    
    country = rnaturalearth::ne_countries(scale = "medium", country = country.name, returnclass = "sf")
    kg_country = terra::crop(kg_map, country)   # Crop to country extent
    kg_country = terra::mask(kg_country, country)  # Mask outside areas
    # Convert the raster to a data frame for ggplot
    kg_df = as.data.frame(kg_country, xy = TRUE, na.rm = TRUE)
    colnames(kg_df)[3] = "climate_zone"  # Rename column for clarity
    
    # Define Köppen-Geiger classification mapping based on provided convention
    koppen_labels <- c(
      "1"  = "Af (1)",
      "2"  = "Am (2)",
      "3"  = "Aw (3)",
      "4"  = "BWh (4)",
      "5"  = "BWk (5)",
      "6"  = "BSh (6)",
      "7"  = "BSk (7)",
      "8"  = "Csa (8)",
      "9"  = "Csb (9)",
      "10" = "Csc (10)",
      "11" = "Cwa (11)",
      "12" = "Cwb (12)",
      "13" = "Cwc (13)",
      "14" = "Cfa (14)",
      "15" = "Cfb (15)",
      "16" = "Cfc (16)",
      "17" = "Dsa (17)",
      "18" = "Dsb (18)",
      "19" = "Dsc (19)",
      "20" = "Dsd (20)",
      "21" = "Dwa (21)",
      "22" = "Dwb (22)",
      "23" = "Dwc (23)",
      "24" = "Dwd (24)",
      "25" = "Dfa (25)",
      "26" = "Dfb (26)",
      "27" = "Dfc (27)",
      "28" = "Dfd (28)",
      "29" = "ET (29)",
      "30" = "EF (30)"
    )
    
    # Convert numeric climate zones to factor with labels
    kg_df <- kg_df %>%
      dplyr::mutate(climate_zone = factor(climate_zone, levels = names(koppen_labels), labels = koppen_labels)) 
    
      # Plot using ggplot
      ggplot() +
        geom_tile(data = kg_df %>% filter(!is.na(climate_zone)), 
                  aes(x = x, y = y, fill = climate_zone)) +  # Raster data
        geom_sf(data = country, fill = NA, color = "black", size = 0.5) +  # Country border
        scale_fill_manual(name = "Climate Zone", values = climate_colors) +  # Discrete color scale
        labs(title = #paste("Köppen-Geiger Climate Zones of", country.name),
               input$KGmap_title,
             x = "Longitude", y = "Latitude") +
        theme(legend.position = "right") +
        theme_classic() +
        coord_sf(
          xlim = if (is.na(input$KG_xmin) | is.na(input$KG_xmax)) NULL else c(input$KG_xmin, input$KG_xmax),
          ylim = if (is.na(input$KG_ymin) | is.na(input$KG_ymax)) NULL else c(input$KG_ymin, input$KG_ymax),
          expand = FALSE
        ) +
        ggthemes[[input$ggtheme_KGmap]] +
        theme(
          axis.title = element_text(size = input$KG_labsize),  # Change axis labels size
          axis.text = element_text(size = input$KG_axisvaluesize),    # Change tick values size
          plot.title = element_text(size = input$KG_title_size), # title size
          axis.text.x = element_text(angle = req(input$label_KG_orient)),
        )
      
    })#reactive
    
    output$plot_KG = renderPlot({
      KG.map()
    })
    
    #################################################################
    # DOWNLOAD KG MAP
    #################################################################
    
    output$downloadplot_KGmap <- downloadHandler(
      filename = function (){paste("KG_map", input$target_country_KG_map, 
                                   input$plot_format_KGmap, sep = '.')},
      content = function(file){
        
        width = as.numeric(input$w_KGmap_plot)
        height = as.numeric(input$h_KGmap_plot)
        dpi = as.numeric(input$res_KGmap_plot)
        units = input$unit_KGmap_plot
        
        ggsave(file, KG.map(), width = width, height = height, dpi = dpi, units = units)
      } 
    )
    
    ###########################
    #######################################################################################################################
  }) # observeEvent(input$plotKGmap, {   
  
  
  
}) # end of start of shinyserver