source("data.R")

# SERVER
shinyServer(
  function(input, output, session) {
    
    # ENTIRE PAGE ####################################################################################################################################
    observe_helpers()
    
    # HOME PAGE ######################################################################################################################################                         
    # circular_barplot_home_ggplot
    output$circular_barplot_home_ggplot <- renderPlot({
      ggplot(data) +      
        geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", position = "stack", alpha=0.5) +
        ylim(-3,max(label_data$tot, na.rm=T)) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
        ) +
        coord_polar() +
        geom_text(data=label_data, aes(x=id, y=1, label=individual, hjust=hjust), color="black",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
       # geom_text(data=label_data, aes(x=id, y=0.5, label=tot, hjust=hjust), color="black",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
        scale_fill_gradient2(low='darkred', mid="red", high='green', space='Lab')
    })
    
    # Making Home Page boxes take you to the corresponding tabs  
    observeEvent(input$GoToProductivityTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "ProductivityTab"
      )
    })
    observeEvent(input$GoToPopulationTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "PopulationTab"
      )
    })
    observeEvent(input$GoToParticipationTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "ParticipationTab"
      )
    })
    observeEvent(input$GoToPeopleTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "PeopleTab"
      )
    })    
    observeEvent(input$GoToPlaceTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "PlaceTab"
      )
    })
    observeEvent(input$GoToSustainabilityTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "SustainabilityTab"
      )
    })
    observeEvent(input$GoToEqualitiesTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "EqualitiesTab"
      )
    })
    
    # PRODUCTIVITY ###################################################################################################################################
    # PRODUCTIVITY OVERVIEW
    # Horizontal bar - growth
    output$growth_bar <- renderUI({
      if (rank_growth_quantile >= 0.75) {
        column(width = 2,
          tags$b("Growth rate: "),
          tags$p(as.character(rank_growth), style="font-size: 48px"),
          style="background-color: red;"
        )
      } else if (rank_growth_quantile <= 0.25) {
        column(width = 2,
          tags$b("Growth rate: "),
          tags$p(as.character(rank_growth), style="font-size: 48px"),
          style="background-color: green;"
        )
      } else {
        column(width = 2,
          tags$b("Growth rate: "),
          tags$p(as.character(rank_growth), style="font-size: 48px"),
          style="background-color: orange;"
        )
      }
    })
    # Horizontal bar - productivity
    output$productivity_bar <- renderUI({
      if (rank_productivity_quantile >= 0.75) {
        column(width = 2,
          tags$b("Productivity: "),
          tags$p(as.character(rank_productivity), style="font-size: 48px"),
          style="background-color: red;"
        )
      } else if (rank_productivity_quantile <= 0.25) {
        column(width = 2,
          tags$b("Productivity: "),
          tags$p(as.character(rank_productivity), style="font-size: 48px"),
          style="background-color: green;"
        )
      } else {
        column(width = 2,
          tags$b("Productivity: "),
          tags$p(as.character(rank_productivity), style="font-size: 48px"),
          style="background-color: orange;"
        )
      }
    })
    # Horizontal bar - exporting
    output$exporting_bar <- renderUI({
      if (rank_exporting_quantile >= 0.75) {
        column(width = 2,
          tags$b("Exporting: "),
          tags$p(as.character(rank_exporting), style="font-size: 48px"),
          style="background-color: red;"
        )
      } else if (rank_exporting_quantile <= 0.25) {
        column(width = 2,
          tags$b("Exporting: "),
          tags$p(as.character(rank_exporting), style="font-size: 48px"),
          style="background-color: green;"
        )
      } else {
        column(width = 2,
          tags$b("Exporting: "),
          tags$p(as.character(rank_exporting), style="font-size: 48px"),
          style="background-color: orange;"
        )
      }
    })
    # Horizontal bar - rd
    output$rd_bar <- renderUI({
      if (rank_rd_quantile >= 0.75) {
        column(width = 2,
          tags$b("R&D: "),
          tags$p(as.character(rank_rd), style="font-size: 48px"),
          style="background-color: red;"
        )
      } else if (rank_rd_quantile <= 0.25) {
        column(width = 2,
          tags$b("R&D: "),
          tags$p(as.character(rank_rd), style="font-size: 48px"),
          style="background-color: green;"
        )
      } else {
        column(width = 2,
          tags$b("R&D: "),
          tags$p(as.character(rank_rd), style="font-size: 48px"),
          style="background-color: orange;"
        )
      }
    })
    # Horizontal bar - entrepreneurialism
    output$entrepreneurialism_bar <- renderUI({
      if (rank_entrepreneurialism_quantile >= 0.75) {
        column(width = 2,
          tags$b("Entrepreneurialism: "),
          tags$p(as.character(rank_entrepreneurialism), style="font-size: 48px"),
          style="background-color: red;"
        )
      } else if (rank_entrepreneurialism_quantile <= 0.25) {
        column(width = 2,
          tags$b("Entrepreneurialism: "),
          tags$p(as.character(rank_entrepreneurialism), style="font-size: 48px"),
          style="background-color: green;"
        )
      } else {
        column(width = 2,
          tags$b("Entrepreneurialism: "),
          tags$p(as.character(rank_entrepreneurialism), style="font-size: 48px"),
          style="background-color: orange;"
        )
      }
    })
    # ggplot barplot for growth_overview
    output$growth_overview_int_barplot <- renderPlot({
      growth_overview_int2 <- growth_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(growth_overview_int$Country == "Scotland","blue","grey"))
      ggplot(growth_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(growth_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("GDP growth rate") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    # ggplot barplot for exporting_overview
    output$exporting_overview_int_barplot <- renderPlot({
      exporting_overview_int2 <- exporting_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(exporting_overview_int$Country == "Scotland (international and rUK)","blue","grey"))
      ggplot(exporting_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=c("grey","grey","grey","grey","grey","blue","grey"), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
  
        ggtitle(str_wrap("Exports (% of GDP)", width = 8)) +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    # ggplot barplot for exporting_overview
  #  output$exporting_overview_int_barplot <- renderPlot({
   #   exporting_overview_int2 <- exporting_overview_int %>%
    #    mutate(Country = (Country),
     #          Value = as.numeric(Value),
      #         fill_type = ifelse(exporting_overview_int$Country == "Scotland (international and rUK)","blue","grey"))
    #  ggplot(exporting_overview_int2, aes(x=Country, y=Value)) +
     #   geom_col(fill=factor(exporting_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
      #  coord_flip(clip="off", expand=TRUE) +
       # labs(x="", y="%") +
        #theme_minimal() +
        #theme(
         # axis.text.x=element_blank()
        #) +
      #  ggtitle("Exports as % of GDP") +
       # geom_text(aes(x=Country, y=Value, label=Value),
        #          vjust=0.3, size=4, hjust=1.1, col="white")
  #  })
    
    
     # ggplot barplot for productivity_overview
    output$productivity_overview_int_barplot <- renderPlot({
      productivity_overview_int2 <- productivity_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(productivity_overview_int$Country == "Scotland","blue","grey"))
      ggplot(productivity_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(productivity_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="% of USA") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Productivity (USA=100)") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    # ggplot lineplot for reputation_overview_sco
    output$reputation_overview_sco_lineplot <- renderPlot({
      ggplot(reputation_overview_sco) +
        geom_line(stat="identity", aes(x=Year, y=Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
        labs(y = "NBI Index", x = "") +
        theme_minimal() +
        ggtitle("Anholt GfK-Roper Nation Brands Index (NBI)") +
        scale_color_jama() +
        geom_point(aes(y=Value, x=Year), color = "#F8766D", show.legend = FALSE, size=4) +
        geom_text(aes(y=Value, x=Year, label=Value), position=position_dodge(width=0.9), size = 4, vjust=2)
    })
    
    # ggplot barplot for rd_overview_int
    output$rd_overview_int_barplot <- renderPlot({
      rd_overview_int2 <- rd_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(rd_overview_int$Country == "Scotland","blue","grey"))
      ggplot(rd_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(rd_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("% of GDP spent on R&D") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # Pie chart
    # # ggplot pieplot for rd_overview_sco
    # rd_overview_sco2 <- rd_overview_sco %>%
    #   mutate(Performer = (Performer),
    #          Value = as.numeric(Value),
    #          fill_type = ifelse(rd_overview_sco$Performer == "Government incl. Research Councils","blue","grey")) 
    # output$rd_overview_sco_pieplot <- renderPlot({
    #   ggplot(rd_overview_sco2, aes(x="", y=Value, fill=Performer)) +
    #     geom_bar(stat="identity", width=1, show.legend = TRUE, color="white") +
    #     coord_polar("y", start=0) +
    #     theme_void() +
    #     ggtitle("Breakdown of Scotland's R&D Expenditure") +
    #     guides(fill=guide_legend(title="Performer",reverse = TRUE)) +
    #     geom_text(aes(label = paste(round(Value / sum(Value) * 100, 1), "%", sep=""), x=1.75),
    #               position = position_stack(vjust = 0.5))
    # })
    
    # PRODUCTIVITY DETAILS
    # Dygraph for growth_int
    output$growth_int_graph <- renderDygraph({
      dygraph(
        growth_int_wide[,c(1, as.numeric(input$growth_int_input))]
      ) %>%
        dyGroup(names(growth_int_wide)[c(1, as.numeric(input$growth_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_growth_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for productivity_int
    output$productivity_int_graph <- renderDygraph({
      dygraph(
        productivity_int_wide[,c(1, as.numeric(input$productivity_int_input))]
      ) %>%
        dyGroup(names(productivity_int_wide)[c(1, as.numeric(input$productivity_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of USA") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_productivity", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for exporting_int
    output$exporting_int_graph <- renderDygraph({
      dygraph(
        exporting_int_wide[,c(1, as.numeric(input$exporting_int_input))]
      ) %>%
        dyGroup(names(exporting_int_wide)[c(1, as.numeric(input$exporting_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of USA") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_exporting", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for rd_int
    output$rd_int_graph <- renderDygraph({
      dygraph(
        rd_int_wide[,c(1, as.numeric(input$rd_int_input))]
      ) %>%
        dyGroup(names(rd_int_wide)[c(1, as.numeric(input$rd_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of GDP") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_rd", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for entrepreneurialism_int
    output$entrepreneurialism_int_graph <- renderDygraph({
      dygraph(
        entrepreneurialism_int_wide[,c(1, as.numeric(input$entrepreneurialism_int_input))]
      ) %>%
        dyGroup(names(entrepreneurialism_int_wide)[c(1, as.numeric(input$entrepreneurialism_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of GDP") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_entrepreneurialism", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # ggplot barplot for entrepreneurialism2_overview_int
    output$entrepreneurialism2_overview_int_barplot <- renderPlot({
      entrepreneurialism2_overview_int <- entrepreneurialism2_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(entrepreneurialism2_overview_int, aes(x=Country, y=Value)) +
        geom_col(fill=factor(entrepreneurialism2_overview_int$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    
    # Dygraph for growth_sco
    output$growth_sco_graph <- renderDygraph({
      dygraph(
        growth_sco_wide[,c(1, as.numeric(input$growth_sco_input))]
      ) %>%
        dyGroup(names(growth_sco_wide)[c(1, as.numeric(input$growth_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_growth_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for productivity_sco
    output$productivity_sco_graph <- renderDygraph({
      dygraph(
        productivity_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Output per hour") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_productivity_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for exporting_destination_sco
    output$exporting_destination_sco_graph <- renderDygraph({
      dygraph(
        exporting_destination_sco[,c(1, as.numeric(input$exporting_destination_sco_input))]
      ) %>%
        dyGroup(names(exporting_destination_sco)[c(1, as.numeric(input$exporting_destination_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "£ million") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_exporting_destination_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for exporting_sector_sco
  #  output$exporting_sector_sco_graph <- renderDygraph({
   #   dygraph(
    #    exporting_sector_sco[,c(1, as.numeric(input$exporting_sector_sco_input))]
     # ) %>%
      #  dyGroup(names(exporting_sector_sco)[c(1, as.numeric(input$exporting_sector_sco_input))], strokeWidth = 2) %>%
       # dyRangeSelector() %>%
        #dyAxis("x", label = "Year", rangePad = 5) %>%
      #  dyAxis("y", label = "£ million") %>%
      #  dyHighlight(
      #    highlightCircleSize = 3,
      #    highlightSeriesBackgroundAlpha = 0.2,
      #    hideOnMouseOut = FALSE,
      #    highlightSeriesOpts = list(strokeWidth = 6)
      #  ) %>%
      #  dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
      #  dyLegend(labelsDiv = "legendDivID_exporting_sector_sco", labelsSeparateLines = TRUE) %>%
      #  dyUnzoom() %>%
      #  dyCrosshair(direction = "vertical")
#    })
    
    #Dygraph for exporting_sector_sco
    output$exporting_sector_sco_graph <- renderDygraph({
      dygraph(
        exporting_sector_sco_wide[,c(1, 2)]
       # exporting_sector_sco_wide[,c(1, as.numeric(input$exporting_sector_sco_input))]
      ) %>%
        dyGroup(names(exporting_sector_sco_wide)[c(1, as.numeric(input$exporting_sector_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of working-age population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_exporting_sector_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for rd_sco
    output$rd_sco_graph <- renderDygraph({
      dygraph(
        rd_sco[,c(1, as.numeric(input$rd_sco_input))]
      ) %>%
        dyGroup(names(rd_sco)[c(1, as.numeric(input$rd_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_rd_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for entrepreneurialism_sco
    output$entrepreneurialism_sco_graph <- renderDygraph({
      dygraph(
        entrepreneurialism_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "TEA rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_entrepreneurialism_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for reputation_sco
    output$reputation_sco_graph <- renderDygraph({
      dygraph(
        reputation_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "NBI") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_reputation_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for nbusiness_sector_sco
    output$nbusiness_sector_sco_graph <- renderDygraph({
      dygraph(
        nbusiness_sector_sco[,c(1, as.numeric(input$nbusiness_sector_sco_input))]
      ) %>%
        dyGroup(names(nbusiness_sector_sco)[c(1, as.numeric(input$nbusiness_sector_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Number of businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_nbusiness_sector_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for nbusiness_region_sco
    output$nbusiness_region_sco_graph <- renderDygraph({
      dygraph(
        nbusiness_region_sco[,c(1, as.numeric(input$nbusiness_region_sco_input))]
      ) %>%
        dyGroup(names(nbusiness_region_sco)[c(1, as.numeric(input$nbusiness_region_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Number of businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_nbusiness_region_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for nbusiness_employees_sco
    output$nbusiness_employees_sco_graph <- renderDygraph({
      dygraph(
        nbusiness_employees_sco[,c(1, as.numeric(input$nbusiness_employees_sco_input))]
      ) %>%
        dyGroup(names(nbusiness_employees_sco)[c(1, as.numeric(input$nbusiness_employees_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Number of businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_nbusiness_employees_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for hbusiness_percent_sco
    output$hbusiness_percent_sco_graph <- renderDygraph({
      dygraph(
        hbusiness_percent_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percent of all stocks") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_hbusiness_percent_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    # Dygraph for ibusiness_sco
    output$ibusiness_sco_graph <- renderDygraph({
      dygraph(
        ibusiness_sco[,c(1, as.numeric(input$ibusiness_sco_input))]
      ) %>%
        dyGroup(names(ibusiness_sco)[c(1, as.numeric(input$ibusiness_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percent") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_ibusiness_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })

    # Dygraph for ncai_sco
    output$ncai_sco_graph <- renderDygraph({
      dygraph(
        ncai_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Index value") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_ncai_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
        # Leaflet map for GVA_reg
    output$GVA_reg_map_caption <- renderText({
      paste("Map 1. GVA per Head across Scottish council areas in ", as.character(input$GVA_reg_input), " (£ per Head)", sep="")
    })
    GVA_reg_map_data <- reactive({
      GVA_reg_one <- GVA_reg[ which(GVA_reg$Year == input$GVA_reg_input), ]
      GVA_reg_one <- GVA_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, GVA_reg_one, by = intersect(names(mapex@data), names(GVA_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_GVA_reg <- colorBin(palette=brewer.pal(n=9, name="BuGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_GVA_reg = choropleth_GVA_reg)
      return(list_return)
    })
    output$GVA_reg_map <- renderLeaflet({
      mapex <- GVA_reg_map_data()$mapex
      choropleth_GVA_reg <- GVA_reg_map_data()$choropleth_GVA_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_GVA_reg(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " £", as.character(mapex$Growth), " per Head", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_GVA_reg, values = mapex$Value, title = paste("GVA per Head", " (£)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
     })
    # Leaflet map for exporting_reg
    output$exporting_reg_map_caption <- renderText({
      paste("Map 2. % of total Scottish exports across Scottish council areas in ", as.character(input$exporting_reg_input), sep="")
    })
    exporting_reg_map_data <- reactive({
      exporting_reg_one <- exporting_reg[ which(exporting_reg$Year == input$exporting_reg_input), ]
      exporting_reg_one <- exporting_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, exporting_reg_one, by = intersect(names(mapex@data), names(exporting_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_exporting_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Exporting, bins = 9)
      list_return <- list(mapex = mapex, choropleth_exporting_reg = choropleth_exporting_reg)
      return(list_return)
    })
    output$exporting_reg_map <- renderLeaflet({
      mapex <- exporting_reg_map_data()$mapex
      choropleth_exporting_reg <- exporting_reg_map_data()$choropleth_exporting_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth_exporting_reg(mapex@data$Exporting), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " ", as.character(mapex@data$Exporting), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_exporting_reg, values = mapex@data$Exporting, title = paste("% of total Scottish exports ", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for rd_reg
    output$rd_reg_map_caption <- renderText({
      paste("Map 3. BERD as share of GDP across Scottish council areas in ", as.character(input$rd_reg_input), sep="")
    })
    rd_reg_map_data <- reactive({
      rd_reg_one <- rd_reg[ which(rd_reg$Year == input$rd_reg_input), ]
      rd_reg_one <- rd_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, rd_reg_one, by = intersect(names(mapex@data), names(rd_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_rd_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$RD, bins = 9)
      list_return <- list(mapex = mapex, choropleth_rd_reg = choropleth_rd_reg)
      return(list_return)
    })
    output$rd_reg_map <- renderLeaflet({
      mapex <- rd_reg_map_data()$mapex
      choropleth_rd_reg <- rd_reg_map_data()$choropleth_rd_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth_rd_reg(mapex@data$RD), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " ", as.character(mapex@data$RD), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_rd_reg, values = mapex@data$RD, title = paste("Total BERD as share of GDP ", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for nbusiness_reg
    output$nbusiness_reg_map_caption <- renderText({
      paste("Map 4. The Number of Businesses across Scottish council areas in ", as.character(input$nbusiness_reg_input), sep="")
    })
    nbusiness_reg_map_data <- reactive({
      nbusiness_reg_one <- nbusiness_reg[ which(nbusiness_reg$Year == input$nbusiness_reg_input), ]
      nbusiness_reg_one <- nbusiness_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, nbusiness_reg_one, by = intersect(names(mapex@data), names(nbusiness_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_nbusiness_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Businesses, bins = 9)
      list_return <- list(mapex = mapex, choropleth_nbusiness_reg = choropleth_nbusiness_reg)
      return(list_return)
    })    
    output$nbusiness_reg_map <- renderLeaflet({
      mapex <- nbusiness_reg_map_data()$mapex
      choropleth_nbusiness_reg <- nbusiness_reg_map_data()$choropleth_nbusiness_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth_nbusiness_reg(mapex@data$Businesses), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " ", as.character(mapex@data$Businesses), " businesses", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_nbusiness_reg, values = mapex@data$Businesses, title = paste("Number of businesses ", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
  
    # POPULATION #####################################################################################################################################
    # POPULATION OVERVIEW
    # Horizontal bar - depratio
    output$depratio_bar <- renderUI({
      if (rank_depratio_quantile >= 0.75) {
        column(width = 2,
               tags$b("Dependency ratio: "),
               tags$p(as.character(rank_depratio), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_depratio_quantile <= 0.25) {
        column(width = 2,
               tags$b("Dependency ratio: "),
               tags$p(as.character(rank_depratio), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Dependency ratio: "),
               tags$p(as.character(rank_depratio), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    
    # ggplot barplot for depratio_overview
    output$depratio_overview_int_barplot <- renderPlot({
      depratio_overview_int2 <- depratio_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(depratio_overview_int$Country == "Scotland","blue","grey"))
      ggplot(depratio_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(depratio_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="% of working-age population") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Age dependency ratio") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # POPULATION DETAILS
    # Dygraph for depratio_int
    output$depratio_int_graph <- renderDygraph({
      dygraph(
        depratio_int_wide[,c(1, as.numeric(input$depratio_int_input))]
      ) %>%
        dyGroup(names(depratio_int_wide)[c(1, as.numeric(input$depratio_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of working-age population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_depratio_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for migration_int
    output$migration_int_graph <- renderDygraph({
      dygraph(
        migration_int_wide[,c(1, as.numeric(input$migration_int_input))]
      ) %>%
        dyGroup(names(migration_int_wide)[c(1, as.numeric(input$migration_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Crude rate of net migration plus statistical adjustment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_migration_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for depratio_sco
    output$depratio_sco_graph <- renderDygraph({
      dygraph(
        depratio_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "% of working age population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_depratio_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for migration_type_overseas_sco
    output$migration_type_overseas_sco_graph <- renderDygraph({
      dygraph(
        migration_type_overseas_sco_wide[,c(1, as.numeric(input$migration_type_overseas_sco_input))]
      ) %>%
        dyGroup(names(migration_type_overseas_sco_wide)[c(1, as.numeric(input$migration_type_overseas_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "People") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_migration_type_overseas_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for migration_in_source_sco
    output$migration_in_source_sco_graph <- renderDygraph({
      dygraph(
        migration_in_source_sco_wide[,c(1, as.numeric(input$migration_in_source_sco_input))]
      ) %>%
        dyGroup(names(migration_in_source_sco_wide)[c(1, as.numeric(input$migration_in_source_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "People") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_migration_in_source_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for migration_in_overseas_age_sco
    output$migration_in_overseas_age_sco_graph <- renderDygraph({
      dygraph(
        migration_in_overseas_age_sco_wide[,c(1, as.numeric(input$migration_in_overseas_age_sco_input))]
      ) %>%
        dyGroup(names(migration_in_overseas_age_sco_wide)[c(1, as.numeric(input$migration_in_overseas_age_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "People") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_migration_in_overseas_age_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for migration_in_overseas_sex_sco
    output$migration_in_overseas_sex_sco_graph <- renderDygraph({
      dygraph(
        migration_in_overseas_sex_sco_wide[,c(1, as.numeric(input$migration_in_overseas_sex_sco_input))]
      ) %>%
        dyGroup(names(migration_in_overseas_sex_sco_wide)[c(1, as.numeric(input$migration_in_overseas_sex_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "People") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_migration_in_overseas_sex_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Leaflet map for depratio_reg
    output$depratio_reg_map_caption <- renderText({
      paste("Map 1. Dependency ratio from population estimates in ", as.character(input$depratio_reg_input), sep="")
    })
    depratio_reg_map_data <- reactive({
      depratio_reg_one <- depratio_reg[ which(depratio_reg$Year == input$depratio_reg_input), ]
      depratio_reg_one <- depratio_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, depratio_reg_one, by = intersect(names(mapex@data), names(depratio_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_depratio_reg <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), mapex@data$depratio, bins = 9)
      list_return <- list(mapex = mapex, choropleth_depratio_reg = choropleth_depratio_reg)
      return(list_return)
    })
    output$depratio_reg_map <- renderLeaflet({
      mapex <- depratio_reg_map_data()$mapex
      choropleth_depratio_reg <- depratio_reg_map_data()$choropleth_depratio_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_depratio_reg(mapex$depratio), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$depratio), sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_depratio_reg, values = mapex$depratio, title = paste("Dependency ratio", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for migration_reg
    output$migration_reg_map_caption <- renderText({
      paste("Map 2. Total net migration across Scottish council areas in ", as.character(input$migration_reg_input), sep="")
    })
    migration_reg_map_data <- reactive({
      migration_reg_one <- migration_reg[ which(migration_reg$Year == input$migration_reg_input), ]
      migration_reg_one <- migration_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, migration_reg_one, by = intersect(names(mapex@data), names(migration_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_migration_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$migration, bins = 9)
      list_return <- list(mapex = mapex, choropleth_migration_reg = choropleth_migration_reg)
      return(list_return)
    })
    output$migration_reg_map <- renderLeaflet({
      mapex <- migration_reg_map_data()$mapex
      choropleth_migration_reg <- migration_reg_map_data()$choropleth_migration_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_migration_reg(mapex$migration), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$migration), sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_migration_reg, values = mapex$migration, title = paste("Total net migration", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for working_age_reg
    output$working_age_reg_map_caption <- renderText({
      paste("Map 3. Total working age population forecast across Scottish council areas in ", as.character(input$working_age_reg_input), sep="")
    })
    working_age_reg_map_data <- reactive({
      working_age_reg_one <- working_age_reg[ which(working_age_reg$Year == input$working_age_reg_input), ]
      working_age_reg_one <- working_age_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, working_age_reg_one, by = intersect(names(mapex@data), names(working_age_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_working_age_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$working_age, bins = 9)
      list_return <- list(mapex = mapex, choropleth_working_age_reg = choropleth_working_age_reg)
      return(list_return)
    })
    output$working_age_reg_map <- renderLeaflet({
      mapex <- working_age_reg_map_data()$mapex
      choropleth_working_age_reg <- working_age_reg_map_data()$choropleth_working_age_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_working_age_reg(mapex$working_age), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$working_age), sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_working_age_reg, values = mapex$working_age, title = paste("Total working age population", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # PARTICIPATION ##################################################################################################################################
    # Horizontal bar - eactivity_bar
    output$eactivity_bar <- renderUI({
      if (rank_depratio_quantile >= 0.75) {
        column(width = 2,
               tags$b("Economic activity: "),
               tags$p(as.character(rank_eactivity), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_depratio_quantile <= 0.25) {
        column(width = 2,
               tags$b("Economic activity: "),
               tags$p(as.character(rank_eactivity), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Economic activity: "),
               tags$p(as.character(rank_eactivity), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    # Horizontal bar - employment_bar
    output$employment_bar <- renderUI({
      if (rank_depratio_quantile >= 0.75) {
        column(width = 2,
               tags$b("Employment rate: "),
               tags$p(as.character(rank_employment), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_depratio_quantile <= 0.25) {
        column(width = 2,
               tags$b("Employment rate: "),
               tags$p(as.character(rank_employment), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Employment rate: "),
               tags$p(as.character(rank_employment), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    # Horizontal bar - unemployment_bar
    output$unemployment_bar <- renderUI({
      if (rank_depratio_quantile >= 0.75) {
        column(width = 2,
               tags$b("Unemployment rate: "),
               tags$p(as.character(rank_unemployment), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_depratio_quantile <= 0.25) {
        column(width = 2,
               tags$b("Unemployment rate: "),
               tags$p(as.character(rank_unemployment), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Unemployment rate: "),
               tags$p(as.character(rank_unemployment), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    # ggplot barplot for eactivity_overview
    output$eactivity_overview_int_barplot <- renderPlot({
      eactivity_overview_int2 <- eactivity_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(eactivity_overview_int$Country == "Scotland","blue","grey"))
      ggplot(eactivity_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(eactivity_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Economic activity rate") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    # ggplot barplot for employment_overview
    output$employment_overview_int_barplot <- renderPlot({
      employment_overview_int2 <- employment_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(employment_overview_int$Country == "Scotland","blue","grey"))
      ggplot(employment_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(employment_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Employment rate") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    # ggplot barplot for unemployment_overview
    output$unemployment_overview_int_barplot <- renderPlot({
      unemployment_overview_int2 <- unemployment_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(unemployment_overview_int$Country == "Scotland","blue","grey"))
      ggplot(unemployment_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(unemployment_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Unemployment rate") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # ggplot barplot for genderpaygap_overview
    output$genderpaygap_overview_int_barplot <- renderPlot({
      genderpaygap_overview_int2 <- genderpaygap_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(genderpaygap_overview_int$Country == "Scotland","blue","grey"))
      genderpaygap_overview_int2 <- subset(genderpaygap_overview_int2, genderpaygap_overview_int2$Value > 0)
      ggplot(genderpaygap_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(genderpaygap_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Gender Pay Gap") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # ggplot barplot for skillsunderprimary_overview
    output$skillsunderprimary_overview_int_barplot <- renderPlot({
      skillsunderprimary_overview_int2 <- skillsunderprimary_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(skillsunderprimary_overview_int$Country == "Scotland","blue","grey"))
      ggplot(skillsunderprimary_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(skillsunderprimary_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Attainment level of less than primary, primary and lower secondary education") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # ggplot barplot for yunemployment_overview
    output$yunemployment_overview_int_barplot <- renderPlot({
      yunemployment_overview_int2 <- yunemployment_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(yunemployment_overview_int$Country == "Scotland","blue","grey"))
      ggplot(yunemployment_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(yunemployment_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Youth unemployment rate") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # Dygraph for eactivity_int
    output$eactivity_int_graph <- renderDygraph({
      dygraph(
        eactivity_int_wide[,c(1, as.numeric(input$eactivity_int_input))]
      ) %>%
        dyGroup(names(eactivity_int_wide)[c(1, as.numeric(input$eactivity_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Economic Activity rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_eactivity_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for employment_int
    output$employment_int_graph <- renderDygraph({
      dygraph(
        employment_int_wide[,c(1, as.numeric(input$employment_int_input))]
      ) %>%
        dyGroup(names(employment_int_wide)[c(1, as.numeric(input$employment_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Employment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_employment_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for unemployment_int
    output$unemployment_int_graph <- renderDygraph({
      dygraph(
        unemployment_int_wide[,c(1, as.numeric(input$unemployment_int_input))]
      ) %>%
        dyGroup(names(unemployment_int_wide)[c(1, as.numeric(input$unemployment_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_unemployment_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # # Dygraph for genderpaygap_int
    # output$genderpaygap_int_graph <- renderDygraph({
    #   dygraph(
    #     genderpaygap_int_wide[,c(1, as.numeric(input$genderpaygap_int_input))]
    #   ) %>%
    #     dyGroup(names(genderpaygap_int_wide)[c(1, as.numeric(input$genderpaygap_int_input))], strokeWidth = 2) %>%
    #     dyRangeSelector() %>%
    #     dyAxis("x", label = "Year", rangePad = 5) %>%
    #     dyAxis("y", label = "genderpaygap rate") %>%
    #     dyHighlight(
    #       highlightCircleSize = 3,
    #       highlightSeriesBackgroundAlpha = 0.2,
    #       hideOnMouseOut = FALSE,
    #       highlightSeriesOpts = list(strokeWidth = 6)
    #     ) %>%
    #     dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
    #     dyLegend(labelsDiv = "legendDivID_genderpaygap_int", labelsSeparateLines = TRUE) %>%
    #     dyUnzoom() %>%
    #     dyCrosshair(direction = "vertical")
    # })
    
    # ggplot barplot for genderpaygap2_overview_int
    output$genderpaygap2_overview_int_barplot <- renderPlot({
      genderpaygap2_overview_int <- genderpaygap2_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(genderpaygap2_overview_int, aes(x=Country, y=Value)) +
        geom_col(fill=factor(genderpaygap2_overview_int$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    
    # Dygraph for skillsunderprimary_int
    output$skillsunderprimary_int_graph <- renderDygraph({
      dygraph(
        skillsunderprimary_int_wide[,c(1, as.numeric(input$skillsunderprimary_int_input))]
      ) %>%
        dyGroup(names(skillsunderprimary_int_wide)[c(1, as.numeric(input$skillsunderprimary_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percentage") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_skillsunderprimary_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for skillstertiary_int
    output$skillstertiary_int_graph <- renderDygraph({
      dygraph(
        skillstertiary_int_wide[,c(1, as.numeric(input$skillstertiary_int_input))]
      ) %>%
        dyGroup(names(skillstertiary_int_wide)[c(1, as.numeric(input$skillstertiary_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percantage") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_skillstertiary_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for yunemployment_int
    output$yunemployment_int_graph <- renderDygraph({
      dygraph(
        yunemployment_int_wide[,c(1, as.numeric(input$yunemployment_int_input))]
      ) %>%
        dyGroup(names(yunemployment_int_wide)[c(1, as.numeric(input$yunemployment_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Youth unemployment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_yunemployment_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # # Dygraph for evoice_int
    # output$evoice_int_graph <- renderDygraph({
    #   dygraph(
    #     evoice_int_wide[,c(1, as.numeric(input$evoice_int_input))]
    #   ) %>%
    #     dyGroup(names(evoice_int_wide)[c(1, as.numeric(input$evoice_int_input))], strokeWidth = 2) %>%
    #     dyRangeSelector() %>%
    #     dyAxis("x", label = "Year", rangePad = 5) %>%
    #     dyAxis("y", label = "Percantage") %>%
    #     dyHighlight(
    #       highlightCircleSize = 3,
    #       highlightSeriesBackgroundAlpha = 0.2,
    #       hideOnMouseOut = FALSE,
    #       highlightSeriesOpts = list(strokeWidth = 6)
    #     ) %>%
    #     dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
    #     dyLegend(labelsDiv = "legendDivID_evoice_int", labelsSeparateLines = TRUE) %>%
    #     dyUnzoom() %>%
    #     dyCrosshair(direction = "vertical")
    # })
    
    # ggplot barplot for evoice2_overview_int
    output$evoice2_overview_int_barplot <- renderPlot({
      evoice2_overview_int <- evoice2_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(evoice2_overview_int, aes(x=Country, y=Value)) +
        geom_col(fill=factor(evoice2_overview_int$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # Dygraph for eactivity_sco
    output$eactivity_sco_graph <- renderDygraph({
      dygraph(
        eactivity_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_eactivity_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for livingwage_sco
    output$livingwage_sco_graph <- renderDygraph({
      dygraph(
        livingwage_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_livingwage_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for genderpaygap_sco
    output$genderpaygap_sco_graph <- renderDygraph({
      dygraph(
        genderpaygap_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "percentage of male earnings") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_genderpaygap_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for evoice_sco
    output$evoice_sco_graph <- renderDygraph({
      dygraph(
        evoice_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_evoice_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for wplearning_sco
    output$wplearning_sco_graph <- renderDygraph({
      dygraph(
        wplearning_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_wplearning_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for youngpplpart_sco
    output$youngpplpart_sco_graph <- renderDygraph({
      dygraph(
        youngpplpart_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_youngpplpart_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for skillshortage_sco
    output$skillshortage_sco_graph <- renderDygraph({
      dygraph(
        skillshortage_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_skillshortage_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for worklessness_sco
    output$worklessness_sco_graph <- renderDygraph({
      dygraph(
        worklessness_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_worklessness_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
    
    
    
    # Leaflet map for eactivity_reg
    output$eactivity_reg_map_caption <- renderText({
      paste("Map 1. Total economic activity rate (16-64) ", as.character(input$eactivity_reg_input), sep="")
    })
    eactivity_reg_map_data <- reactive({
      eactivity_reg_one <- eactivity_reg[ which(eactivity_reg$Year == input$eactivity_reg_input), ]
      eactivity_reg_one <- eactivity_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, eactivity_reg_one, by = intersect(names(mapex@data), names(eactivity_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_eactivity_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$eactivity, bins = 9)
      list_return <- list(mapex = mapex, choropleth_eactivity_reg = choropleth_eactivity_reg)
      return(list_return)
    })
    output$eactivity_reg_map <- renderLeaflet({
      mapex <- eactivity_reg_map_data()$mapex
      choropleth_eactivity_reg <- eactivity_reg_map_data()$choropleth_eactivity_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_eactivity_reg(mapex$eactivity), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$eactivity), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_eactivity_reg, values = mapex$eactivity, title = paste("Economic Activity", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    # Leaflet map for lwage_reg
    output$lwage_reg_map_caption <- renderText({
      paste("Map 2. Percentage of Employees Earning Below the Living Wage ", as.character(input$lwage_reg_input), sep="")
    })
    lwage_reg_map_data <- reactive({
      lwage_reg_one <- lwage_reg[ which(lwage_reg$Year == input$lwage_reg_input), ]
      lwage_reg_one <- lwage_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, lwage_reg_one, by = intersect(names(mapex@data), names(lwage_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_lwage_reg <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), mapex@data$lwage, bins = 9)
      list_return <- list(mapex = mapex, choropleth_lwage_reg = choropleth_lwage_reg)
      return(list_return)
    })
    output$lwage_reg_map <- renderLeaflet({
      mapex <- lwage_reg_map_data()$mapex
      choropleth_lwage_reg <- lwage_reg_map_data()$choropleth_lwage_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_lwage_reg(mapex$lwage), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$lwage), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_lwage_reg, values = mapex$lwage, title = paste("% earning below living wage", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    # Leaflet map for gpaygap_reg
    output$gpaygap_reg_map_caption <- renderText({
      paste("Map 3. Median hourly gender pay gap - full time employees ", as.character(input$gpaygap_reg_input), sep="")
    })
    gpaygap_reg_map_data <- reactive({
      gpaygap_reg_one <- gpaygap_reg[ which(gpaygap_reg$Year == input$gpaygap_reg_input), ]
      gpaygap_reg_one <- gpaygap_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, gpaygap_reg_one, by = intersect(names(mapex@data), names(gpaygap_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_gpaygap_reg <- colorBin(palette=brewer.pal(n=9, name="PuOr"), mapex@data$gpaygap, bins = 9)
      list_return <- list(mapex = mapex, choropleth_gpaygap_reg = choropleth_gpaygap_reg)
      return(list_return)
    })
    output$gpaygap_reg_map <- renderLeaflet({
      mapex <- gpaygap_reg_map_data()$mapex
      choropleth_gpaygap_reg <- gpaygap_reg_map_data()$choropleth_gpaygap_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_gpaygap_reg(mapex$gpaygap), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$gpaygap), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_gpaygap_reg, values = mapex$gpaygap, title = paste("Gender Pay Gap", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    # Leaflet map for wplearning_reg
    output$wplearning_reg_map_caption <- renderText({
      paste("Map 4. Percentage of All Employees Who received Job Related Training in the last 13 weeks (aged 16-64) ", as.character(input$wplearning_reg_input), sep="")
    })
    wplearning_reg_map_data <- reactive({
      wplearning_reg_one <- wplearning_reg[ which(wplearning_reg$Year == input$wplearning_reg_input), ]
      wplearning_reg_one <- wplearning_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, wplearning_reg_one, by = intersect(names(mapex@data), names(wplearning_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_wplearning_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$wplearning, bins = 9)
      list_return <- list(mapex = mapex, choropleth_wplearning_reg = choropleth_wplearning_reg)
      return(list_return)
    })
    output$wplearning_reg_map <- renderLeaflet({
      mapex <- wplearning_reg_map_data()$mapex
      choropleth_wplearning_reg <- wplearning_reg_map_data()$choropleth_wplearning_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_wplearning_reg(mapex$wplearning), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$wplearning), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_wplearning_reg, values = mapex$wplearning, title = paste("%", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for worklessness_reg
    output$worklessness_reg_map_caption <- renderText({
      paste("Map 5. Proportion of households classified as workless ", as.character(input$worklessness_reg_input), sep="")
    })
    worklessness_reg_map_data <- reactive({
      worklessness_reg_one <- worklessness_reg[ which(worklessness_reg$Year == input$worklessness_reg_input), ]
      worklessness_reg_one <- worklessness_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, worklessness_reg_one, by = intersect(names(mapex@data), names(worklessness_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_worklessness_reg <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_worklessness_reg = choropleth_worklessness_reg)
      return(list_return)
    })
    output$worklessness_reg_map <- renderLeaflet({
      mapex <- worklessness_reg_map_data()$mapex
      choropleth_worklessness_reg <- worklessness_reg_map_data()$choropleth_worklessness_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_worklessness_reg(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_worklessness_reg, values = mapex$Value, title = paste("%", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # PEOPLE #########################################################################################################################################
    
    # Horizontal bar - lifeexpall_bar
    output$lifeexpall_bar <- renderUI({
      if (rank_depratio_quantile >= 0.75) {
        column(width = 2,
               tags$b("Life expectancy: "),
               tags$p(as.character(rank_lifeexpall), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_depratio_quantile <= 0.25) {
        column(width = 2,
               tags$b("Life expectancy: "),
               tags$p(as.character(rank_lifeexpall), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Life expectancy: "),
               tags$p(as.character(rank_lifeexpall), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    # ggplot barplot for lifeexpall_overview
    output$lifeexpall_overview_int_barplot <- renderPlot({
      lifeexpall_overview_int2 <- lifeexpall_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(lifeexpall_overview_int$Country == "Scotland","blue","grey"))
      ggplot(lifeexpall_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(lifeexpall_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="Years") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Life expectancy") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    
    # Dygraph for lifeexpall_int
    output$lifeexpall_int_graph <- renderDygraph({
      dygraph(
        lifeexpall_int_wide[,c(1, as.numeric(input$lifeexpall_int_input))]
      ) %>%
        dyGroup(names(lifeexpall_int_wide)[c(1, as.numeric(input$lifeexpall_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percantage") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_lifeexpall_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for rphousingc_sco
    output$rphousingc_sco_graph <- renderDygraph({
      dygraph(
        rphousingc_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percentage Of People") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_rphousingc_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for cpoverty_sco
    output$cpoverty_sco_graph <- renderDygraph({
      dygraph(
        cpoverty_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year (last year of 3-year average)", rangePad = 5) %>%
        dyAxis("y", label = "Percentage of children") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_cpoverty_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
 
    # Dygraph for pratio_sco
    output$pratio_sco_graph <- renderDygraph({
      dygraph(
        pratio_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year (last year of 3-year average)", rangePad = 5) %>%
        dyAxis("y", label = "Ratio") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_pratio_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
             # ggplot barplot for hlifeexp_male_overview_int
    output$hlifeexp_male_overview_sco_barplot <- renderPlot({
      hlifeexp_male_overview_sco <- hlifeexp_male_sco %>%
        mutate(Age = (Age),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(hlifeexp_male_overview_sco, aes(x=Age, y=Value)) +
        geom_col(fill=factor(hlifeexp_male_overview_sco$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("") +
        geom_text(aes(x=Age, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # ggplot barplot for hlifeexp_female_overview_sco
    output$hlifeexp_female_overview_sco_barplot <- renderPlot({
      hlifeexp_female_overview_sco <- hlifeexp_female_sco %>%
        mutate(Age = (Age),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(hlifeexp_female_overview_sco, aes(x=Age, y=Value)) +
        geom_col(fill=factor(hlifeexp_female_overview_sco$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("") +
        geom_text(aes(x=Age, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
        # Dygraph for hlifeexp_female_sco
    output$hlifeexp_female_sco_graph <- renderDygraph({
      dygraph(
        hlifeexp_female_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year marking end of 3-year average",  rangePad = 5) %>%
        dyAxis("y", label = "Healthy life expectancy at birth",valueRange = c(55,70) ) %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_hlifeexp_female_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
            # Dygraph for hlifeexp_male_sco
    output$hlifeexp_male_sco_graph <- renderDygraph({
      dygraph(
        hlifeexp_male_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year marking end of 3-year average", rangePad = 5) %>%
        dyAxis("y", label = "Healthy life expectancy at birth",  valueRange = c(55,70)) %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_hlifeexp_male_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
    # Dygraph for mwellbeing_sco
    output$mwellbeing_sco_graph <- renderDygraph({
      dygraph(
        mwellbeing_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "SWEMWBS Score") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_mwellbeing_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for scapital_sco
    output$scapital_sco_graph <- renderDygraph({
      dygraph(
        scapital_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Social Capital Index") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_scapital_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
       # Leaflet map for hlifeexp_male_reg
    output$hlifeexp_male_reg_map_caption <- renderText({
      paste("Map 4. Healthy life expectancy (male) ", as.character(input$hlifeexp_male_reg_input), sep="")
    })
    hlifeexp_male_reg_map_data <- reactive({
      hlifeexp_male_reg_one <- hlifeexp_male_reg[ which(hlifeexp_male_reg$Year == input$hlifeexp_male_reg_input), ]
      hlifeexp_male_reg_one <- hlifeexp_male_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, hlifeexp_male_reg_one, by = intersect(names(mapex@data), names(hlifeexp_male_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_hlifeexp_male_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$hlifeexp_male, bins = 9)
      list_return <- list(mapex = mapex, choropleth_hlifeexp_male_reg = choropleth_hlifeexp_male_reg)
      return(list_return)
    })
    output$hlifeexp_male_reg_map <- renderLeaflet({
      mapex <- hlifeexp_male_reg_map_data()$mapex
      choropleth_hlifeexp_male_reg <- hlifeexp_male_reg_map_data()$choropleth_hlifeexp_male_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_hlifeexp_male_reg(mapex$hlifeexp_male), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$hlifeexp_male), " year", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_hlifeexp_male_reg, values = mapex$hlifeexp_male, title = paste("Healthy life expectancy (male)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    # Leaflet map for hlifeexp_female_reg
    output$hlifeexp_female_reg_map_caption <- renderText({
      paste("Map 5. Healthy life expectancy (female) ", as.character(input$hlifeexp_female_reg_input), sep="")
    })
    hlifeexp_female_reg_map_data <- reactive({
      hlifeexp_female_reg_one <- hlifeexp_female_reg[ which(hlifeexp_female_reg$Year == input$hlifeexp_female_reg_input), ]
      hlifeexp_female_reg_one <- hlifeexp_female_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, hlifeexp_female_reg_one, by = intersect(names(mapex@data), names(hlifeexp_female_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_hlifeexp_female_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$hlifeexp_female, bins = 9)
      list_return <- list(mapex = mapex, choropleth_hlifeexp_female_reg = choropleth_hlifeexp_female_reg)
      return(list_return)
    })
    output$hlifeexp_female_reg_map <- renderLeaflet({
      mapex <- hlifeexp_female_reg_map_data()$mapex
      choropleth_hlifeexp_female_reg <- hlifeexp_female_reg_map_data()$choropleth_hlifeexp_female_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_hlifeexp_female_reg(mapex$hlifeexp_female), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$hlifeexp_female), " year", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_hlifeexp_female_reg, values = mapex$hlifeexp_female, title = paste("Healthy life expectancy (female)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for cpoverty_reg
    output$cpoverty_reg_map_caption <- renderText({
      paste("Map 5. Child poverty (% in low income families after housing costs) ", as.character(input$cpoverty_reg_input), sep="")
    })
    cpoverty_reg_map_data <- reactive({
      cpoverty_reg_one <- cpoverty_reg[ which(cpoverty_reg$Year == input$cpoverty_reg_input), ]
      cpoverty_reg_one <- cpoverty_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, cpoverty_reg_one, by = intersect(names(mapex@data), names(cpoverty_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_cpoverty_reg <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_cpoverty_reg = choropleth_cpoverty_reg)
      return(list_return)
    })
    output$cpoverty_reg_map <- renderLeaflet({
      mapex <- cpoverty_reg_map_data()$mapex
      choropleth_cpoverty_reg <- cpoverty_reg_map_data()$choropleth_cpoverty_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_cpoverty_reg(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), "%", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_cpoverty_reg, values = mapex$Value, title = paste("Child poverty", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for noquals_reg
    output$noquals_reg_map_caption <- renderText({
      paste("Map 6. Proportion of population aged 16-64 with no qualifications ", as.character(input$noquals_reg_input), sep="")
    })
    noquals_reg_map_data <- reactive({
      noquals_reg_one <- noquals_reg[ which(noquals_reg$Year == input$noquals_reg_input), ]
      noquals_reg_one <- noquals_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, noquals_reg_one, by = intersect(names(mapex@data), names(noquals_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_noquals_reg <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_noquals_reg = choropleth_noquals_reg)
      return(list_return)
    })
    output$noquals_reg_map <- renderLeaflet({
      mapex <- noquals_reg_map_data()$mapex
      choropleth_noquals_reg <- noquals_reg_map_data()$choropleth_noquals_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_noquals_reg(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), "%", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_noquals_reg, values = mapex$Value, title = paste("% No qualifications", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # PLACE ##########################################################################################################################################
    # Horizontal bar - broadband
    output$broadband_bar <- renderUI({
      if (rank_broadband_quantile >= 0.75) {
        column(width = 2,
               tags$b("Broadband: "),
               tags$p(as.character(rank_broadband), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_broadband_quantile <= 0.25) {
        column(width = 2,
               tags$b("Broadband: "),
               tags$p(as.character(rank_broadband), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Broadband: "),
               tags$p(as.character(rank_broadband), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    
    # ggplot barplot for broadband_overview
    output$broadband_overview_int_barplot <- renderPlot({
      broadband_overview_int2 <- broadband_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(broadband_overview_int$Country == "Scotland","blue","grey"))
      ggplot(broadband_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(broadband_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="% of households") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Broadband (% of households)") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # Dygraph for broadband_int
    output$broadband_int_graph <- renderDygraph({
      dygraph(
        broadband_int_wide[,c(1, as.numeric(input$broadband_int_input))]
      ) %>%
        dyGroup(names(broadband_int_wide)[c(1, as.numeric(input$broadband_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percentage") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_broadband_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for greenandbluespace_sco
    output$greenandbluespace_sco_graph <- renderDygraph({
      dygraph(
        greenandbluespace_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_greenandbluespace_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for atravel_sco
    output$atravel_sco_graph <- renderDygraph({
      dygraph(
        atravel_sco_wide[,c(1, as.numeric(input$atravel_sco_input))]
      ) %>%
        dyGroup(names(atravel_sco_wide)[c(1, as.numeric(input$atravel_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Percentage of journeys (<2 miles for walking, <5 miles for cycling)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_atravel_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })   
    
    # Leaflet map for bgreen_reg
    output$bgreen_reg_map_caption <- renderText({
      paste("Map 2. Percentage of adults living within five minutes' walk of their nearest green or blue space ", as.character(input$bgreen_reg_input), sep="")
    })
    bgreen_reg_map_data <- reactive({
      bgreen_reg_one <- bgreen_reg[ which(bgreen_reg$Year == input$bgreen_reg_input), ]
      bgreen_reg_one <- bgreen_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, bgreen_reg_one, by = intersect(names(mapex@data), names(bgreen_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$Name),]
      choropleth_bgreen_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$blueorgreen, bins = 9)
      list_return <- list(mapex = mapex, choropleth_bgreen_reg = choropleth_bgreen_reg)
      return(list_return)
    })
    output$bgreen_reg_map <- renderLeaflet({
      mapex <- bgreen_reg_map_data()$mapex
      choropleth_bgreen_reg <- bgreen_reg_map_data()$choropleth_bgreen_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$Name, fillColor = ~choropleth_bgreen_reg(mapex$blueorgreen), fillOpacity=1, popup = ~paste(as.character(mapex$Name), " ", as.character(mapex$blueorgreen), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_bgreen_reg, values = mapex$blueorgreen, title = paste("%", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    
    
     # Leaflet map for pubservsat_reg
    output$pubservsat_reg_map_caption <- renderText({
      paste("Map 2. Percentage of the population very or fairly satisfied with the quality of public services delivered (local health services,local schools and public transport) ", as.character(input$pubservsat_reg_input), sep="")
    })
    pubservsat_reg_map_data <- reactive({
      pubservsat_reg_one <- pubservsat_reg[ which(pubservsat_reg$Year == input$pubservsat_reg_input), ]
      pubservsat_reg_one <- pubservsat_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, pubservsat_reg_one, by = intersect(names(mapex@data), names(pubservsat_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_pubservsat_reg <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$pubservsat, bins = 9)
      list_return <- list(mapex = mapex, choropleth_pubservsat_reg = choropleth_pubservsat_reg)
      return(list_return)
    })
    output$pubservsat_reg_map <- renderLeaflet({
      mapex <- pubservsat_reg_map_data()$mapex
      choropleth_pubservsat_reg <- pubservsat_reg_map_data()$choropleth_pubservsat_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_pubservsat_reg(mapex$pubservsat), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$pubservsat), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_pubservsat_reg, values = mapex$pubservsat, title = paste("% satisfied", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for airqual_reg
    output$airqual_reg_map_caption <- renderText({
      paste("Map 2. Daily Air Quality Index ", as.character(input$airqual_reg_input), sep="")
    })
    airqual_reg_map_data <- reactive({
      airqual_reg_one <- airqual_reg[ which(as.Date(airqual_reg$Date) == input$airqual_reg_input), ]
      airqual_reg_one <- airqual_reg_one[ ,c(2,3)]
      merged <- merge(mapex@data, airqual_reg_one, by = intersect(names(mapex@data), names(airqual_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$`LA Name`),]
      choropleth_airqual_reg <- colorBin(palette=brewer.pal(n=3, name="RdYlGn"), mapex@data$DAQI, bins = 3)
      list_return <- list(mapex = mapex, choropleth_airqual_reg = choropleth_airqual_reg)
      return(list_return)
    })
    output$airqual_reg_map <- renderLeaflet({
      mapex <- airqual_reg_map_data()$mapex
      choropleth_airqual_reg <- airqual_reg_map_data()$choropleth_airqual_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$`LA Name`, fillColor = ~choropleth_airqual_reg(mapex$DAQI), fillOpacity=1, popup = ~paste(as.character(mapex$`LA Name`), " ", as.character(mapex$DAQI), " ", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_airqual_reg, values = mapex$DAQI, title = paste("Daily Air Quality Index", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    
    # Leaflet map for broadband_reg
    output$broadband_reg_map_caption <- renderText({
      paste("Map 3. Proportion of residential and non-residential addresses where superfast broadband is not available (as a Percentage) ", as.character(input$broadband_reg_input), sep="")
    })
    broadband_reg_map_data <- reactive({
      broadband_reg_one <- broadband_reg[ which(broadband_reg$Year == input$broadband_reg_input), ]
      broadband_reg_one <- broadband_reg_one[ ,c(1,3)]
      merged <- merge(mapex@data, broadband_reg_one, by = intersect(names(mapex@data), names(broadband_reg_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_broadband_reg <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), mapex@data$broadband, bins = 9)
      list_return <- list(mapex = mapex, choropleth_broadband_reg = choropleth_broadband_reg)
      return(list_return)
    })
    output$broadband_reg_map <- renderLeaflet({
      mapex <- broadband_reg_map_data()$mapex
      choropleth_broadband_reg <- broadband_reg_map_data()$choropleth_broadband_reg
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_broadband_reg(mapex$broadband), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$broadband), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_broadband_reg, values = mapex$broadband, title = paste("% who don't have access", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # SUSTAINABILITY #################################################################################################################################
    # Horizontal bar - ggemissions2
    output$ggemissions2_bar <- renderUI({
      if (rank_ggemissions2_quantile >= 0.75) {
        column(width = 2,
               tags$b("Greenhouse Gas Emissions: "),
               tags$p(as.character(rank_ggemissions2), style="font-size: 48px"),
               style="background-color: red;"
        )
      } else if (rank_ggemissions2_quantile <= 0.25) {
        column(width = 2,
               tags$b("Greenhouse Gas Emissions: "),
               tags$p(as.character(rank_ggemissions2), style="font-size: 48px"),
               style="background-color: green;"
        )
      } else {
        column(width = 2,
               tags$b("Greenhouse Gas Emissions: "),
               tags$p(as.character(rank_ggemissions2), style="font-size: 48px"),
               style="background-color: orange;"
        )
      }
    })
    
    # ggplot barplot for ggemissions2_overview
    output$ggemissions2_overview_int_barplot <- renderPlot({
      ggemissions2_overview_int2 <- ggemissions2_overview_int %>%
        mutate(Country = (Country),
               Value = as.numeric(Value),
               fill_type = ifelse(ggemissions2_overview_int$Country == "Scotland","blue","grey"))
      ggplot(ggemissions2_overview_int2, aes(x=Country, y=Value)) +
        geom_col(fill=factor(ggemissions2_overview_int2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="Tonnes CO2e per capita") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Greenhouse Gas Emissions (Tonnes CO2e per capita)") +
        geom_text(aes(x=Country, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # Dygraph for ggemissions_int
    output$ggemissions_int_graph <- renderDygraph({
      dygraph(
        ggemissions_int_wide[,c(1, as.numeric(input$ggemissions_int_input))]
      ) %>%
        dyGroup(names(ggemissions_int_wide)[c(1, as.numeric(input$ggemissions_int_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Kilograms per capita, Thousands") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_ggemissions_int", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for cfootprint_sco
    output$cfootprint_sco_graph <- renderDygraph({
      dygraph(
        cfootprint_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Mt Co2Equiv") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_cfootprint_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for mfootprint_sco
    output$mfootprint_sco_graph <- renderDygraph({
      dygraph(
        mfootprint_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Tons per capita") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_mfootprint_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
    
    # Dygraph for naturalf_sco
    output$naturalf_sco_graph <- renderDygraph({
      dygraph(
        naturalf_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "%") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_naturalf_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for gasemissions_sco
    output$gasemissions_sco_graph <- renderDygraph({
      dygraph(
        gasemissions_sco
      ) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Kt Co2Equiv") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_gasemissions_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # Dygraph for airpollutant_sco
    output$airpollutant_sco_graph <- renderDygraph({
      dygraph(
        airpollutant_sco_wide[,c(1, as.numeric(input$airpollutant_sco_input))]
      ) %>%
        dyGroup(names(airpollutant_sco_wide)[c(1, as.numeric(input$airpollutant_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Air pollutant emissions (Kt)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_airpollutant_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
    # Dygraph for mandtspecies_sco
    output$mandtspecies_sco_graph <- renderDygraph({
      dygraph(
        mandtspecies_sco_wide[,c(1, as.numeric(input$mandtspecies_sco_input))]
      ) %>%
        dyGroup(names(mandtspecies_sco_wide)[c(1, as.numeric(input$mandtspecies_sco_input))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Index value") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_mandtspecies_sco", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # EQUALITIES DASHBOARD ###########################################################################################################################
    # gpaygap_eq_overview_int_lineplot
    output$gpaygap_eq_overview_int_lineplot <- renderPlot({
      ggplot(gpaygap_eq) +
        geom_line(stat="identity", aes(x=gpaygap_eq$Year, y=gpaygap_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
        labs(x = "Year", y = "%") +
        theme_minimal() +
        ggtitle("Gender Pay Gap - Full-time gender pay gap (%)") +
        scale_color_jama() +
        geom_point(aes(x=gpaygap_eq$Year, y=gpaygap_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
        geom_text(aes(x=gpaygap_eq$Year, y=gpaygap_eq$Value, label=gpaygap_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
    })
    
    # dempgap_eq_overview_int_lineplot
    output$dempgap_eq_overview_int_lineplot <- renderPlot({
      ggplot(dempgap_eq) +
        geom_line(stat="identity", aes(x=dempgap_eq$Year, y=dempgap_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
        labs(x = "Year", y = "%") +
        theme_minimal() +
        ggtitle("Disabled Employment Gap") +
        scale_color_jama() +
        geom_point(aes(x=dempgap_eq$Year, y=dempgap_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
        geom_text(aes(x=dempgap_eq$Year, y=dempgap_eq$Value, label=dempgap_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
    })
    
    # youthunemp_eq_overview_int_lineplot
    output$youthunemp_eq_overview_int_lineplot <- renderPlot({
      ggplot(youthunemp_eq) +
        geom_line(stat="identity", aes(x=youthunemp_eq$Year, y=youthunemp_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
        labs(x = "Year", y = "%") +
        theme_minimal() +
        ggtitle("Youth Unemployment Rate (16 - 24)") +
        scale_color_jama() +
        geom_point(aes(x=youthunemp_eq$Year, y=youthunemp_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
        geom_text(aes(x=youthunemp_eq$Year, y=youthunemp_eq$Value, label=youthunemp_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
    })
    
    # ethnicmgap_eq_overview_int_lineplot
    output$ethnicmgap_eq_overview_int_lineplot <- renderPlot({
      ggplot(ethnicmgap_eq) +
        geom_line(stat="identity", aes(x=ethnicmgap_eq$Year, y=ethnicmgap_eq$Value), color ="#00BFC4", show.legend = FALSE, size=1.5) +
        labs(x = "Year", y = "%") +
        theme_minimal() +
        ggtitle("Ethnic Minority Employment Gap") +
        scale_color_jama() +
        geom_point(aes(x=ethnicmgap_eq$Year, y=ethnicmgap_eq$Value), color = "#F8766D", show.legend = FALSE, size=4) +
        geom_text(aes(x=ethnicmgap_eq$Year, y=ethnicmgap_eq$Value, label=ethnicmgap_eq$Value), position=position_dodge(width=0.9), size = 4, vjust=-1)
    })
  
    # socioecon_eq_overview_int_barplot
    output$socioecon_eq_overview_int_barplot <- renderPlot({
      socioecon_eq2 <- socioecon_eq %>%
        mutate(SIMD = (SIMD),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(socioecon_eq2, aes(x=SIMD, y=Value)) +
        geom_col(fill=factor(socioecon_eq2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Employment gap between most and least deprived SIMD quintiles") +
        geom_text(aes(x=SIMD, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # religion_eq_overview_int_barplot
    output$religion_eq_overview_int_barplot <- renderPlot({
      religion_eq2 <- religion_eq %>%
        mutate(Religion = (Religion),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(religion_eq2, aes(x=Religion, y=Value)) +
        geom_col(fill=factor(religion_eq2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Adults in relative poverty AHC (below 60% of UK median income)") +
        geom_text(aes(x=Religion, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # orientation_eq_overview_int_barplot
    output$orientation_eq_overview_int_barplot <- renderPlot({
      orientation_eq2 <- orientation_eq %>%
        mutate(Orientation = (Orientation),
               Value = as.numeric(Value),
               fill_type = "grey")
      ggplot(orientation_eq2, aes(x=Orientation, y=Value)) +
        geom_col(fill=factor(orientation_eq2$fill_type), show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="%") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Experiences of discrimination") +
        geom_text(aes(x=Orientation, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
  } # ... server ends here.
)
