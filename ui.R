source("data.R")

# UI
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  # HEADING ##########################################################################################################################################
  navbarPage(id = "MainNav",
             windowTitle = "Wellbeing Economy Monitor",
             title = div(
               span(a(img(src = "Govscot_logo_white.png", height=20), href = "https://www.gov.scot/"), style = "padding-right:40px;"),
               span("Wellbeing Economy Monitor")
             ),
             # HOME PAGE #############################################################################################################################
             tabPanel("Home",
                      tags$head(tags$link(rel="shortcut icon", src="./www/favicon.ico")),
                      h1("Scotland's Wellbeing Economy Monitor", style="text-align: center"),
                      p("Comparison of Scotland's rank among a comparable group of countries (Finland, Denmark, New Zealand, Iceland, Sweden, Norway, UK) across a range of indicators.", style="text-align: center"),
                      fluidRow(
                        column(width = 4,
                               strong("Greenhouse Gas Emissions (2017)"),
                               p("Value: 7.47 [t CO2 per capita]"),
                               p("Average: 6.73 [t CO2 per capita]"),
                               p("Min: 1.96 [t CO2 per capita]"),
                               p("Max: 9.57 [t CO2 per capita]"),
                               p("*excluding: Norway, New Zealand, Iceland"),
                               strong("Research and Development (2019)"),
                               p("Value: 0.84%"),
                               p("Average: 1.55%"),
                               p("Min: 0.84%"),
                               p("Max: 2.43%"),
                               p("*excluding: New Zealand"),
                               strong("Productivity (2019)"),
                               p("Value: 82.6%"),
                               p("Average: 93.9%"),
                               p("Min: 61.1%"),
                               p("Max: 121%"),
                               strong("Growth (2019)"),
                               p("Value: 0.8%"),
                               p("Average: 1.78%"),
                               p("Min: 0.8%"),
                               p("Max: 2.43%"),
                               strong("Exporting (International) (2019)"),
                               p("Value: 20.13%"),
                               p("Average: 40.76%"),
                               p("Min: 20.80%"),
                               p("Max: 59.00%"),
                               strong("Exporting (Int & rUK) (2019)"),
                               p("Value: 52.88%"),
                               p("Average: 40.76%"),
                               p("Min: 26.99%"),
                               p("Max: 59.00%"),
                            #   strong("Economic Activity (2018)"),
                            #   p("Value: 77.4%"),
                            #   p("Average: 80.3%"),
                            #   p("Min: 77.4%"),
                            #   p("Max: 87.28%")
                        ),
                        column(width = 5,
                               plotOutput("circular_barplot_home_ggplot", width = "100%", height = "800px")
                        ),
                        column(width = 3,
                               strong("Employment (2018)"),
                               p("Value: 74.1%"),
                               p("Average: 76.44%"),
                               p("Min: 72.21%"),
                               p("Max: 84.84%"),
                               strong("Skills (higher) (2018)"),
                               p("Value: 47.4%"),
                               p("Average: 43.6%"),
                               p("Min: 39.7%"),
                               p("Max: 47.4%"),
                               p("*excluding: Norway, New Zealand, Iceland"),
                               strong("Skills (lower) (2018)"),
                               p("Value: 20.5%"),
                               p("Average: 16.74%"),
                               p("Min: 10.8%"),
                               p("Max: 20.5%"),
                               p("*excluding: Norway, New Zealand, Iceland"),
                               strong("Gender Pay Gap (2018)"),
                               p("Value: 13.81%"),
                               p("Average: 10.44%"),
                               p("Min: 4.86%"),
                               p("Max: 18.86%"),
                               strong("Unemployment (2019)"),
                               p("Value: 3.6%"),
                               p("Average: 4.84%"),
                               p("Min: 3.6%"),
                               p("Max: 6.77%"),
                               strong("Life Expectancy (2019)"),
                               p("Value: 79.1 years"),
                               p("Average: 82.36 years"),
                               p("Min: 79.1 years"),
                               p("Max: 83.2 years"),
                               strong("Dependency Ratio (2018)"),
                               p("Value: 53.35%"),
                               p("Average: 56.07%"),
                               p("Min: 52.9%"),
                               p("Max: 60.94%")
                        ),
                        
                      )
             ), 
             # PRODUCTIVITY ##########################################################################################################################
             tabPanel(
               value = "ProductivityTab",
               title = tags$div(icon("tachometer-alt", lib = "font-awesome"), "Productivity"),
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 2,
                               tags$b("Scotland's rank among 37 OECD countries:")
                        ),
                        uiOutput("growth_bar"),
                        uiOutput("productivity_bar"),
                        uiOutput("exporting_bar"),
                        uiOutput("rd_bar"),
                        uiOutput("entrepreneurialism_bar"),
                        style="background-color: grey; color: white"
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Growth"),
                               plotOutput("growth_overview_int_barplot", height = "200px"),
                               p("Scotland's growth rate was ", text_scotland_thisyear_growth_int,"% in 2020, compared to ", text_oecd_thisyear_growth_int,"% in the OECD. Over the past 5 years, the average GDP growth rate was ", text_scotland_last5_growth_int ,"% in Scotland, compared to ", text_oecd_last5_growth_int, "% in the OECD overall.")
                        ),
                        column(width = 4,
                               h2("Productivity"),
                               plotOutput("productivity_overview_int_barplot", height = "200px"),
                               p("Scotland's productivity was ", text_scotland_thisyear_productivity_int,"% of the USA's in 2018, compared to ", text_oecd_thisyear_productivity_int,"% in the OECD. Over the past 5 years, the average productivity was ", text_scotland_last5_productivity_int ,"% in Scotland, compared to ", text_oecd_last5_productivity_int, "% in the OECD overall.")
                        ),
                        column(width = 4,
                               h2("Reputation"),
                               plotOutput("reputation_overview_sco_lineplot", height = "200px")
                        )
                        
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Exporting"),
                               plotOutput("exporting_overview_int_barplot", height = "200px"),
                               p("Scotland's exporting* as % of GDP was ", text_scotland_thisyear_exporting_int,"% in 2020, compared to ", text_oecd_thisyear_exporting_int,"% in the OECD. Over the past 5 years, the average exporting as % of GDP was ", text_scotland_last5_exporting_int ,"% in Scotland, compared to ", text_oecd_last5_exporting_int, "% in the OECD overall."),
                               p("*(International & RUK)")
                        ),
                        column(width = 4,
                               h2("R&D"),
                               plotOutput("rd_overview_int_barplot", height = "200px"),
                               p("Scotland's expenditure on R&D as % of GDP was ", text_scotland_thisyear_rd_int,"% in 2019, compared to ", text_oecd_thisyear_rd_int,"% in the OECD. Over the past 5 years, the average expenditure on R&D as % of GDP was ", text_scotland_last5_rd_int ,"% in Scotland, compared to ", text_oecd_last5_rd_int, "% in the OECD overall.")
                               # Pie chart
                               # plotOutput("rd_overview_sco_pieplot", height = "170px")
                        ),
                        column(width = 4,
                               h2("Business"),
                               p("In", text_year_entrepreneurialism_sco, ", in Scotland, ", text_value_entrepreneurialism_sco, "% of people were actively trying to start a business, or owned/managed a business which is less than 3.5 years old."),
                               p("In 2020, there were 179,460 registered businesses in Scotland.", "Of them, 3,010 were foreign-owned"),
                              
                        )
               ),
               # DETAILS ####
               h1("Details"),
                 tabsetPanel(
                 # INTERNATIONAL ####
                   tabPanel("International",
                            navlistPanel(widths=c(3,9),
                              tabPanel("Growth",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "growth_int_input",
                                                           label = "",
                                                           choiceNames = names(growth_int_wide),
                                                           choiceValues = c(seq(1:length(names(growth_int_wide)))),
                                                           selected = positions_selected_countries_growth_int
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 1. Gross Domestic Product (GDP) Growth Rate ", "(", as.character(start_year_growth_int), " - ", as.character(end_year_growth_int), ") ", "(expenditure approach)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("growth_int_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_growth_int"),
                                                         p("Source: "), 
                                                         a("OECD", href = "https://data.oecd.org/gdp/gross-domestic-product-gdp.htm"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),   
                              tabPanel("Productivity",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "productivity_int_input",
                                                           label = "",
                                                           choiceNames = names(productivity_int_wide),
                                                           choiceValues = c(seq(1:length(names(productivity_int_wide)))),
                                                           selected = positions_selected_countries_productivity_int
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 2. Estimated Productivity Levels across OECD Countries: ", "(", as.character(start_year_productivity_int), " - ", as.character(end_year_productivity_int), ") ", "(Current prices, and current PPPs, USA=100)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("productivity_int_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_productivity"),
                                                         p("Source: "), 
                                                         a("2017 Productivity levels from OECD Statistics Portal - data extracted on 4 February 2019", href = "https://stats.oecd.org/"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("Exporting",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "exporting_int_input",
                                                           label = "",
                                                           choiceNames = names(exporting_int_wide),
                                                           choiceValues = c(seq(1:length(names(exporting_int_wide)))),
                                                           selected = positions_selected_countries_exporting_int
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 3. Exports of goods and services (% of GDP) across OECD Countries: ", "(", as.character(start_year_exporting_int), " - ", as.character(end_year_exporting_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("exporting_int_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_exporting"),
                                                         p("Source: "), 
                                                         a("World Bank, Scottish Government Quarterly National Accounts Scotland", href = "https://webarchive.nrscotland.gov.uk/20170916204357/http://www.gov.scot/Topics/Statistics/Browse/Economy/QNAS"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("R&D",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "rd_int_input",
                                                           label = "",
                                                           choiceNames = names(rd_int_wide),
                                                           choiceValues = c(seq(1:length(names(rd_int_wide)))),
                                                           selected = positions_selected_countries_rd_int
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 4. Business Enterprise on R&D Expenditure performed in OECD countries: ", "(", as.character(start_year_rd_int), " - ", as.character(end_year_rd_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("rd_int_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_rd"),
                                                         p("Source: "), 
                                                         a("MSTI 2018/2, ONS & Scottish Government", href = "https://www.gov.scot/collections/economy-statistics/"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("Entrepreneurialism",
                                       fluidRow(width = 12,
                                                p(tags$b(paste("Figure 5. Percent of 18-64 population who are either a nascent entrepreneur or owner-manager of a new business (2018)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                plotOutput("entrepreneurialism2_overview_int_barplot")
                                                # column(width=3, 
                                                #        wellPanel(
                                                #          checkboxGroupInput(
                                                #            inputId = "entrepreneurialism_int_input",
                                                #            label = "",
                                                #            choiceNames = names(entrepreneurialism_int_wide),
                                                #            choiceValues = c(seq(1:length(names(entrepreneurialism_int_wide)))),
                                                #            selected = positions_selected_countries_entrepreneurialism_int
                                                #          )
                                                #        )
                                                # ),      
                                                # column(width=9, 
                                                #        fluidRow(
                                                #          p(tags$b(paste("Figure 5. % of 18-64 population who are either a nascent entrepreneur or owner-manager of a new business: ", "(", as.character(start_year_entrepreneurialism_int), " - ", as.character(end_year_entrepreneurialism_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                #          withSpinner(dygraphOutput("entrepreneurialism_int_graph"), type = 5),
                                                #          align = "center"
                                                #        ),
                                                #        fluidRow(
                                                #          textOutput("legendDivID_entrepreneurialism"),
                                                #          p("Source: "), 
                                                #          a("Total early-stage Entrepreneurial Activity (TEA)", href = "d"),
                                                #          collapsible = FALSE,
                                                #          width = 12,
                                                #          style="margin-bottom: 100px;"
                                                #        )
                                                # )
                                       )
                              )
                            )
                   ),
                 # SCOTLAND ####
                   tabPanel("Scotland",
                            navlistPanel(widths=c(3,9),
                              tabPanel("Growth",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "growth_sco_input",
                                                           label = "",
                                                           choiceNames = names(growth_sco_wide),
                                                           choiceValues = c(seq(1:length(names(growth_sco_wide)))),
                                                           selected = positions_selected_sectors_growth_sco
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 1. Scotland's GDP Growth Rate by Sector", "(", as.character(start_year_growth_sco), " - ", as.character(end_year_growth_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("growth_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_growth_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("Productivity",
                                       fluidRow(width = 12,
                                                column(width=12, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 2. Real output per hour: ", "(", as.character(start_year_productivity_sco), " - ", as.character(end_year_productivity_sco), ") ", " (Index (2007=100))", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("productivity_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_productivity_sco"),
                                                         p("Source: "), 
                                                         a("Scottish Government", href = "https://www.gov.scot/publications/labour-productivity-statistics-2019-q4"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("Exporting",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "exporting_destination_sco_input",
                                                           label = "",
                                                           choiceNames = names(exporting_destination_sco_wide),
                                                           choiceValues = c(seq(1:length(names(exporting_destination_sco_wide)))),
                                                           selected = positions_selected_indices_exporting_destination_sco
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 3. Scotland's Exports by Destination ", "(", as.character(start_year_exporting_destination_sco), " - ", as.character(end_year_exporting_destination_sco), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("exporting_destination_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_exporting_destination_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       ),
            
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "exporting_sector_sco1_input",
                                                           label = "",
                                                           choiceNames = names(exporting_sector_sco1_wide),
                                                           choiceValues = c(seq(1:length(names(exporting_sector_sco1_wide)))),
                                                           selected = positions_selected_indices_exporting_sector_sco1
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 3. Scotland's exports by industry ", "(", as.character(start_year_exporting_sector_sco1), " - ", as.character(end_year_exporting_sector_sco1), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("exporting_sector_sco1_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_exporting_sector_sco1"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ), 
                              
                              tabPanel("R&D",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "rd_sco_input",
                                                           label = "",
                                                           choiceNames = names(rd_sco_wide),
                                                           choiceValues = c(seq(1:length(names(rd_sco_wide)))),
                                                           selected = positions_selected_indices_rd_sco
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 5. Expenditure on Research and Development in Scotland as a percentage of GDP ", "(", as.character(start_year_rd_sco), " - ", as.character(end_year_rd_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("rd_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_rd_sco"),
                                                         p("Source: "), 
                                                         a("Open Data Platform", href = "https://statistics.gov.scot/data/gross-expenditure-on-research-and-development"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("Entrepreneurialism",
                                       fluidRow(width = 12,
                                                column(width=12, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 6. Total Early-stage Entrepreneurial Activity (TEA) rate: ", "(", as.character(start_year_entrepreneurialism_sco), " - ", as.character(end_year_entrepreneurialism_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("entrepreneurialism_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_entrepreneurialism_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       ),
                                                       fluidRow(
                                                         width = 12,
                                                         p("Total Early-stage Entrepreneurial Activity (TEA) rate: proportion of the adult working age population that is actively trying to start a business, or that own/manage a business which is less than 3.5 years old.")
                                                       ),
                                                )
                                       )
                              ),
                              tabPanel("Scotland's reputation",
                                       fluidRow(width = 12,
                                                column(width=12, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 7. Anholt GfK-Roper Nation Brands Index (NBI): ", "(", as.character(start_year_reputation_sco), " - ", as.character(end_year_reputation_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("reputation_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_reputation_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       ),
                                                       fluidRow(
                                                         width = 12,
                                                         p("Anholt GfK-Roper Nation Brands Index (NBI): Average scores of the six dimensions of national competence, given as a value (not percentage) out of 100.")
                                                       ),
                                                )
                                       )
                              ),
                              tabPanel("The number of businesses",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "nbusiness_sector_sco_input",
                                                           label = "",
                                                           choiceNames = names(nbusiness_sector_sco_wide),
                                                           choiceValues = c(seq(1:length(names(nbusiness_sector_sco_wide)))),
                                                           selected = positions_selected_sector_nbusiness_sector_sco
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 8. Total number of private sector enterprises (registered for VAT and/orPAYE) in Scotland per 10,000 adults by Sector ", "(", as.character(start_year_nbusiness_sector_sco), " - ", as.character(end_year_nbusiness_sector_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("nbusiness_sector_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_nbusiness_sector_sco"),
                                                         p("Source: "), 
                                                         a("Businesses in Scotland:2020", href = "https://www.gov.scot/publications/businesses-in-scotland-2020/"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       ),
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "nbusiness_region_sco_input",
                                                           label = "",
                                                           choiceNames = names(nbusiness_region_sco_wide),
                                                           choiceValues = c(seq(1:length(names(nbusiness_region_sco_wide)))),
                                                           selected = positions_selected_region_nbusiness_region_sco
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 9. Number of businesses in Scotland by Region of Ownership ", "(", as.character(start_year_nbusiness_region_sco), " - ", as.character(end_year_nbusiness_region_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("nbusiness_region_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_nbusiness_region_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       ),
                                      # fluidRow(width = 12,
                                      #          column(width=3, 
                                      #                 wellPanel(
                                      #                   checkboxGroupInput(
                                      #                     inputId = "nbusiness_employees_sco_input",
                                      #                     label = "",
                                      #                     choiceNames = names(nbusiness_employees_sco),
                                      #                     choiceValues = c(seq(1:length(names(nbusiness_employees_sco)))),
                                      #                     selected = positions_selected_employees_nbusiness_employees_sco
                                      #                   )
                                      #                 )
                                      #          ),      
                                      #          column(width=9, 
                                      #                 fluidRow(
                                      #                   p(tags$b(paste("Figure 10. Total number of private sector enterprises (registered for VAT and/or PAYE) in Scotland per 10,000 adults by Number of Employees ", "(", as.character(start_year_nbusiness_employees_sco), " - ", as.character(end_year_nbusiness_employees_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                      #                   withSpinner(dygraphOutput("nbusiness_employees_sco_graph"), type = 5),
                                      #                   align = "center"
                                      #                 ),
                                      #                 fluidRow(
                                      #                   textOutput("legendDivID_nbusiness_employees_sco"),
                                      #                   p("Source: "), 
                                      #                   a("Businesses in Scotland:2020", href = "https://www.gov.scot/publications/businesses-in-scotland-2020/"),
                                      #                   collapsible = FALSE,
                                      #                   width = 12,
                                      #                   style="margin-bottom: 100px;"
                                      #                 )
                                      #          )
                                      # )

                              ),
                              tabPanel("High growth businesses",
                                       fluidRow(width = 12,
                                                column(width=12, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 11. Percentage share of High Growth stocks ", "(", as.character(start_year_hbusiness_percent_sco), " - ", as.character(end_year_hbusiness_percent_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("hbusiness_percent_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_hbusiness_percent_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              tabPanel("Innovative businesses",
                                       fluidRow(width = 12,
                                                column(width=3, 
                                                       wellPanel(
                                                         checkboxGroupInput(
                                                           inputId = "ibusiness_sco_input",
                                                           label = "",
                                                           choiceNames = names(ibusiness_sco_wide),
                                                           choiceValues = c(seq(1:length(names(ibusiness_sco_wide)))),
                                                           selected = positions_selected_activities_ibusiness_sco
                                                         )
                                                       )
                                                ),      
                                                column(width=9, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 12. Share of businesses involved in innovation activities ", "(", as.character(start_year_ibusiness_sco), " - ", as.character(end_year_ibusiness_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("ibusiness_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_ibusiness_sco"),
                                                         p("Source: "), 
                                                         a("ODP", href = "https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fbusiness-innovation"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              ),
                              
                              tabPanel("Natural Capital Asset Index",
                                       fluidRow(width = 12,
                                                column(width=12, 
                                                       fluidRow(
                                                         p(tags$b(paste("Figure 13. Natural Capital Asset Index ", "(", as.character(start_year_ncai_sco), " - ", as.character(end_year_ncai_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                         withSpinner(dygraphOutput("ncai_sco_graph"), type = 5),
                                                         align = "center"
                                                       ),
                                                       fluidRow(
                                                         textOutput("legendDivID_ncai_sco"),
                                                         p("Source: "), 
                                                         a("Natural Capital Asset Index 2021", href = "https://www.nature.scot"),
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         style="margin-bottom: 100px;"
                                                       )
                                                )
                                       )
                              )
                              
                            )
                   ),
                 # REGIONAL ####
                   tabPanel("Regional",
                            navlistPanel(widths=c(3,9),
                                         tabPanel("Productivity",
                                                  fluidRow(width = 12,
                                                    column(width=12,
                                                           tags$b(textOutput("GVA_reg_map_caption")),
                                                           sliderInput("GVA_reg_input", label = "", min = start_year_GVA_reg , max = end_year_GVA_reg, value = end_year_GVA_reg, width = "50%", sep = "", step = 1),
                                                      withSpinner(leafletOutput("GVA_reg_map"), type = 5),
                                                      p("Source: "), 
                                                      a("Office for National Statistics", href = "https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/regionalgrossvalueaddedbalancedbylocalauthorityintheuk")
                                                    )
                                                  )
                                         ),
                                        # tabPanel("Exporting",
                                        #          fluidRow(width = 12,
                                         #                  column(width=12,
                                         #                         tags$b(textOutput("exporting_reg_map_caption")),
                                         #                         sliderInput("exporting_reg_input", label = "", min = start_year_exporting_reg , max = end_year_exporting_reg, value = end_year_exporting_reg, width = "50%", sep = "", step = 1),
                                         #                         withSpinner(leafletOutput("exporting_reg_map"), type = 5),
                                         #                         p("Source: "), 
                                         #                         a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/")
                                         #                  )
                                          #        )
                                        # ),
                                         tabPanel("R&D",
                                                  fluidRow(width = 12,
                                                           column(width=12,
                                                                  tags$b(textOutput("rd_reg_map_caption")),
                                                                  sliderInput("rd_reg_input", label = "", min = start_year_rd_reg , max = end_year_rd_reg, value = end_year_rd_reg, width = "50%", sep = "", step = 1),
                                                                  withSpinner(leafletOutput("rd_reg_map"), type = 5),
                                                                  p("Source: "), 
                                                                  a("National Statistics publication for Scotland", href = "https://www.gov.scot/publications/business-enterprise-research-and-development-2019/")
                                                           )
                                                  )
                                         ),
                                         tabPanel("Number of businesses",
                                                  fluidRow(width = 12,
                                                           column(width=12,
                                                                  # p(tags$b(paste("Map 4. Number of businesses per 10,000 adults across Scottish council areas in ", "YEAR", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                                  tags$b(textOutput("nbusiness_reg_map_caption")),
                                                                  sliderInput("nbusiness_reg_input", label = "", min = start_year_nbusiness_reg , max = end_year_nbusiness_reg, value = end_year_nbusiness_reg, width = "50%", sep = "", step = 1),
                                                                  withSpinner(leafletOutput("nbusiness_reg_map"), type = 5),
                                                                  p("Source: "), 
                                                                  a("Businesses in Scotland", href = "https://www.gov.scot/publications/businesses-in-scotland-2020/")
                                                           )
                                                  )
                                         )
                            )
                   )
                 )
             ),
             # POPULATION ############################################################################################################################
             tabPanel(
               value = "PopulationTab",
               title = tags$div(icon("user-friends", lib = "font-awesome"), "Population"),
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 2,
                               tags$b("Scotland's rank among 37 OECD countries:")
                        ),
                        uiOutput("depratio_bar"),
                        style="background-color: grey; color: white"
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Dependency ratio"),
                               plotOutput("depratio_overview_int_barplot", height = "200px"),
                               p("Scotland's dependency ratio was ", text_scotland_thisyear_depratio_int,"% in 2020, compared to ", text_oecd_thisyear_depratio_int,"% in the OECD. Over the past 5 years, the average dependency ratio was ", text_scotland_last5_depratio_int ,"% in Scotland, compared to ", text_oecd_last5_depratio_int, "% in the OECD overall.")
                               
                        ),
                        column(width = 4,
                        ),
                        column(width = 4,
                        )
                        
               ),
               # DETAILS ####
               h1("Details"),
               tabsetPanel(
                 # INTERNATIONAL ####
                 tabPanel("International",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Dependency ratio", 
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "depratio_int_input",
                                                         label = "",
                                                         choiceNames = names(depratio_int_wide),
                                                         choiceValues = c(seq(1:length(names(depratio_int_wide)))),
                                                         selected = positions_selected_countries_depratio_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Age dependency ratio ", "(", as.character(start_year_depratio_int), " - ", as.character(end_year_depratio_int), ") ", "(% of working-age population)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("depratio_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_depratio_int"),
                                                       p("Source: "), 
                                                       a("World Bank, National Records of Scotland ", href = "https://www.nrscotland.gov.uk/statistics-and-data"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Migration",
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "migration_int_input",
                                                         label = "",
                                                         choiceNames = names(migration_int_wide),
                                                         choiceValues = c(seq(1:length(names(migration_int_wide)))),
                                                         selected = positions_selected_countries_migration_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 2. Population change ", "(", as.character(start_year_migration_int), " - ", as.character(end_year_migration_int), ") ", "(crude rate of net migration plus statistical adjustment)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("migration_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_migration_int"),
                                                       p("Source: "), 
                                                       a("Eurostat", href = "https://ec.europa.eu/eurostat/home?"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )
                          )
                 ),
                 # SCOTLAND ####
                 tabPanel("Scotland",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Dependency ratio", 
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. The population of children (aged 0 to 15) and older people (aged 65 and over) expressed as a percentage of people aged 16 to 64 ", "(", as.character(start_year_depratio_sco), " - ", as.character(end_year_depratio_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("depratio_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_depratio_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Government", href = "https://statistics.gov.scot/data/population-estimates-dependency"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Migration",
                                     fluidRow(width = 12,
                                             # column(width=3,
                                             #        wellPanel(
                                             #          checkboxGroupInput(
                                             #            inputId = "migration_type_overseas_sco_input",
                                             #            label = "",
                                             #            choiceNames = names(migration_type_overseas_sco_wide),
                                             #           choiceValues = c(seq(1:length(names(migration_type_overseas_sco_wide)))),
                                             #            selected = positions_selected_types_migration_type_overseas_sco
                                              #         )
                                               #      )
                                              #),
                                             # column(width=9,
                                             #        fluidRow(
                                             #          p(tags$b(paste("Figure 1. Overseas migration to and from Scotland ", "(", as.character(start_year_migration_type_overseas_sco), " - ", as.character(end_year_migration_type_overseas_sco), ") ", "(absolute values)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                             #          withSpinner(dygraphOutput("migration_type_overseas_sco_graph"), type = 5),
                                             #          align = "center"
                                             #        ),
                                             #        fluidRow(
                                             #          textOutput("legendDivID_migration_type_overseas_sco"),
                                             #          p("Source: "), 
                                             #          a("ODP", href = "https://statistics.gov.scot/data/search"),
                                             #          collapsible = FALSE,
                                             #          width = 12,
                                             #          style="margin-bottom: 100px;"
                                             #       )
                                             # )
                                    # ),
                                     fluidRow(width = 12,
                                              column(width=3,
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "migration_in_source_sco_input",
                                                         label = "",
                                                         choiceNames = names(migration_in_source_sco_wide),
                                                         choiceValues = c(seq(1:length(names(migration_in_source_sco_wide)))),
                                                         selected = positions_selected_sources_migration_in_source_sco
                                                       )
                                                     )
                                              ),
                                              column(width=9,
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Migration to and from Scotland - Overseas vs Rest of the UK ", "(", as.character(start_year_migration_in_source_sco), " - ", as.character(end_year_migration_in_source_sco), ") ", "(absolute values)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("migration_in_source_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_migration_in_source_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Government", href = "https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fmigration-to-and-from-scotland&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fage=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fage%2Fall&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FmigrationType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fmigration-type%2Fnet&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fsex=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fsex%2Fall"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                              #       fluidRow(width = 12,
                               #               column(width=3,
                                #                     wellPanel(
                                 #                      checkboxGroupInput(
                                  #                       inputId = "migration_in_overseas_age_sco_input",
                                   #                      label = "",
                                    #                     choiceNames = names(migration_in_overseas_age_sco_wide),
                                     #                    choiceValues = c(seq(1:length(names(migration_in_overseas_age_sco_wide)))),
                                      #                   selected = positions_selected_ages_migration_in_overseas_age_sco
                                       #                )
                                        #             )
                                         #     ),
                                          #    column(width=9,
                                           #          fluidRow(
                                            #           p(tags$b(paste("Figure 3. Age distribution of migration to Scotland from Overseas ", "(", as.character(start_year_migration_in_overseas_age_sco), " - ", as.character(end_year_migration_in_overseas_age_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                             #          withSpinner(dygraphOutput("migration_in_overseas_age_sco_graph"), type = 5),
                                              #         align = "center"
                                               #      ),
                                                #     fluidRow(
                                                 #      textOutput("legendDivID_migration_in_overseas_age_sco"),
                                                  #     p("Source: "), 
                                                   #    a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                    #   collapsible = FALSE,
                                                     #  width = 12,
                                                      # style="margin-bottom: 100px;"
                                   #                  )
                                   #           )
                                   #  ),
                                   #  fluidRow(width = 12,
                                   #           column(width=3,
                                   #                  wellPanel(
                                   #                    checkboxGroupInput(
                                   #                      inputId = "migration_in_overseas_sex_sco_input",
                                   #                      label = "",
                                   #                      choiceNames = names(migration_in_overseas_sex_sco_wide),
                                   #                      choiceValues = c(seq(1:length(names(migration_in_overseas_sex_sco_wide)))),
                                   #                      selected = positions_selected_sexes_migration_in_overseas_sex_sco
                                    #                   )
                                     #                )
                                      #        ),
                                       #       column(width=9,
                                        #             fluidRow(
                                        #               p(tags$b(paste("Figure 4. Migration to Scotland from Overseas by Age ", "(", as.character(start_year_migration_in_overseas_sex_sco), " - ", as.character(end_year_migration_in_overseas_sex_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                        #               withSpinner(dygraphOutput("migration_in_overseas_sex_sco_graph"), type = 5),
                                        #               align = "center"
                                        #             ),
                                        #             fluidRow(
                                        #               textOutput("legendDivID_migration_in_overseas_sex_sco"),
                                        #               p("Source: "), 
                                        #               a("ODP", href = "https://statistics.gov.scot/data/search"),
                                        #               collapsible = FALSE,
                                        #               width = 12,
                                        #               style="margin-bottom: 100px;"
                                        #             )
                                        #      )
                                    )
                            )
                          )
                 ),
                 # REGIONAL ####
                 tabPanel("Regional",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Dependency ratio", 
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("depratio_reg_map_caption")),
                                                                sliderInput("depratio_reg_input", label = "", min = start_year_depratio_reg , max = end_year_depratio_reg, value = end_year_depratio_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("depratio_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Scottish Government", href = "http://statistics.gov.scot/data/population-estimates-dependency")
                                                         )
                                                )
                                       ),
                                       tabPanel("Migration",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("migration_reg_map_caption")),
                                                                sliderInput("migration_reg_input", label = "", min = start_year_migration_reg , max = end_year_migration_reg, value = end_year_migration_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("migration_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Open Data Platform", href = "https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fnet-migration&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2020&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fage=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fage%2Fall&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fsex=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fsex%2Fall")
                                                         )
                                                )
                                       ),
                                       tabPanel("Working-age population forecasts",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("working_age_reg_map_caption")),
                                                                sliderInput("working_age_reg_input", label = "", min = start_year_working_age_reg , max = end_year_working_age_reg, value = end_year_working_age_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("working_age_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Open Data Platform", href = "https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpopulation-projections-2018-based&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2043&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fage=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fage%2Fworking-age-16-64&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fsex=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fsex%2Fall")
                                                         )
                                                )
                                       )
                          )
                 )
               )
             ),
             # PARTICIPATION ########################################################################################################################
             tabPanel(
               value = "ParticipationTab",
               title = tags$div(icon("chart-pie", lib = "font-awesome"), "Participation"),
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 2,
                               tags$b("Scotland's rank among 37 OECD countries:")
                        ),
                        uiOutput("eactivity_bar"),
                        uiOutput("employment_bar"),
                        uiOutput("unemployment_bar"),
               #         uiOutput("genderpaygap_bar"),
               #          uiOutput("skills_bar"),
               #          uiOutput("neet_bar"),
               #          uiOutput("evoice_bar"),
                        style="background-color: grey; color: white"
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Economic activity"),
                               plotOutput("eactivity_overview_int_barplot", height = "200px"),
                               p("Scotland's economic activity rate was ", text_scotland_thisyear_eactivity_int,"% in 2018, compared to ", text_oecd_thisyear_eactivity_int,"% in the OECD. Over the past 5 years, the average economic activity rate was ", text_scotland_last5_eactivity_int ,"% in Scotland, compared to ", text_oecd_last5_eactivity_int, "% in the OECD overall.")
                        ),
                        column(width = 4,
                               h2("Employment"),
                               plotOutput("employment_overview_int_barplot", height = "200px"),
                               p("Scotland's employment rate was ", text_scotland_thisyear_employment_int,"% in 2018, compared to ", text_oecd_thisyear_employment_int,"% in the OECD. Over the past 5 years, the average employment rate was ", text_scotland_last5_employment_int ,"% in Scotland, compared to ", text_oecd_last5_employment_int, "% in the OECD overall.")
                        ),
                        column(width = 4,
                               h2("Unemployment"),
                               plotOutput("unemployment_overview_int_barplot", height = "200px"),
                               p("Scotland's unemployment rate was ", text_scotland_thisyear_unemployment_int,"% in 2018, compared to ", text_oecd_thisyear_unemployment_int,"% in the OECD. Over the past 5 years, the average unemployment rate was ", text_scotland_last5_unemployment_int ,"% in Scotland, compared to ", text_oecd_last5_unemployment_int, "% in the OECD overall.")
                        )
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Gender Pay Gap"),
                               plotOutput("genderpaygap_overview_int_barplot", height = "200px"),
                               p("Scotland's gender pay gap was 16% in 2017, compared to 13.5% in the OECD. Over the past 5 years, the average gender pay gap was 16.8% in Scotland, compared to 14% in the OECD overall.")
                        ),
                        column(width = 4,
                               h2("Skills"),
                               plotOutput("skillsunderprimary_overview_int_barplot", height = "200px"),
                               p("Scotland's attainment level of skills under primary was ", text_scotland_thisyear_skillsunderprimary_int,"% in 2018, compared to ", text_eu_thisyear_skillsunderprimary_int, "% in the EU.")
                        ),
                        column(width = 4,
                               h2("Youth Unemployment"),
                               plotOutput("yunemployment_overview_int_barplot", height = "200px"),
                               p("Scotland's youth unemployment rate was ", text_scotland_thisyear_yunemployment_int,"% in 2018, compared to ", text_eu_thisyear_yunemployment_int,"% in the EU.")
                        )
               ),
               # DETAILS ####
               h1("Details"),
               tabsetPanel(
                 # INTERNATIONAL ####
                 tabPanel("International",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Economic activity", 
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "eactivity_int_input",
                                                         label = "",
                                                         choiceNames = names(eactivity_int_wide),
                                                         choiceValues = c(seq(1:length(names(eactivity_int_wide)))),
                                                         selected = positions_selected_countries_eactivity_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Economic activity rate (aged 16-64 for Scotland, aged 15-64 for others) ", "(", as.character(start_year_eactivity_int), " - ", as.character(end_year_eactivity_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("eactivity_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_eactivity_int"),
                                                       p("Source: "), 
                                                       a("OECD", href = "https://data.oecd.org/"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Employment",
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "employment_int_input",
                                                         label = "",
                                                         choiceNames = names(employment_int_wide),
                                                         choiceValues = c(seq(1:length(names(employment_int_wide)))),
                                                         selected = positions_selected_countries_employment_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 2. Employment rate (aged 16-64 for Scotland, aged 15-64 for others) ", "(", as.character(start_year_employment_int), " - ", as.character(end_year_employment_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("employment_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_employment_int"),
                                                       p("Source: "), 
                                                       a("OECD", href = "https://data.oecd.org/"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Unemployment",
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "unemployment_int_input",
                                                         label = "",
                                                         choiceNames = names(unemployment_int_wide),
                                                         choiceValues = c(seq(1:length(names(unemployment_int_wide)))),
                                                         selected = positions_selected_countries_unemployment_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 3. Unemployment rate (aged 16-64 for Scotland, aged 15-64 for others) ", "(", as.character(start_year_unemployment_int), " - ", as.character(end_year_unemployment_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("unemployment_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_unemployment_int"),
                                                       p("Source: "), 
                                                       a("OECD", href = "https://data.oecd.org/"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Gender pay gap",
                                     fluidRow(width = 12,
                                              p(tags$b(paste("Figure 4. The difference between median earnings for men and women relative to median earnings for men (2014)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                              plotOutput("genderpaygap2_overview_int_barplot")
                                              # column(width=3, 
                                              #        wellPanel(
                                              #          checkboxGroupInput(
                                              #            inputId = "genderpaygap_int_input",
                                              #            label = "",
                                              #            choiceNames = names(genderpaygap_int_wide),
                                              #            choiceValues = c(seq(1:length(names(genderpaygap_int_wide)))),
                                              #            selected = positions_selected_countries_genderpaygap_int
                                              #          )
                                              #        )
                                              # ),      
                                              # column(width=9, 
                                              #        fluidRow(
                                              #          p(tags$b(paste("Figure 4. The difference between median earnings for men and women relative to median earnings for men ", "(", as.character(start_year_genderpaygap_int), " - ", as.character(end_year_genderpaygap_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                              #          withSpinner(dygraphOutput("genderpaygap_int_graph"), type = 5),
                                              #          align = "center"
                                              #        ),
                                              #        fluidRow(
                                              #          textOutput("legendDivID_genderpaygap_int"),
                                              #          p("Source: "), 
                                              #          a("OECD", href = "https://stats.oecd.org/Index.aspx?QueryId=54751#"),
                                              #          collapsible = FALSE,
                                              #          width = 12,
                                              #          style="margin-bottom: 100px;"
                                              #        )
                                              # )
                                     )
                            ),
                            tabPanel("Skills",
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "skillsunderprimary_int_input",
                                                         label = "",
                                                         choiceNames = names(skillsunderprimary_int_wide),
                                                         choiceValues = c(seq(1:length(names(skillsunderprimary_int_wide)))),
                                                         selected = positions_selected_countries_skillsunderprimary_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 5. Population aged 25-64 with ess than primary, primary and lower secondary education (levels 0-2) ", "(", as.character(start_year_skillsunderprimary_int), " - ", as.character(end_year_skillsunderprimary_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("skillsunderprimary_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_skillsunderprimary_int"),
                                                       p("Source: "), 
                                                       a("Eurostat", href = "https://ec.europa.eu/eurostat/home?"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     ),
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "skillstertiary_int_input",
                                                         label = "",
                                                         choiceNames = names(skillstertiary_int_wide),
                                                         choiceValues = c(seq(1:length(names(skillstertiary_int_wide)))),
                                                         selected = positions_selected_countries_skillstertiary_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 6. Population aged 25-64 by educational attainment level: Tertiary education (levels 5-8) ", "(", as.character(start_year_skillstertiary_int), " - ", as.character(end_year_skillstertiary_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("skillstertiary_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_skillstertiary_int"),
                                                       p("Source: "), 
                                                       a("Eurostat", href = "https://ec.europa.eu/eurostat/home?"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Youth unemployment",
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "yunemployment_int_input",
                                                         label = "",
                                                         choiceNames = names(yunemployment_int_wide),
                                                         choiceValues = c(seq(1:length(names(yunemployment_int_wide)))),
                                                         selected = positions_selected_countries_yunemployment_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 7. Youth unemployment rate ", "(", as.character(start_year_yunemployment_int), " - ", as.character(end_year_yunemployment_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("yunemployment_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_yunemployment_int"),
                                                       p("Source: "), 
                                                       a("Eurostat", href = "https://ec.europa.eu/eurostat/home?"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Employee voice",
                                     fluidRow(width = 12,
                                              p(tags$b(paste("Figure 8. Percentage of employees with the right to bargain (2017)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                              plotOutput("evoice2_overview_int_barplot")
                                              # column(width=3, 
                                              #        wellPanel(
                                              #          checkboxGroupInput(
                                              #            inputId = "evoice_int_input",
                                              #            label = "",
                                              #            choiceNames = names(evoice_int_wide),
                                              #            choiceValues = c(seq(1:length(names(evoice_int_wide)))),
                                              #            selected = positions_selected_countries_evoice_int
                                              #          )
                                              #        )
                                              # ),      
                                              # column(width=9, 
                                              #        fluidRow(
                                              #          p(tags$b(paste("Figure 8. Percentage of employees with the right to bargain ", "(", as.character(start_year_evoice_int), " - ", as.character(end_year_evoice_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                              #          withSpinner(dygraphOutput("evoice_int_graph"), type = 5),
                                              #          align = "center"
                                              #        ),
                                              #        fluidRow(
                                              #          textOutput("legendDivID_evoice_int"),
                                              #          p("Source: "), 
                                              #          a("OECD", href = "https://data.oecd.org/"),
                                              #          collapsible = FALSE,
                                              #          width = 12,
                                              #          style="margin-bottom: 100px;"
                                              #        )
                                              # )
                                     )
                            )
                          )
                 ),
                 # SCOTLAND ####
                 tabPanel("Scotland",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Economic participation", 
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Economic activity rate (%): Scotland: Aged 16-64: ", "(", as.character(start_year_eactivity_sco), " - ", as.character(end_year_eactivity_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("eactivity_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_eactivity_sco"),
                                                       p("Source: "), 
                                                       a("Office for National Statistics", href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/timeseries/lf3n/lms"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Living wage",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 2. Percentage of employees earning less than the living wage: ", "(", as.character(start_year_livingwage_sco), " - ", as.character(end_year_livingwage_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("livingwage_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_livingwage_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Gender pay gap",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 3. The difference between male and female full-time hourly earnings, expressed as a percentage of male full-time hourly earnings: ", "(", as.character(start_year_genderpaygap_sco), " - ", as.character(end_year_genderpaygap_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("genderpaygap_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_genderpaygap_sco"),
                                                       p("Source: "), 
                                                       a("Annual Survey of Hours and Earnings", href = "https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fearnings-paygap"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Employee voice",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 4. The percentage of employees who agree that they are affected by collective agreement: ", "(", as.character(start_year_productivity_sco), " - ", as.character(end_year_productivity_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("evoice_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_evoice_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Work place learning",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 5. Percent of employees who received on the job training in the last 3 months: ", "(", as.character(start_year_wplearning_sco), " - ", as.character(end_year_wplearning_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("wplearning_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_wplearning_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Government", href = "https://statistics.gov.scot/data/national-performance-framework"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Young people's participation",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 6. Percentage of young adults (16-19 year olds) participating in education, training or employment.: ", "(", as.character(start_year_youngpplpart_sco), " - ", as.character(end_year_youngpplpart_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("youngpplpart_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_youngpplpart_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Skill shortage vacancies",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 7. The proportion of employers in Scotland with at least one skills shortage vacancy: ", "(", as.character(start_year_skillshortage_sco), " - ", as.character(end_year_skillshortage_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("skillshortage_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_skillshortage_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Worklessness",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 8. Proportion of households in Scotland classed as workless: ", "(", as.character(start_year_worklessness_sco), " - ", as.character(end_year_worklessness_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("worklessness_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_worklessness_sco"),
                                                       p("Source: "), 
                                                       a("X", href = "x"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )
                            
                            
                          )
                 ),
                 # REGIONAL ####
                 tabPanel("Regional",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Economic Activity", 
                                               fluidRow(width = 12,
                                                        column(width=12,
                                                               tags$b(textOutput("eactivity_reg_map_caption")),
                                                               sliderInput("eactivity_reg_input", label = "", min = start_year_eactivity_reg , max = end_year_eactivity_reg, value = end_year_eactivity_reg, width = "50%", sep = "", step = 1),
                                                               withSpinner(leafletOutput("eactivity_reg_map"), type = 5),
                                                               p("Source: "), 
                                                               a("SCRIG", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/")
                                                        )
                                               )
                                       ),
                                       tabPanel("Living wage",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("lwage_reg_map_caption")),
                                                                sliderInput("lwage_reg_input", label = "", min = start_year_lwage_reg , max = end_year_lwage_reg, value = end_year_lwage_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("lwage_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Open Data Platform", href = "https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fliving-wage&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fpercent&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2020&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2Fgender=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fgender%2Fall")
                                                         )
                                                )
                                       ),
                                       tabPanel("Gender pay gap",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("gpaygap_reg_map_caption")),
                                                                sliderInput("gpaygap_reg_input", label = "", min = start_year_gpaygap_reg , max = end_year_gpaygap_reg, value = end_year_gpaygap_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("gpaygap_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Open Data Platform", href = "https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fearnings-paygap&http%3A%2F%2Fpurl.org%2Flinked-data%2Fsdmx%2F2009%2Fdimension%23refPeriod=http%3A%2F%2Freference.data.gov.uk%2Fid%2Fyear%2F2020&http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fdimension%2FworkingPattern=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fconcept%2Fworking-pattern%2Ffull-time")
                                                         )
                                                )
                                       ),
                                       tabPanel("Workplace learning",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("wplearning_reg_map_caption")),
                                                                sliderInput("wplearning_reg_input", label = "", min = start_year_wplearning_reg , max = end_year_wplearning_reg, value = end_year_wplearning_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("wplearning_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Scotland's Labour Market - People, Places and Regions: Annual Population Survey", href = "https://www.gov.scot/publications/scotlands-labour-market-people-places-and-regions-background-tables-and-charts/")
                                                         )
                                                )
                                       ),
                                       tabPanel("Worklessness",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("worklessness_reg_map_caption")),
                                                                sliderInput("worklessness_reg_input", label = "", min = start_year_worklessness_reg , max = end_year_worklessness_reg, value = end_year_worklessness_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("worklessness_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("NOMIS", href = "https://www.nomisweb.co.uk")
                                                         )
                                                )
                                       )
                          )
                                  
                 )
               )
             ),
             # PEOPLE ################################################################################################################################
             tabPanel(
               title = tags$div(icon("male", lib = "font-awesome"), "People"),
               value = "PeopleTab",
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 2,
                               tags$b("Scotland's rank among 37 OECD countries:")
                        ),
                        uiOutput("lifeexpall_bar"),
                        style="background-color: grey; color: white"
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Life expectancy"),
                               plotOutput("lifeexpall_overview_int_barplot", height = "200px"),
                               p("Scotland's life expectancy was ", text_scotland_thisyear_lifeexpall_int," in 2017, compared to ", text_oecd_thisyear_lifeexpall_int," in the OECD. Over the past 5 years, the average life expectancy was ", text_scotland_last5_lifeexpall_int ," in Scotland, compared to ", text_oecd_last5_lifeexpall_int, " in the OECD overall.")
                        ),
                        column(width = 4,
                        ),
                        column(width = 4,
                        )
               ),
               # DETAILS ####
               h1("Details"),
               tabsetPanel(
                 # INTERNATIONAL ####
                 tabPanel("International",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Life expectancy",
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "lifeexpall_int_input",
                                                         label = "",
                                                         choiceNames = names(lifeexpall_int_wide),
                                                         choiceValues = c(seq(1:length(names(lifeexpall_int_wide)))),
                                                         selected = positions_selected_countries_lifeexpall_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Life expectancy of the total population at birth ", "(", as.character(start_year_lifeexpall_int), " - ", as.character(end_year_lifeexpall_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("lifeexpall_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_lifeexpall_int"),
                                                       p("Source: "), 
                                                       a("OECD", href = "https://data.oecd.org/"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )
                          )
                 ),
                 # SCOTLAND ####
                 tabPanel("Scotland",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Poverty",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 3. Proportion of people who are in relative poverty (below 60% of UK median income after housing costs (three-year rolling average): ", "(", as.character(start_year_rphousingc_sco), " - ", as.character(end_year_rphousingc_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("rphousingc_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_rphousingc_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Child poverty",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 3. Proportion of children who are in relative poverty (below 60% of UK median income after housing costs (three-year rolling average): ", "(", as.character(start_year_cpoverty_sco), " - ", as.character(end_year_cpoverty_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("cpoverty_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_cpoverty_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Government analysis of the Family Resources Survey, Households Below Average Incomes dataset", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Income inequality",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 3. Palma ratio of income inequality (total household income of top 10% of population divided by that of bottom 40%) expressed as a percentage (three-year rolling average): ", "(", as.character(start_year_pratio_sco), " - ", as.character(end_year_pratio_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("pratio_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_pratio_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Government", href = "https://statistics.gov.scot/data/national-performance-framework"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            
                            
                            tabPanel("Healthy life expectancy - female",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Healthy life expectancy (female): ", "(", as.character(start_year_hlifeexp_female_sco), " - ", as.character(end_year_hlifeexp_female_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("hlifeexp_female_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_hlifeexp_female_sco"),
                                                       p("Source: "), 
                                                       a("National Performance Framework", href = "https://statistics.gov.scot/data/national-performance-framework"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            
                            tabPanel("Healthy life expectancy - male",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Healthy life expectancy (male): ", "(", as.character(start_year_hlifeexp_male_sco), " - ", as.character(end_year_hlifeexp_male_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("hlifeexp_male_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_hlifeexp_male_sco"),
                                                       p("Source: "), 
                                                       a("National Performance Framework", href = "https://statistics.gov.scot/data/national-performance-framework"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Mental wellbeing",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 5. Mental wellbeing - Average score on Warwick-Edinburgh Mental Wellbeing Scale: ", "(", as.character(start_year_mwellbeing_sco), " - ", as.character(end_year_mwellbeing_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("mwellbeing_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_mwellbeing_sco"),
                                                       p("Source: "), 
                                                       a("National Performance Framework", href = "https://statistics.gov.scot/data/national-performance-framework"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Social capital",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 6. Social Capital Index (2013 base year) - measure of social networks, community cohesion, social particpation, trust and empowerment: ", "(", as.character(start_year_scapital_sco), " - ", as.character(end_year_scapital_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("scapital_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_scapital_sco"),
                                                       p("Source: "), 
                                                       a("National Performance Framework", href = "https://statistics.gov.scot/data/national-performance-framework"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )     
                          )
                 ),
                 # REGIONAL ####
                 tabPanel("Regional",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Healthy life expectancy",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("hlifeexp_male_reg_map_caption")),
                                                                sliderInput("hlifeexp_male_reg_input", label = "", min = start_year_hlifeexp_male_reg , max = end_year_hlifeexp_male_reg, value = end_year_hlifeexp_male_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("hlifeexp_male_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("National Records of Scotland", href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/healthy-life-expectancy-in-scotland/2017-2019")
                                                         )
                                                ),
                                                fluidRow(width = 12, style="padding-top: 20px;"),
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("hlifeexp_female_reg_map_caption")),
                                                                sliderInput("hlifeexp_female_reg_input", label = "", min = start_year_hlifeexp_female_reg , max = end_year_hlifeexp_female_reg, value = end_year_hlifeexp_female_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("hlifeexp_female_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("National Records of Scotland", href = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/healthy-life-expectancy-in-scotland/2017-2019")
                                                         )
                                                )
                                       ),
                                       tabPanel("Child poverty",
          
                                                fluidRow(width = 12, style="padding-top: 20px;"),
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("cpoverty_reg_map_caption")),
                                                                sliderInput("cpoverty_reg_input", label = "", min = start_year_cpoverty_reg , max = end_year_cpoverty_reg, value = end_year_cpoverty_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("cpoverty_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Department for Work and Pensions", href = "https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-2014-to-2020")
                                                         )
                                                )
                                       ), 
                                       
                                       tabPanel("No qualifications",
                                                
                                                fluidRow(width = 12, style="padding-top: 20px;"),
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("noquals_reg_map_caption")),
                                                                sliderInput("noquals_reg_input", label = "", min = start_year_noquals_reg , max = end_year_noquals_reg, value = end_year_noquals_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("noquals_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("NOMIS", href = "https://www.nomisweb.co.uk")
                                                         )
                                                )
                                       )
                                       
                          )
                 )
               )
             ),    
             # PLACE ################################################################################################################################
             tabPanel(
               title = tags$div(icon("map-marked", lib = "font-awesome"), "Place"),
               value = "PlaceTab",
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 2,
                               tags$b("Scotland's rank among 28 EU countries:")
                        ),
                        uiOutput("broadband_bar"),
                        style="background-color: grey; color: white"
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Broadband"),
                               plotOutput("broadband_overview_int_barplot", height = "200px"),
                               p("Scotland's broadband access was ", text_scotland_thisyear_broadband_int," (% of households) in 2019, compared to ", text_eu_thisyear_broadband_int," (% of households) in the 28 EU countries.")
                        ),
                        column(width = 4,
                        ),
                        column(width = 4,
                        )
               ),
               # DETAILS ####
               h1("Details"),
               tabsetPanel(
                 # INTERNATIONAL ####
                 tabPanel("International",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Broadband", 
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "broadband_int_input",
                                                         label = "",
                                                         choiceNames = names(broadband_int_wide),
                                                         choiceValues = c(seq(1:length(names(broadband_int_wide)))),
                                                         selected = positions_selected_countries_broadband_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Share of households with broadband internet access (% of total households) ", "(", as.character(start_year_broadband_int), " - ", as.character(end_year_broadband_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("broadband_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_broadband_int"),
                                                       p("Source: "), 
                                                       a("OECD", href = "https://data.oecd.org/"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )
                          )
                 ),
                 # SCOTLAND ####
                 tabPanel("Scotland",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Access to green and blue space", 
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Percentage of adults living within 5 minutes walking distance of their nearest green or blue space: ", "(", as.character(start_year_greenandbluespace_sco), " - ", as.character(end_year_greenandbluespace_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("greenandbluespace_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_greenandbluespace_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Housing satisfaction", 
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Percentage of households reporting that they are either 'very satisfied' or 'fairly satisfied' with their house or flat: ", "(", as.character(start_year_housing_sco), " - ", as.character(end_year_housing_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("housing_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_housing_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Household Survey", href = "https://www.gov.scot/collections/scottish-household-survey"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Active travel", 
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "atravel_sco_input",
                                                         label = "",
                                                         choiceNames = names(atravel_sco_wide),
                                                         choiceValues = c(seq(1:length(names(atravel_sco_wide)))),
                                                         selected = positions_selected_countries_atravel_sco
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 2. Share of journeys made by walking or cycling ", "(", as.character(start_year_atravel_sco), " - ", as.character(end_year_atravel_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("atravel_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_atravel_sco"),
                                                       p("Source: "), 
                                                       a("Scottish Household Survey analysed and published by Transport Scotland", href = "https://www.transport.gov.scot/our-approach/statistics/#42764"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )
                          )
                 ),
                 # REGIONAL ####
                 tabPanel("Regional",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Access to green and blue space", 
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("bgreen_reg_map_caption")),
                                                                sliderInput("bgreen_reg_input", label = "", min = start_year_bgreen_reg , max = end_year_bgreen_reg, value = end_year_bgreen_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("bgreen_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Scottish Household Survey", href = "https://scotland.shinyapps.io/sg-scottish-household-survey-data-explorer/")
                                                         )
                                                )
                                       ),
                                       
                                       tabPanel("Housing satisfaction", 
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("housing_reg_map_caption")),
                                                                sliderInput("housing_reg_input", label = "", min = start_year_housing_reg , max = end_year_housing_reg, value = end_year_housing_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("housing_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Scottish Household Survey", href = "https://scotland.shinyapps.io/sg-scottish-household-survey-data-explorer/")
                                                         )
                                                )
                                       ),                      
                                       tabPanel("Quality of public services",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("pubservsat_reg_map_caption")),
                                                                sliderInput("pubservsat_reg_input", label = "", min = start_year_pubservsat_reg , max = end_year_pubservsat_reg, value = end_year_pubservsat_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("pubservsat_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Local Service Satisfaction - Scottish Surveys Core Questions", href = "https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Flocal-service-satisfaction-sscq")
                                                         )
                                                )
                                       ),
                                       tabPanel("Access to superfast broadband",
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("broadband_reg_map_caption")),
                                                                sliderInput("broadband_reg_input", label = "", min = start_year_broadband_reg , max = end_year_broadband_reg, value = end_year_broadband_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("broadband_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("OFCOM", href = "Fixed coverage local and unitary authority data' at https://www.ofcom.org.uk/research-and-data/multi-sector-research/infrastructure-research/connected-nations-2020/data-downloads ")
                                                         )
                                                )
                                       )
                          )
                 )
               )
             ),        
             # SUSTAINABILITY #######################################################################################################################
             tabPanel(
               title = tags$div(icon("tree", lib = "font-awesome"), "Sustainability"),
               value = "SustainabilityTab",
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 2,
                               tags$b("Scotland's rank among 28 EU countries:")
                        ),
                        uiOutput("ggemissions2_bar"),
                        style="background-color: grey; color: white"
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Greenhouse Gas Emissions"),
                               plotOutput("ggemissions2_overview_int_barplot", height = "200px"),
                               p("Scotland's greenhouse gas emissions were ", text_scotland_thisyear_ggemissions2_int," (Thousand tonnes of CO2 equivalent) in 2019, compared to ", text_eu_thisyear_ggemissions2_int," (Thousand tonnes of CO2 equivalent) total across all OECD European countries.")
                        ),
                        column(width = 4,
                        ),
                        column(width = 4,
                        )
               ),
               # DETAILS ####
               h1("Details"),
               tabsetPanel(
                 # INTERNATIONAL ####
                 tabPanel("International",
                          navlistPanel(widths=c(3,9),
                            tabPanel("Greenhouse Gas Emissions ", 
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "ggemissions_int_input",
                                                         label = "",
                                                         choiceNames = names(ggemissions_int_wide),
                                                         choiceValues = c(seq(1:length(names(ggemissions_int_wide)))),
                                                         selected = positions_selected_countries_ggemissions_int
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 1. Greenhouse gas emissions excl LULUCF (Tonnes of CO2 equivalent, Thousands) ", "(", as.character(start_year_ggemissions_int), " - ", as.character(end_year_ggemissions_int), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("ggemissions_int_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_ggemissions_int"),
                                                       p("Source: "), 
                                                       a("OECD", href = "https://data.oecd.org/"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            )
                          )
                 ),
                 # SCOTLAND ####
                 tabPanel("Scotland",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Greenhouse gas emissions",
                                                fluidRow(width = 12,
                                                         column(width=12, 
                                                                fluidRow(
                                                                  p(tags$b(paste("Figure 1. Scotland's greenhouse gas emissions excl LULUCF: ", "(", as.character(start_year_gasemissions_sco), " - ", as.character(end_year_gasemissions_sco), ") ", " (Mt Co2Equiv)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                                  withSpinner(dygraphOutput("gasemissions_sco_graph"), type = 5),
                                                                  align = "center"
                                                                ),
                                                                fluidRow(
                                                                  textOutput("legendDivID_gasemissions_sco"),
                                                                  p("Source: "), 
                                                                  a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                                  collapsible = FALSE,
                                                                  width = 12,
                                                                  style="margin-bottom: 100px;"
                                                                )
                                                         )
                                                )
                                       ),
                                       
                                       tabPanel("Carbon footprint", 
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 2. Greenhouse gas emissions associated with the consumption by Scottish residents on goods and services and by private heating and motoring: ", "(", as.character(start_year_cfootprint_sco), " - ", as.character(end_year_cfootprint_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("cfootprint_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_cfootprint_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            
                            tabPanel("Air pollutant emissions", 
                                     fluidRow(width = 12,
                                              column(width=3, 
                                                     wellPanel(
                                                       checkboxGroupInput(
                                                         inputId = "airpollutant_sco_input",
                                                         label = "",
                                                         choiceNames = names(airpollutant_sco_wide),
                                                         choiceValues = c(seq(1:length(names(airpollutant_sco_wide)))),
                                                         selected = positions_selected_indices_airpollutant_sco
                                                       )
                                                     )
                                              ),      
                                              column(width=9, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 3. Air pollutant emissions by type ", "(", as.character(start_year_airpollutant_sco), " - ", as.character(end_year_airpollutant_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("airpollutant_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_airpollutant_sco"),
                                                       p("Source: "), 
                                                       a("Air Pollutant Inventories for England, Scotland, Wales and Northern Ireland", href = "https://naei.beis.gov.uk/reports/reports?report_id=1010"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ), 
                            
                            
                            tabPanel("Material footprint", 
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 4. Total Domestic Material Consumption (tonnes per capita) ", "(", as.character(start_year_mfootprint_sco), " - ", as.character(end_year_mfootprint_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("mfootprint_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_mfootprint_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Natural Capital",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 5. Percent of natural features, on protected nature sites, in favourable condition: ", "(", as.character(start_year_naturalf_sco), " - ", as.character(end_year_naturalf_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("naturalf_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_naturalf_sco"),
                                                       p("Source: "), 
                                                       a("ODP", href = "https://statistics.gov.scot/data/search"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            tabPanel("Natural Capital Accounts",
                                     fluidRow(width = 12,
                                              column(width=12, 
                                                     fluidRow(
                                                       p(tags$b(paste("Figure 5. Scottish Natural Capital Accounts 2021: Annual Flows Monetary Value: ", "(", as.character(start_year_naturalcapaccs_sco), " - ", as.character(end_year_naturalcapaccs_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                       withSpinner(dygraphOutput("naturalcapaccs_sco_graph"), type = 5),
                                                       align = "center"
                                                     ),
                                                     fluidRow(
                                                       textOutput("legendDivID_naturalcapaccs_sco"),
                                                       p("Note: The monetary value of services can be affected by financial as well as physical factors."),
                                                       p("Source: "), 
                                                       a("Scottish Natural Capital Accounts 2021", href = "https://www.gov.scot/publications/scottish-natural-capital-accounts-2021"),
                                                       collapsible = FALSE,
                                                       width = 12,
                                                       style="margin-bottom: 100px;"
                                                     )
                                              )
                                     )
                            ),
                            
                            
                            
                            tabPanel("Marine and terrestrial species", 
                                    fluidRow(width = 12,
                                            column(width=3, 
                                                    wellPanel(
                                                                             checkboxGroupInput(
                                                                               inputId = "mandtspecies_sco_input",
                                                                               label = "",
                                                                               choiceNames = names(mandtspecies_sco_wide),
                                                                               choiceValues = c(seq(1:length(names(mandtspecies_sco_wide)))),
                                                                               selected = positions_selected_indices_mandtspecies_sco
                                                                             )
                                                                           )
                                                                    ),      
                                                                    column(width=9, 
                                                                           fluidRow(
                                                                             p(tags$b(paste("Figure 6. Marine and Terrestrial Species Indices ", "(", as.character(start_year_mandtspecies_sco), " - ", as.character(end_year_mandtspecies_sco), ") ", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                                             withSpinner(dygraphOutput("mandtspecies_sco_graph"), type = 5),
                                                                             align = "center"
                                                                           ),
                                                                           fluidRow(
                                                                             textOutput("legendDivID_mandtspecies_sco"),
                                                                             p("Source: "), 
                                                                             a("National Performance Framework", href = "https:statistics.gov.scot/data/national-performance-framework"),
                                                                             collapsible = FALSE,
                                                                             width = 12,
                                                                             style="margin-bottom: 100px;"
                                                                           )
                                                                    )
                                                           )
                                                  )
                                     )
                            ),                 
                            
                        
                 # REGIONAL ####
                 tabPanel("Regional",
                          navlistPanel(widths=c(3,9),
                                       tabPanel("Air quality", 
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("airqual_reg_map_caption")),
                                                                sliderInput("airqual_reg_input", label = "", min = as.Date(start_year_airqual_reg) , max = as.Date(end_year_airqual_reg), value = as.Date(end_year_airqual_reg), width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("airqual_reg_map"), type = 5),
                                                                p("Source: "), 
                                                                a("Air Quality in Scotland", href = "https://www.scottishairquality.scot")
                                                         )
                                                )
                                       ),
                                       tabPanel("CO2 emissions", 
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("CO2_reg_map_caption")),
                                                                sliderInput("CO2_reg_input", label = "", min = start_year_CO2_reg , max = end_year_CO2_reg, value = end_year_CO2_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("CO2_reg_map"), type = 5),

                                                                p("To note: These estimates of regional Scottish emissions differ from those typically shown in the Scottish greenhouse gas statistics publication which is used for monitoring progress against Scotland's statutory emissions reductions targets.  The principal reasons being: 1. These data relate to emissions of carbon dioxide only and exclude other greenhouse gases; and 2. they are presented on an end-user basis for main energy carriers."),
                                                                p("Source: "), 
                                                                a("UK local authority and regional carbon dioxide emissions national statistics: 2005 to 2019", href = "https://data.gov.uk/dataset/723c243d-2f1a-4d27-8b61-cdb93e5b10ff/uk-local-authority-and-regional-carbon-dioxide-emissions-national-statistics-2005-to-2019")
                                                         )
                                                )
                                       ),
                                       tabPanel("Household waste", 
                                                fluidRow(width = 12,
                                                         column(width=12,
                                                                tags$b(textOutput("hhwaste_reg_map_caption")),
                                                                sliderInput("hhwaste_reg_input", label = "", min = start_year_hhwaste_reg , max = end_year_hhwaste_reg, value = end_year_hhwaste_reg, width = "50%", sep = "", step = 1),
                                                                withSpinner(leafletOutput("hhwaste_reg_map"), type = 5),
                                                                
                                                                
                                                                p("Source: "), 
                                                                a("SEPA", href = "https://informatics.sepa.org.uk/HouseholdWaste/")
                                                         )
                                                )
                                       ),
                                       tabPanel("Habitat connectivity - data not yet available")
                                       )
               )
               )
             ),        
             # EQUALITIES DASHBOARD #################################################################################################################
             tabPanel(
               title = tags$div(icon("balance-scale", lib = "font-awesome"), "Equalities Dashboard"),
               value = "EqualitiesTab",
               # OVERVIEW ####
               fluidRow(width = 12,
                        column(width = 4,
                               h2('Gender'),
                               plotOutput("gpaygap_eq_overview_int_lineplot", height = "200px"),
                               p("Source: Annual Survey of Hours and Earnings"),
                               p("Scotland's gender pay gap was ", gender_last_figure, "% in ", gender_year, ". In the past 5 years, the average gender pay gap was ", gender_5years_figure, "% .")
                        ),
                        column(width = 4,
                               h2('Disability'),
                               plotOutput("dempgap_eq_overview_int_lineplot", height = "200px"),
                               p("Source: Annual Population Survey"),
                               p("Scotland's disabled employment pay gap was ", disability_last_figure, "% in ", disability_year, ". In the past 5 years, the average disabled employment pay gap was ", disability_5years_figure, "% .")
                        ),
                        column(width = 4,
                               h2('Age'),
                               plotOutput("youthunemp_eq_overview_int_lineplot", height = "200px"),
                               p("Source: Annual Population Survey"),
                               p("Scotland's youth unemployment rate was ", age_last_figure, "% in ", age_year, ". In the past 5 years, the average youth unemployment rate was ", age_5years_figure, "% .")
                        )
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2('Ethnicity'),
                               plotOutput("ethnicmgap_eq_overview_int_lineplot", height = "200px"),
                               p("Source: Annual Population Survey"),
                               p("Scotland's ethnic minority employment gap was ", ethnicity_last_figure, "% in ", ethnicity_year, ". In the past 5 years, the average ethnic minority employment gap was ", ethnicity_5years_figure, "% .")
                        ),
                        column(width = 4,
                               h2("Socio-economic status"),
                               plotOutput("socioecon_eq_overview_int_barplot", height = "200px"),
                               p("Source: Regional employment patterns in Scotland"),
                               p("In 2018, employment gap for the most deprived (Q1) was 62.5%, compared to 79.2% for the least deprived (Q5).")
                        ),
                        column(width = 4,
                               h2("Religion"),
                               plotOutput("religion_eq_overview_int_barplot", height = "200px"),
                               p("Source: Poverty and Income Inequality in Scotland 2017-20"),
                               p("On average, in the period 2015-20 52% of Muslim adults were in relative poverty (below 60% UK median income after housing costs).")
                        )
               ),
               fluidRow(width = 12,
                        column(width = 4,
                               h2("Sexual orientation"),
                               plotOutput("orientation_eq_overview_int_barplot", height = "200px"),
                               p("Source: Scottish Household Survey 2018"),
                               p("In 2019, 22% of Gay/Lesbian/Bisexual individuals experienced discrimination as compared with 7% for Straight individuals."),
                        ),
                        column(width = 4,
                        ),
                        column(width = 4,
                        )
               )
            )
    ),
  # FOOTER ##########################################################################################################################################
  fluidRow(
    br(),
    wellPanel(
      fluidRow(
        # FOOTER - ABOUT
        column(width = 3,
               icon("info", lib = "font-awesome"),
               strong("ABOUT"),
               p("The Economy Board requested that OCEA lead on developing a performance framework for the Economy Board, taking into account the framework developed for the Enterprise and Skills Strategic Board."),
        ),
        # FOOTER - COPYRIGHT NOTICE
        column(width = 3,
               icon("copyright", lib = "font-awesome"),
               strong("COPYRIGHT NOTICE"),
               p("You may use or re-use this information (not including logos) free of charge in any format or medium, under the terms of the ",
                 a("Open Government Licence", href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"), ".")
        ),
        # FOOTER - CONTACT DETAILS
        column(width = 3,
               icon("at", lib = "font-awesome"),
               strong("CONTACT DETAILS"),
               p("We welcome your feedback:"),
               p("SCRIG@gov.scot", style = "line-height: 0%;")
        ),
        # FOOTER - EXTERNAL LINKS
        column(width = 3,
               icon("external-link-alt", lib = "font-awesome"),
               strong("EXTERNAL LINKS"),
               p(a("OECD data", href = "https://data.oecd.org/")),
               p(a("Open Data Platfrom", href = "https://statistics.gov.scot/data/search")),
               p(a("National Performance Framework", href = "https://nationalperformance.gov.scot/")),
               p(a("SCRIG ", href = "https://www.inclusivegrowth.scot/")),
               p(a("SCRIG dashboard ", href = "https://scotland.shinyapps.io/sg-scrig-dashboard/"))
        )
      ),
      fluidRow(
        p("Reload the page should you experience any issues."),
        style = "text-align: center; outline: 0px;"
      )
    )
  ) # Navbar page ends here
)
) # UI ends here

