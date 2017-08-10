#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                              Formats the entire page        
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

dashboardPage(skin = 'green',
              
              dashboardHeader(
                title = HTML(paste(img(src= 'DSOM_logo.png', height = 25, width=25),'Duke Data Service')),
                titleWidth = 240),
              
              dashboardSidebar(width = 250,
                               sidebarMenu(
                                 menuItem(h3("Duke DS Overview"), tabName = "c_d"),
                                 menuItem(h3("Project Level"), tabName = "p_d"))),
              
              dashboardBody(
                tabItems(
                  
                  ######################################################################################################################################################################################################################################################################################################################
                  #                                                               Company Dashboard Tab
                  ######################################################################################################################################################################################################################################################################################################################
                  
                  tabItem(tabName = "c_d",
                          googleChartsInit(),
                          
                          fluidPage(theme = shinytheme("flatly"),
                                    h1(tags$b("Duke DS Dashboard"), align = "center",  style = "color:blue"),
                                    
                                    fluidRow(
                                      valueBox(prettyNum(sum(company_upload_vs_project_df$number_uploads), big.mark=","), "Total Uploads",  icon = icon("list"), color = "yellow"),
                                      valueBox(humanReadable(non_deleted_uploads, units = "GiB", width = 2), "Total Storage from Active Files",  icon = icon("list"), color = "blue"),
                                      valueBox(humanReadable(deleted_uploads, units = "GiB", width = 2), "Total Storage from Deleted Files",  icon = icon("list"), color = "red")),
                                    
                                    fluidRow(
                                      splitLayout(cellWidths = c("40%", "40%", "20%"),  align = "center", htmlOutput("bar_chart_dds_uploads"),htmlOutput("bar_chart_dds_storage"), htmlOutput("bar_chart_dds_activity"))),
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    #                              Formats the company's projects vs uploads chart and calls out to display it
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    fluidRow(
                                      googleBubbleChart("company_uploads_vs_time", width="100%", height = "675px",
                                                        options = list(
                                                          fontSize = 16,
                                                          sizeAxis = list(minValue= 0,  maxSize= 20),
                                                          hAxis = list( title = "Date"),
                                                          vAxis = list( title = "Number of Uploads"),
                                                          chartArea = list( top = 50, left = 75, height = "85%", width = "80%"),
                                                          colors = c('red', 'blue'),
                                                          bubble = list( opacity = 0.4, stroke = "none", textStyle = list( color = "none")),
                                                          explorer = list(),
                                                          titleTextStyle = list( fontSize = 20),
                                                          
                                                          tooltip = list(textStyle = list(fontSize = 12))))),
                                    
                                    fluidRow(
                                      googleBubbleChart("company_storage_vs_time", width="100%", height = "675px",
                                                        options = list(
                                                          fontSize = 16,
                                                          sizeAxis = list(minValue= 0,  maxSize= 20),
                                                          hAxis = list( title = "Date"),
                                                          vAxis = list( title = "Total Storage (in Bytes)"),
                                                          chartArea = list( top = 50, left = 75, height = "85%", width = "80%"),
                                                          colors = c('red', 'blue'),
                                                          bubble = list( opacity = 0.4, stroke = "none", textStyle = list( color = "none")),
                                                          explorer = list(),
                                                          titleTextStyle = list( fontSize = 20),
                                                          tooltip = list( textStyle = list( fontSize = 12))))),
                                    
                                    fluidRow(
                                      dataTableOutput("companyTable")))),
                  
                  ######################################################################################################################################################################################################################################################################################################################
                  #                                                               Projects Dashboard Tab
                  ######################################################################################################################################################################################################################################################################################################################
                  
                  tabItem(tabName = "p_d",
                          fluidPage(theme = shinytheme("flatly"),
                                    h1(textOutput("project_name" , container = tags$b), align = "center",  style = "color:blue"),
                                    selectizeInput("project_select", label = "Choose a project:", choices= projects_table$name, multiple = FALSE,width="100%"),
                                    googleChartsInit(),
                                    
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    #                              Formats the time vs number uploads chart and calls out to display it
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    
                                    fluidRow(
                                      #**********************************make it so that active files and deleted files are different
                                      valueBoxOutput("UploadBox"),
                                      valueBoxOutput("ActiveFileBox"),
                                      valueBoxOutput("DeletedFileBox")),
                                    
                                    googleBubbleChart("chart_time_vs_total_uploads", width="100%", height = "675px", 
                                                      options = list(
                                                        fontSize = 16,
                                                        hAxis = list( title = "Date"),
                                                        vAxis = list( title = "Number of Uploads" ),
                                                        chartArea = list( top = 50, left = 75, height = "85%", width = "90%" ),
                                                        bubble = list( opacity = 0.4, stroke = "none", textStyle = list( color = "none" )),
                                                        titleTextStyle = list( fontSize = 20 ),
                                                        tooltip = list( textStyle = list( fontSize = 12 )))),
                                    
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    #                              Formats the time vs upload size chart and calls out to display it
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    
                                    googleBubbleChart("chart", width="100%", height = "675px",
                                                      options = list(
                                                        fontName = "Source Sans Pro",
                                                        fontSize = 16,
                                                        sizeAxis = list(minValue= -.25,  maxSize= 20),
                                                        hAxis = list( title = "Date"),
                                                        vAxis = list(title = "Size of Upload (bytes)"),
                                                        chartArea = list( top = 50, left = 75, height = "85%", width = "80%"),
                                                        vAxis.titleTextStyle = list( bold=TRUE),
                                                        bubble = list(opacity = 0.4, stroke = "none", textStyle = list(color = "none")),
                                                        titleTextStyle = list(fontSize = 20),
                                                        tooltip = list(textStyle = list(fontSize = 12)))),
                                    
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    #                              Calls out to display the pie chart
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    
                                    htmlOutput("pie"),
                                    
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    #                              Calls out to display the uploads bar graph and the activity bar graph
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    
                                    fluidPage( #extra fluid page to blend the white background
                                      fluidRow(
                                        splitLayout(cellWidths = c("50%", "50%"), htmlOutput("bar_chart_uploads"), htmlOutput("bar_chart_activity")))),
                                    
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    #                              The call that prints the data table          
                                    #-----------------------------------------------------------------------------------------------------------------------------------------------------------
                                    
                                    dataTableOutput("uploadsTable")
                          ))))
              
)