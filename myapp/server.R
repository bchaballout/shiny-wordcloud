shinyServer(function(input, output,session) {
  output$project_name <- renderText({ paste0("Dashboard for Project: '", input$project_select, "'")}) #generates text for the title of the dashboard
  
  #########################################################################################################################################################################################################################################################################
  #                                                                                             DDS DashBoard
  #########################################################################################################################################################################################################################################################################

  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Queries the relevent info about company's total uploads per project     
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------

   company_uploads_vs_project <- reactive({
    df <- company_upload_vs_project_df %>%
      select(name, date, number_uploads, deleted, number_uploads) #This prints the data table
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Sends the data in the formatted form to the Google Chart about company's total uploads per project      
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$company_uploads_vs_time <- reactive({
    input$project_select
    list(
      data = googleDataTable(company_uploads_vs_project()),
      options = list(
        title = sprintf(
          "Project Uploads vs Date")))
  }) 

  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Queries the relevent info about company's storage uploads per project     
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  company_storage_vs_project <- reactive({
    df <- company_upload_vs_project_df %>%
      select(name, date, total_storage, deleted, total_storage) #This prints the data table
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Sends the data in the formatted form to the Google Chart about company's total storage per project      
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$company_storage_vs_time <- reactive({
    input$project_select
    list(
      data = googleDataTable(company_storage_vs_project()),
      options = list(
        title = sprintf(
          "Project Storage vs Date"))) #humanReadable makes it so that the total storage isn't just displayed in bytes
  }) 
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              DDS project average project's uploads 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$bar_chart_dds_uploads <- renderGvis({
    
    average_active_uploads <-  floor( length(extra_metrics_DF$size[ which(extra_metrics_DF$is_deleted==FALSE)]) / length(unique(extra_metrics_DF$project_id)) )
    average_deleted_uploads <-  floor( length(extra_metrics_DF$size[ which(extra_metrics_DF$is_deleted==TRUE)]) / length(unique(extra_metrics_DF$project_id)) )
    
    upload_bar_df <- data.frame(pro = c("Average"), active_uploads = c(average_active_uploads), deleted_uploads = c(average_deleted_uploads))
    gvisColumnChart(upload_bar_df, xvar="pro", yvar = c("active_uploads", "deleted_uploads"),
                    options=list(
                      titleTextStyle = "{fontSize: 19}",
                      legend= "{position: 'bottom', textStyle: {fontSize: 14}}",
                      fontSize = 14,
                      width="50%", height = "575px",
                      title='Average Uploads'),
                    chartid= "bar_chart_dds_uploads")
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              DDS project average project's storage 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$bar_chart_dds_storage <- renderGvis({
    
    average_active_upload_usage <-   sum(extra_metrics_DF$size[ which(extra_metrics_DF$is_deleted==FALSE)]) / length(unique(extra_metrics_DF$project_id))
    total_deleted_uploads<-   sum( extra_metrics_DF[ which(extra_metrics_DF$is_deleted==TRUE),] $size)
    average_deleted_uploads <- total_deleted_uploads / length(unique(extra_metrics_DF$project_id))
    
    upload_bar_df <- data.frame(pro = c("Average"), active_usage = c(average_active_upload_usage), deleted_usage = c(average_deleted_uploads))
    gvisColumnChart(upload_bar_df, xvar="pro", yvar = c("active_usage", "deleted_usage"),
                    options=list(
                      titleTextStyle = "{fontSize: 19}",
                      legend= "{position: 'bottom', textStyle: {fontSize: 14}}",
                      fontSize = 14,
                      width="50%", height = "575px",
                      title='Average Storage'),
                    chartid= "bar_chart_dds_storage")
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              DDS average project activity 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$bar_chart_dds_activity <- renderGvis({
    project_days_df <- ddply(extra_metrics_DF,~project_id,summarise,number_of_distinct_orders=length(unique(date))) #Gives each project and how many days it was worked on
    average_days_activity <-   sum(project_days_df$number_of_distinct_orders) / length(unique(extra_metrics_DF$project_id)) #Gets the average amount of days each project is working on
    activity_bar_df <- data.frame(pro = c("Average"), average_age = c(average_days_activity))
    gvisColumnChart(activity_bar_df, xvar="pro", yvar = c("average_age"),
                    options=list(
                      titleTextStyle = "{fontSize: 19}",
                      legend= "{position: 'bottom', textStyle: {fontSize: 14}}",
                      fontSize = 14,
                      width="50%", height = "575px",
                      title='Average Age'),
                    chartid= "bar_chart_dds_activity")
  })
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Prints the relevent info about company's total uploads per project
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$companyTable <- renderDataTable({
    companyTable <- company_upload_vs_project_df[,c(1,2,3,4, 5)]
  })
  
  #########################################################################################################################################################################################################################################################################
  #                                                                                             Project DashBoard
  #########################################################################################################################################################################################################################################################################
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Queries the relevent info about projects' upload totals per day     
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  projectData_time_vs_total_uploads <- reactive({
    selected_project <- projects_table$project_id[which(projects_table$name == input$project_select)] #selected_project = selected project id
    check<<- projects_table$project_id[which(projects_table$name == input$project_select)] #this is to not print any charts if an invalid project was selected
    #Can have if statement here if user selects to display the deleted uploads to either do the following query or the same one minus the part that says where is_deleted = f
    if (length(check) >0) {
      upload_query <- paste(sep="", "select count(A.name) as total, date_trunc('day', A.created_at) as day, A.project_id from uploads A inner join file_versions B on A.id = B.upload_id where project_id = '",selected_project,"' group by day, A.project_id order by A.project_id") #ordered by content_type to perserve color order between bubble and pie chart
      upload_total_vs_day_DF <- dbGetQuery(connect, upload_query) #save it as a global data frame so that it can be acessed in the table function 
    }
    else upload_total_vs_day_DF <<- data.frame(project_id = c(""), day= c(0), total= c(0))
    df <- upload_total_vs_day_DF %>% select(project_id, day, total, total, total) #This prints the data table
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Sends the data in the formatted form to the Google Chart about projects' upload sizes      
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$chart_time_vs_total_uploads <- reactive({
    input$project_select
    list(
      data = googleDataTable(projectData_time_vs_total_uploads()),
      options = list(
        title = sprintf("Time vs Number Uploads"))) #humanReadable makes it so that the total storage isn't just displayed in bytes
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Queries the relevent info about projects' upload sizes     
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  projectData <- reactive({
    selected_project <- projects_table$project_id[which(projects_table$name == input$project_select)] #selected_project = selected project id
    #Can have if statement here if user selects to display the deleted uploads to either do the following query or the same one minus the part that says where is_deleted = f
    if (length(check) >0) {
      upload_query <- paste(sep="", "select A.name, A.created_at, A.size, A.creator_id, A.content_type, B.is_deleted from uploads A inner join file_versions B on A.id = B.upload_id where A.project_id = '",selected_project,"' order by A.content_type") #ordered by content_type to perserve color order between bubble and pie chart
      uploadsTableDF <<- dbGetQuery(connect, upload_query) #save it as a global data frame so that it can be acessed in the table function 
      uploadsTableDF<-transform(uploadsTableDF, is_deleted_numeric= ifelse(is_deleted==FALSE, 1, 0)) #converting the is_deleted boolean values to numbers so that the chart can change the size if the upload is or isn't deleted
    }
    else uploadsTableDF <<- data.frame(name = c(""), created_at = c(0), size= c(0), content_type = c(""), is_deleted_numeric = c(1)) #for when a person types and there is nothing selected yet
      df <- uploadsTableDF %>% select(name, created_at, size, content_type, is_deleted_numeric) #This prints the data table
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Sends the data in the formatted form to the Google Chart about projects' upload sizes      
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$chart <- reactive({
    input$project_select
    list(
      data = googleDataTable(projectData()),
      options = list(
        title = "Time vs Upload Size")) #humanReadable makes it so that the total storage isn't just displayed in bytes
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Sends the data in the formatted form to the googleVis Pie Chart    
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$pie <- renderGvis({
    selected_project <- projects_table$project_id[which(projects_table$name == input$project_select)] #This call is just to refresh the data
    if (length(check) >0) {
      dbGetQuery(connect, "create table temp_upload as select A.size, A.content_type, A.project_id from uploads A inner join file_versions B on A.id = B.upload_id")
      upload_query_new <- paste(sep="", "select sum(size) as size, coalesce(nullif(content_type,'' ), 'n/a') as content_type from temp_upload where project_id = '",selected_project,"'  group by content_type")
      uploadsTableDF_new<<-dbGetQuery(connect, upload_query_new)
      dbGetQuery(connect, "drop table temp_upload")
    }
    else uploadsTableDF_new <<- data.frame(content_type = c("N/A"), size= c(0)) #for when a person types and there is nothing selected yet
    data_pie<- data.frame(type= uploadsTableDF_new$content_type, size = c(uploadsTableDF_new$size)) #creates the pie chart's data frame
    gvisPieChart(data_pie,
                 options=list(
                   fontSize = 19,
                   width="100%", height = "400px",
                   legend= "{textStyle: {fontSize: 14}}",
                   title='Breakdown of Content-Type',
                   is3D = TRUE,
                   slices="{0: {offset: 0.2}, 1: {offset: 0.2}, 2: {offset: 0.2}}",
                   pieSliceText='label'),
                 chartid= "pie")
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Compares the selected project's uploads to the average of every other project's uploads 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$bar_chart_uploads <- renderGvis({
    selected_project <- projects_table$project_id[which(projects_table$name == input$project_select)] #This call is just to refresh the data
    average_upload_usage <-   sum(extra_metrics_DF$size) / length(unique(extra_metrics_DF$project_id))
    total_deleted_uploads<-   sum( extra_metrics_DF[ which(extra_metrics_DF$is_deleted==TRUE),] $size)
    average_deleted_uploads <- total_deleted_uploads / length(unique(extra_metrics_DF$project_id))
    if (length(check) >0) {
      sum_size_specific_project <- sum( extra_metrics_DF[ which(extra_metrics_DF$project_id==selected_project),] $size)
      sum_deletes_specific_project <- sum( extra_metrics_DF[ which(extra_metrics_DF$project_id==selected_project & extra_metrics_DF$is_deleted==TRUE),] $size)
      upload_bar_df <- data.frame(pro = c(paste( "Selected project: ",input$project_select), "Average"), total_size = c(sum_size_specific_project, average_upload_usage), deleted_files = c( sum_deletes_specific_project,average_deleted_uploads))
    }
    else upload_bar_df <- data.frame(pro = c(paste( "N/A"), "Average"), total_size = c(0, average_upload_usage), deleted_files = c( 0,average_deleted_uploads))
    gvisColumnChart(upload_bar_df, xvar="pro", yvar = c("total_size", "deleted_files"),
                    options=list(
                      titleTextStyle = "{fontSize: 19}",
                      legend= "{textStyle: {fontSize: 14}}",
                      fontSize = 19,
                      width="50%", height = "575px",
                      title='Selected Project Uploads vs Average Project Uploads'),
                    chartid= "bar_chart_uploads")
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Compares the selected project's activity to the average of every other project's activity 
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$bar_chart_activity <- renderGvis({
    selected_project <- projects_table$project_id[which(projects_table$name == input$project_select)] #This call is just to refresh the data
    project_days_df <- ddply(extra_metrics_DF,~project_id,summarise,number_of_distinct_orders=length(unique(date))) #Gives each project and how many days it was worked on
    average_days_activity <-   sum(project_days_df$number_of_distinct_orders) / length(unique(extra_metrics_DF$project_id)) #Gets the average amount of days each project is working on
    if (length(check) >0) {
      specific_project_days <- project_days_df[ which(project_days_df$project_id==selected_project),] $number_of_distinct_orders
      activity_bar_df <- data.frame(pro = c(paste( "Selected project: ",input$project_select), "Average"), average_days_upload_activity = c(specific_project_days, average_days_activity))
    }
    else activity_bar_df <- data.frame(pro = c(paste( "N/A"), "Average"), average_days_upload_activity = c(0, average_days_activity))
    gvisColumnChart(activity_bar_df, xvar="pro", yvar = c("average_days_upload_activity"),
                    options=list(
                      titleTextStyle = "{fontSize: 19}",
                      legend= "{textStyle: {fontSize: 14}}",
                      fontSize = 19,
                      width="50%", height = "575px",
                      title='Selected Project Activity vs Average Project Activity',
                      is3D = TRUE),
                    chartid= "bar_chart_activity")
  })

  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Prints the relevent info about projects in a table  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$uploadsTable <- renderDataTable({
    selected_project <- projects_table$project_id[which(projects_table$name == input$project_select)] #This call is just to refresh the data
    uploadsTable <- uploadsTableDF
  })
  
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #                              Prints the dynamic value boxes for the specific project
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$UploadBox <- renderValueBox({
    input$project_select #to update you
    if (length(check) >0) valueBox(prettyNum(length(uploadsTableDF$size),big.mark=","),"Total Uploads",  icon = icon("list"), color = "yellow") #for when a person types and there is nothing selected yet
    else valueBox(0,"Total Uploads",  icon = icon("list"), color = "yellow")
  })
  
  output$ActiveFileBox <- renderValueBox({
    input$project_select #to update you
    valueBox(humanReadable(c(sum( uploadsTableDF[ which(uploadsTableDF$is_deleted==FALSE),] $size)), units = "GiB", width = 2),"Total Storage from Active Files",  icon = icon("list"), color = "blue")
  })
  
  output$DeletedFileBox <- renderValueBox({  
    input$project_select #to update you
    valueBox(humanReadable(c(sum( uploadsTableDF[ which(uploadsTableDF$is_deleted==TRUE),] $size)), units = "GiB", width = 2), "Total Storage from Deleted Files",  icon = icon("list"), color = "red")
  })
})
