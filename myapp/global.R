library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleCharts)
library(dplyr)
library(plyr)
library(googleVis)
library(gdata)
require("RPostgreSQL")
library("RColorBrewer") 
library("scales")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                              Initializing the Google charts library           
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

tags$link(href=paste0("http://fonts.googleapis.com/css?",
                      "family=Source+Sans+Pro:300,600,300italic"),
          rel="stylesheet", type="text/css")

tags$style(type="text/css","body {font-family: 'Source Sans Pro'}")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                              Sets up the connection to the psql database         
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

drv <- dbDriver("PostgreSQL")
pw <- {"new_user_password"}
connect <<- dbConnect(drv, dbname = "vis_dash3",
                      host = "localhost", port = 5432,
                      user = "bhc15", password = pw)
rm(pw) #removing password

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                              Generating data frame for extra metrics    
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

dbGetQuery(connect, "create table extra_metrics as select A.size, A.content_type, A.project_id, date_trunc('DAY', A.created_at) as date, B.is_deleted from uploads A inner join file_versions B on A.id = B.upload_id")  
extra_metrics_DF <<- dbGetQuery(connect, "select * from extra_metrics")
dbGetQuery(connect, "drop table extra_metrics")

non_deleted_uploads <<-   sum( extra_metrics_DF[ which(extra_metrics_DF$is_deleted==FALSE),] $size)
deleted_uploads <<-   sum( extra_metrics_DF[ which(extra_metrics_DF$is_deleted==TRUE),] $size)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                              Loading up the projects in descending order of number of uploads           
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

dbGetQuery(connect, "create table temp_U_FV as select A.name, A.created_at, A.size, A.creator_id, A.content_type, A.project_ID from uploads A inner join file_versions B on A.id = B.upload_id")
projects_table <<- dbGetQuery(connect, "select count(*) Number_uploads, A.project_id, B.name from temp_U_FV A inner join projects B on A.project_id= B.id  group by A.project_ID, B.name order by Number_Uploads")
projects_table$name<- make.unique(projects_table$name) #making it so that different projects with the same name have unique names
dbGetQuery(connect, "drop table temp1")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#                              Queries the relevent info about company's total uploads per project     
#-----------------------------------------------------------------------------------------------------------------------------------------------------------

upload_query <- paste(sep="", "select  B.name, count(A.created_at) as number_uploads, sum(A.size) as total_storage, B.created_at as date, B.is_deleted from uploads A inner join projects B on A.project_id = B.id inner join file_versions C on A.id = C.upload_id group by B.id order by number_uploads desc, total_storage desc") #get's the number of uploads per project
company_upload_vs_project_df <<- dbGetQuery(connect, upload_query)
company_upload_vs_project_df<<-transform(company_upload_vs_project_df, deleted= ifelse(is_deleted==FALSE, "active", "deleted"))

