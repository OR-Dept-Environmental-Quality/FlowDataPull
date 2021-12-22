#create shiny app to pull USGS data and automatically calculate the 7Q10, 30Q5, and harmonic mean flow
#over a selectd time period

#this app uses the dflow function created by Ryan Michie

#to run, click the "Run App" button above

print("Initial data queries may take a few minutes")

library(shiny)
library(RCurl)
library(lubridate)
library(zoo)
library(dplyr)
library(dataRetrieval)
library(dflowR)
library(psych)
library(shinybusy)
library(openxlsx)
library(DT)

#harmonic mean function I made myself - note that this will only accept a df with one column
#the column needs to be flow values
dharmonic<-function(x) {
  
  return(harmonic.mean(x,zero=FALSE)*(length(x)-length(which(x==0)))/length(x))
  
}

# no valid values to query, user will need to know which USGS station they need in advance at the moment

#############################Define UI##########################
ui<-fluidPage(
  #sidebar with parameter inputs
  
  sidebarLayout(
    sidebarPanel(
      
      #add warning
      
      tags$em("Warning: Current code only handles one USGS station at a time. If you want data for more than one station, please complete query and download the data separately"),
      
      #USGS station
      textInput("Station",
      label=" USGS Station"),
    
      tags$em("Note: Flow calculations will be run on entire dataset selected"),
    
       # Start Date (make start date ten years ago as default)
      dateInput("startd",
              label = "Select Start Date",
              min = '1949-09-15',
              value = Sys.Date()-3650),
       # End date
      dateInput("endd",
              label = "Select End Date",
              min = '1900-1-1'),
      
      #add action button, so query doesn't run until button is clicked
      actionButton("goButton","Run Query"),
      
      #add a download button so we can download query results
      downloadButton('downloadData', 'Download Data')
    ),
  
  #Setup main panel
  mainPanel(h1("USGS Flow Data Pull and Calculations"),
            h3("All data is in cfs and all flow values are mean daily stream flow"),
            
            #add line
            tags$hr(),
            #add break
            tags$br(),
            
            #multiple tabs: station info, original data, total calcs, and monthly calcs
            tabsetPanel(
              
              #station info
              tabPanel("USGS Station Information",
                       DT::dataTableOutput("info")),
              
              #original data
              tabPanel("USGS flow data",
                       DT::dataTableOutput("rawtable")),
              
              #total 7Q10, 30Q5, and harmonic mean flow for time range specified
              tabPanel("Flow Calculations for all data",
                       textOutput("sevenQten"),
                       textOutput("thirtyQfive"),
                       textOutput("harmonic")),
              
              #monthly calcs for 7Q10 and 30Q5
              tabPanel("Monthly 7Q10 and 30Q5",
                       DT::dataTableOutput("monthlys"))
            )
  )
),

#add icon to show when program is running query or download
add_busy_spinner(spin="fading-circle")
)
            
              
###################  SERVER ###############################

server<- function(input, output, session) {
  
  #isolate data so that you have to click a button to run query using eventReactive
  
  #get station information
  info<-eventReactive(input$goButton, {
    info<-readNWISsite(input$Station)
    
    sub<-select(info, agency_cd,site_no,station_nm,dec_lat_va,dec_long_va,huc_cd)
    
    colnames(sub)<-c("Agency","Station","Name","Latitude","Longitude","HUC")
    
    sub
  })
  
  #table for shiny view
  output$info<-renderDataTable({
    info()
  })
  
  #get USGS data
  data<-eventReactive(input$goButton, {
    
    #query data - returns mean daily flow in cfs
    q.df <- readNWISdv(siteNumbers = toString(sprintf("%s",input$Station)),
                       parameterCd = "00060",
                       startDate =toString(sprintf("%s",input$startd)),
                       endDate = toString(sprintf("%s",input$endd)),
                       statCd = "00003")
   
    colnames(q.df)<-c("Agency","Station","Date","mean daily stream flow (cfs)","Data Quality")
    
    q.df
  })
 
 #table of queried raw data for shiny app view
 output$rawtable<-renderDataTable({
   data()
 })
 
 #calculate the total flow stats for 7Q10, 30Q5, and harmonic mean flow
 sevenQten<-eventReactive(input$goButton, {
   q.df<-data()
   q <- q.df[,c(3,4)]
   colnames(q) <-c("date", "flow")
   q$date <- as.POSIXct(q$date, format="%Y-%m-%d")
   
   seven<-dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")
   
   seven
 })
   
   
 thirtyQfive<-eventReactive(input$goButton, {
   q.df<-data()
   q <- q.df[,c(3,4)]
   colnames(q) <-c("date", "flow")
   q$date <- as.POSIXct(q$date, format="%Y-%m-%d")
   
   thirty<-dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")
   
   thirty
 })

 
 harmonic<-eventReactive(input$goButton, {
   q.df<-data()
   q <- q.df[,4]
   
   harmonic<-dharmonic(q)
   
   harmonic
 })       
 
 #data for shiny app view
 output$sevenQten<-renderText({
   paste0("7Q10: ",round(sevenQten(),digits=2))
 })
 output$thirtyQfive<-renderText({
   paste0("30Q5: ",round(thirtyQfive(),digits=2))
 })
 output$harmonic<-renderText({
   paste0("Harmonic Mean: ",round(harmonic(),digits=2))
 })
              
#calculate monthly 7Q10s and 30Q5s - merge into one dataframe
 #note that monthly flows for february will exclude a leap year day
 
 monthly<-eventReactive(input$goButton, {
   q<-data()
   q <- q[,c(3,4)]
   colnames(q) <-c("date", "flow")
   q$date <- as.POSIXct(q$date, format="%Y-%m-%d")
   
   #calculate monthly dflow for 7Q10 and 30Q5
   jan7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="01-01", wyend="01-31"),digits=2)
   feb7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="02-01", wyend="02-28"),digits=2)
   mar7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="03-01", wyend="03-31"),digits=2)
   apr7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="04-01", wyend="04-30"),digits=2)
   may7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="05-01", wyend="05-31"),digits=2)
   jun7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="06-01", wyend="06-30"),digits=2)
   jul7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="07-01", wyend="07-31"),digits=2)
   aug7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="08-01", wyend="08-31"),digits=2)
   sep7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="09-01", wyend="09-30"),digits=2)
   oct7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="10-01", wyend="10-31"),digits=2)
   nov7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="11-01", wyend="11-30"),digits=2)
   dec7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="12-01", wyend="12-31"),digits=2)
   
   jan30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="01-01", wyend="01-31"),digits=2)
   feb30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="02-01", wyend="02-28"),digits=2)
   mar30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="03-01", wyend="03-31"),digits=2)
   apr30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="04-01", wyend="04-30"),digits=2)
   may30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="05-01", wyend="05-31"),digits=2)
   jun30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="06-01", wyend="06-30"),digits=2)
   jul30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="07-01", wyend="07-31"),digits=2)
   aug30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="08-01", wyend="08-31"),digits=2)
   sep30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="09-01", wyend="09-30"),digits=2)
   oct30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="10-01", wyend="10-31"),digits=2)
   nov30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="11-01", wyend="11-30"),digits=2)
   dec30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="12-01", wyend="12-31"),digits=2)
   
   #combine into df
   months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
   sevens<-c(jan7,feb7,mar7,apr7,may7,jun7,jul7,aug7,sep7,oct7,nov7,dec7)
   thirtys<-c(jan30,feb30,mar30,apr30,may30,jun30,jul30,aug30,sep30,oct30,nov30,dec30)
   
   monthly<-data.frame("Month"=months,"7Q10"=sevens,"30Q5"=thirtys)
   
   monthly
   
 })
 
 #data for shiny app view
 output$monthlys<-renderDataTable({
   monthly()
 })
              
##### Excel Output ####
 
#want a page with query, page with raw data, page with flow calcs (can probably put all flow cals on one page)
 
#put any styles here
 #no styles yet
 
param<-eventReactive(input$goButton, {
  
  wb<-createWorkbook()
  
  #create strings for input parameters
  stations<- paste0("Stations = ",toString(sprintf("'%s'", input$Station)))
  startdt<-paste0("Startdate = ",toString(sprintf("%s",input$startd)))
  enddt<-paste0("Enddate = ",toString(sprintf("%s",input$endd)))
  
  addWorksheet(wb,"Search Criteria")
  
  #add title and data
  title<-"USGS Station Search Criteria"
  writeData(wb,sheet="Search Criteria",x=title,startRow=1,startCol=1)
  
  querydate<-paste0("Date of query, ",Sys.Date())
  writeData(wb,sheet="Search Criteria",x=querydate,startRow=2,startCol=1)
  
  writeData(wb,sheet="Search Criteria",x=stations,startRow=4,startCol=1)
  writeData(wb,sheet="Search Criteria",x=startdt,startRow=5,startCol=1)
  writeData(wb,sheet="Search Criteria",x=enddt,startRow=6,startCol=1)
  
  writeDataTable(wb,sheet="Search Criteria",x=info(),startRow=8,tableStyle="none")
  
  ##raw data sheet
  
  addWorksheet(wb,"Raw Data")
  
  writeData(wb,"Raw Data", startRow=1, x="All data is in cfs and all flow values are mean daily stream flow")
  writeDataTable(wb,"Raw Data",startRow=4, x=data(),tableStyle="none")
  
  ## flow calc data sheet
  addWorksheet(wb,"Flow Calculations")
  
  writeData(wb,"Flow Calculations", startRow=1, x="Calculations from flow data")
  
  writeData(wb,"Flow Calculations", startRow=3, x="7Q10 for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=3,startCol=2, x=sevenQten())
  
  writeData(wb,"Flow Calculations", startRow=5, x= "30Q5 for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=5,startCol=2, x=thirtyQfive())
  
  writeData(wb,"Flow Calculations", startRow=7, x="harmonic mean for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=7,startCol=2, x=harmonic())
  
  writeData(wb,"Flow Calculations", startRow=3, startCol=6, x="7Q10 and 30Q5 for each month of the year")
  writeDataTable(wb,"Flow Calculations", startRow=5, startCol=6, x=monthly(),tableStyle="none")
  
  wb
})
 
 #download option
 
 output$downloadData<-downloadHandler(
   
   filename = function() {paste(input$Station,"-DATA-USGSflowcalcs-", format(Sys.Date(),"%Y%m%d"),".xlsx", sep="")},
   content = function(file) {
     #sheet with query parameters
     saveWorkbook(param(),file)
   })
 
 }

#run application
shinyApp(ui=ui, server=server)


#####making sure Repo works
 
              
              
              
              
              
