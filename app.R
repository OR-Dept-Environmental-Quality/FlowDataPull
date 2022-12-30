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
library(ggplot2)

#harmonic mean function I made myself - note that this will only accept a df with one column
#the column needs to be flow values
dharmonic<-function(x) {
  
  return(harmonic.mean(x,zero=FALSE)*(length(x)-length(which(x==0)))/length(x))
  
}

#OWRD data pull function
source("OWRDdatapull.R")

# no valid values to query, user will need to know which USGS station they need in advance at the moment

#############################Define UI##########################
ui<-fluidPage(
  #sidebar with parameter inputs
  
  sidebarLayout(
    sidebarPanel(
      
      #add warning
      
      tags$em("Warning: Current code only handles ONE station at a time. If you want data for more than one station, please complete query and download the data separately"),
      
      #stations
      selectizeInput("type",
                     "Select Station Type",
                     choices=c("USGS","OWRD"),
                     multiple=FALSE),
      
      #station number
      textInput("Station",
      label="Station Number"),
    
      tags$em("Note: Flow calculations will be run on entire dataset selected"),
    
       # Start Date (make start date thirty years ago as default)
      dateInput("startd",
              label = "Select Start Date",
              min = '1900-01-01',
              value = Sys.Date()-10950),
       # End date
      dateInput("endd",
              label = "Select End Date",
              min = '1900-1-1'),
      
      #seasonal specifications - have to put full date
      tags$em("Specify any seasonal requirements here, default dates will automatically run the full water year"),
      
      textInput("startm",
                label="Select season start (mm-dd)",
                value = "10-01"),
      textInput("endm",
                label="Select season end (mm-dd)",
                value = "09-30"),
      
      #add action button, so query doesn't run until button is clicked
      actionButton("goButton","Run Query"),
      
      #add a download button so we can download query results
      downloadButton('downloadData', 'Download Data')
    ),
  
  #Setup main panel
  mainPanel(h1("USGS Flow Data Pull and Calculations"),
            h3("All data is in cfs and all flow values are mean daily stream flow"),
            h3("Water years with any data missing are excluded from flow calculation analyses"),
            tags$a(href="https://dashboard.waterdata.usgs.gov/app/nwd/?region=lower48&aoi=default","Click here to find sites at the USGS National Water Dashboard",target="_blank"),
            tags$br(),
            tags$a(href="https://apps.wrd.state.or.us/apps/sw/hydro_report/","Click here to find OWRD historical streamflow data"),
            
            #add line
            tags$hr(),
            #add break
            tags$br(),
            
            #multiple tabs: station info, original data, total calcs, and monthly calcs
            tabsetPanel(
              
              #USGS station info
              tabPanel("USGS Station Information",
                       h4("Station Information available only for USGS stations, not for OWRD stations"),
                       DT::dataTableOutput("info")),
              
              #velocity
              tabPanel("Velocity Data",
                       h4("Velocity data available only for USGS stations, not for OWRD stations"),
                       DT::dataTableOutput("velocity")),
              
              #depth
              tabPanel("Depth Data",
                       h4("Depth Data available only for USGS stations, not for OWRD stations"),
                      DT::dataTableOutput("depth")),
              
              #raw flow data
              tabPanel("Flow data",
                       DT::dataTableOutput("rawtable")),
              
              #plot of flow
              tabPanel("Flow graph",
                       tags$em("Y axis is in log scale"),
                       plotOutput("flowplot")),
              
              #monthly boxplots
              tabPanel("Mean Monthly Flow Boxplots",
                       plotOutput("boxplot"),
                       tags$em("Y axis is in log scale. Boxplots represent median and interquartile range (1st - 3rd quartiles). 
                               Minimum and maximum, shown as whiskers, represent values within 1.5 times the interquartile range. 
                               Outliers are greater than 1.5 but less than 3 times the interquartile range.
                               Red points are mean monthly flow.")),
                       
              
              #total 1Q10, 7Q10, 30Q5, and harmonic mean flow for time range specified
              tabPanel("Flow Calculations for all data",
                       textOutput("oneQten"),
                       textOutput("sevenQten"),
                       textOutput("thirtyQfive"),
                       textOutput("harmonic")),
              
              #monthly calcs for 1Q10, 7Q10 and 30Q5
              tabPanel("Monthly 1Q10, 7Q10, and 30Q5",
                       DT::dataTableOutput("monthlys"),
                       tags$em("February data does not include leap year days")),
              
              #seasonal calcs (specified by user)
              tabPanel("Seasonal 1Q10, 7Q10, and 30Q5",
                       DT::dataTableOutput("seasonal"),
                       tags$em("Seasons are specified in sidebar. "),
                       tags$em("February data does not include leap year days")),
            
              #missing dates
              tabPanel("Missing Flow Dates",
                     h3("Dates without flow information during the specified window"),
                     DT::dataTableOutput("missing")),
              
              #citations and information
              tabPanel("Information",
                       h4("Methodology for flow calculations are based on the DFLOW user manual as presented by Rossman (1999)."),
                       h4("If there are missing flow data in any particular water year, all data from that water year is omitted from the flow calculations. 
                          This approach is recommended by EPA (https://www.epa.gov/ceam/technical-support-dflow#xqy) and is applied in USGS's SWSTAT program, 
                          although it is not implemented in DFLOW 3.1."),
                       h5("Shiny Application created and maintained by Aliana Britson."),
                       h5("Code assistance credit to Ryan Michie and Vanessa Rose"),
                       h5("Validation assistance credit to Steven Schnurbusch and Erich Brandstetter"))
                     
  ),

#add icon to show when program is running query or download
add_busy_spinner(spin="fading-circle")
  )
)
)
            
              
###################  SERVER ###############################

server<- function(input, output, session) {
  
  #isolate data so that you have to click a button to run query using eventReactive
  
  #get station information
  info<-eventReactive(input$goButton, {
    
    if(input$type=="USGS"){
    info<-readNWISsite(input$Station)
    
    sub<-select(info, agency_cd,site_no,station_nm,dec_lat_va,dec_long_va,huc_cd)
    
    colnames(sub)<-c("Agency","Station","Name","Latitude","Longitude","HUC")
    
    sub}
    
    else{sub<-data.frame(No.Station.Data=character())}
    
    sub
  })
  
  #table for shiny view
  output$info<-renderDataTable({
    info()
  })
  
  #get USGS flow data
  data<-eventReactive(input$goButton, {
    
    if(input$type=="USGS") {
    #query data - returns mean daily flow in cfs
    q.df <- readNWISdv(siteNumbers = toString(sprintf("%s",input$Station)),
                       parameterCd = "00060",
                       startDate =toString(sprintf("%s",input$startd)),
                       endDate = toString(sprintf("%s",input$endd)),
                       statCd = "00003")
    
    q <- q.df[,c(3,4,5)]
    colnames(q) <-c("date", "flow","status")
    q$date <- as.POSIXct(q$date, format="%Y-%m-%d")
    }
    
    else{
      #query OWRD
      q.df<-owrd_data(station=toString(sprintf("%s",input$Station)),
                         startdate=toString(sprintf("%s",input$startd)),
                         enddate=toString(sprintf("%s",input$endd)),
                         char="MDF")
    p <- q.df[,c(2,5,8)]
    colnames(p) <-c("date", "flow","status")
    p$date <- as.POSIXct(p$date, format="%m-%d-%Y")
    q<-as.data.frame(p)
    }
    
    q
  })
 
 #table of queried raw data for shiny app view
 output$rawtable<-renderDataTable({
   data()
 })
 
 
 #get line plot of data
flowplot<-eventReactive(input$goButton, {
   q<-data()
   
   ggplot(q, aes(x = date, y = flow)) +
     geom_line() +
     scale_x_datetime(date_breaks="2 years", date_labels="%Y") +
     scale_y_continuous(labels=scales::label_number(),trans = 'log10')+ 
     theme_bw() +
     theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
           legend.position = "none") +
     labs(y = "Mean Daily Discharge (cfs)", x = "") +
     scale_color_brewer(palette = "Set1")
 })

output$flowplot<-renderPlot({
  flowplot()
})

##make boxplots of data
boxplot<-eventReactive(input$goButton, {
  q<-data()
  
q$Month<-format(q$date,"%b")
q$Month<-factor(q$Month, levels=month.abb)

ggplot(q, aes(x = Month, y = flow)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom ="point", shape = 20, size=3, color ="red", fill ="red") +
  scale_y_continuous(labels=scales::label_number(),trans = 'log10')+ 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(y = "Discharge (cfs)", x = "") +
  ggtitle("Boxplots of mean monthly flow") +
  scale_color_brewer(palette = "Set1")
})

output$boxplot<-renderPlot({
  boxplot()
})
 
 
 
 #get velocity data
 velocity<-eventReactive(input$goButton, { if(input$type=="USGS"){
   
   q.df <- readWQPqw(siteNumbers = paste0('USGS-',toString(sprintf("%s",input$Station))),
                      parameterCd = c("00055","70232", '72149', '72168',"72169","72190","72254","72255","72294","72321",
                                    "72322","72323","81380","81904"),
                     startDate =toString(sprintf("%s",input$startd)),
                      endDate = toString(sprintf("%s",input$endd))
                      )
   
   q.df<-select(q.df,OrganizationIdentifier, OrganizationFormalName,ActivityIdentifier,ActivityTypeCode,ActivityMediaName,
                ActivityMediaSubdivisionName,ActivityStartDate,ActivityStartTime.Time,ActivityStartTime.TimeZoneCode,
                ActivityConductingOrganizationText,MonitoringLocationIdentifier,HydrologicCondition,HydrologicEvent, 
                SampleCollectionMethod.MethodIdentifier,SampleCollectionMethod.MethodName, SampleCollectionEquipmentName,
                CharacteristicName,ResultMeasureValue,ResultMeasure.MeasureUnitCode,ResultStatusIdentifier,
                StatisticalBaseCode,ResultValueTypeName)
 }
   
   else {q.df<-data.frame(No.Velocity.Data=character())}
   
   q.df
 })
 
 #table of queried velocity data for shiny app view
 output$velocity<-renderDataTable({
   velocity()
 })
 
 #get depth data
 depth<-eventReactive(input$goButton, {if(input$type=="USGS"){
   
   q.df <- readWQPqw(siteNumbers = paste0('USGS-',toString(sprintf("%s",input$Station))),
                       parameterCd = c("00064","72178","72199","82903", "85310","85311"),
                      startDate =toString(sprintf("%s",input$startd)),
                      endDate = toString(sprintf("%s",input$endd)))
   
   q.df<-select(q.df,OrganizationIdentifier, OrganizationFormalName,ActivityIdentifier,ActivityTypeCode,ActivityMediaName,
                ActivityMediaSubdivisionName,ActivityStartDate,ActivityStartTime.Time,ActivityStartTime.TimeZoneCode,
                ActivityConductingOrganizationText,MonitoringLocationIdentifier,HydrologicCondition,HydrologicEvent, 
                SampleCollectionMethod.MethodIdentifier,SampleCollectionMethod.MethodName,SampleCollectionEquipmentName,
                CharacteristicName,ResultMeasureValue,ResultMeasure.MeasureUnitCode,ResultStatusIdentifier,
                StatisticalBaseCode,ResultValueTypeName)
 }
   else {q.df<-data.frame(No.Depth.Data=character())}
   
   q.df
 })
 
 #table of queried depth data for shiny app view
 output$depth<-renderDataTable({
   depth()
 })
 
 #calculate the total flow stats for 1Q10, 7Q10, 30Q5, and harmonic mean flow
 oneQten<-eventReactive(input$goButton, { if(nrow(data())!=0)
  { q<-data()[,c(1,2)]
   
   one<-dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")
   }
   
   else {one<-paste0("No flow data")}
   
   one
 })
 
 
 sevenQten<-eventReactive(input$goButton, {if(nrow(data())!=0)
   {q<-data()[,c(1,2)]
   
   seven<-dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")
  }
   
   else {seven<-paste0("No flow data")}
   
   seven
 })
   
   
 thirtyQfive<-eventReactive(input$goButton, {if(nrow(data())!=0)
   {q<-data()[,c(1,2)]
   
   thirty<-dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")
   }
   
   else {thirty<-paste0("No flow data")}
   
   thirty
 })

 
 harmonic<-eventReactive(input$goButton, {if(nrow(data())!=0) 
   {q.df<-data()
   q <- q.df[,2]
   
   harmonic<-dharmonic(q)
 }
  else{harmonic<-paste0("No flow data")} 
   
   harmonic
 })       
 
 #data for shiny app view
 output$oneQten<-renderText({
   if(nrow(data())!=0) {paste0("1Q10: ",round(oneQten(),digits=0))}
   else{paste0("1Q10: ",oneQten())}
 })
 output$sevenQten<-renderText({
   if(nrow(data())!=0) {paste0("7Q10: ",round(sevenQten(),digits=0))}
   else{paste0("1Q10: ",sevenQten())}
 })
 output$thirtyQfive<-renderText({
   if(nrow(data())!=0) {paste0("30Q5: ",round(thirtyQfive(),digits=0))}
   else{paste0("1Q10: ",thirtyQfive())}
 })
 output$harmonic<-renderText({
   if(nrow(data())!=0) {paste0("Harmonic Mean: ",round(harmonic(),digits=0))}
   else{paste0("1Q10: ",harmonic())}
 })
              
#calculate monthly 1Q10s, 7Q10s, and 30Q5s - merge into one dataframe
 #note that monthly flows for february will exclude a leap year day
 
 monthly<-eventReactive(input$goButton, {if(nrow(data())!=0){
   q<-data()[,c(1,2)]
   
   #calculate monthly dflow for 7Q10 and 30Q5
   jan1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="01-01", wyend="01-31"),digits=0)
   feb1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="02-01", wyend="02-28"),digits=0)
   mar1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="03-01", wyend="03-31"),digits=0)
   apr1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="04-01", wyend="04-30"),digits=0)
   may1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="05-01", wyend="05-31"),digits=0)
   jun1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="06-01", wyend="06-30"),digits=0)
   jul1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="07-01", wyend="07-31"),digits=0)
   aug1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="08-01", wyend="08-31"),digits=0)
   sep1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="09-01", wyend="09-30"),digits=0)
   oct1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="10-01", wyend="10-31"),digits=0)
   nov1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="11-01", wyend="11-30"),digits=0)
   dec1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart="12-01", wyend="12-31"),digits=0)
   
   jan7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="01-01", wyend="01-31"),digits=0)
   feb7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="02-01", wyend="02-28"),digits=0)
   mar7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="03-01", wyend="03-31"),digits=0)
   apr7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="04-01", wyend="04-30"),digits=0)
   may7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="05-01", wyend="05-31"),digits=0)
   jun7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="06-01", wyend="06-30"),digits=0)
   jul7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="07-01", wyend="07-31"),digits=0)
   aug7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="08-01", wyend="08-31"),digits=0)
   sep7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="09-01", wyend="09-30"),digits=0)
   oct7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="10-01", wyend="10-31"),digits=0)
   nov7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="11-01", wyend="11-30"),digits=0)
   dec7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart="12-01", wyend="12-31"),digits=0)
   
   jan30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="01-01", wyend="01-31"),digits=0)
   feb30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="02-01", wyend="02-28"),digits=0)
   mar30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="03-01", wyend="03-31"),digits=0)
   apr30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="04-01", wyend="04-30"),digits=0)
   may30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="05-01", wyend="05-31"),digits=0)
   jun30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="06-01", wyend="06-30"),digits=0)
   jul30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="07-01", wyend="07-31"),digits=0)
   aug30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="08-01", wyend="08-31"),digits=0)
   sep30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="09-01", wyend="09-30"),digits=0)
   oct30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="10-01", wyend="10-31"),digits=0)
   nov30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="11-01", wyend="11-30"),digits=0)
   dec30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="12-01", wyend="12-31"),digits=0)
   
   #combine into df
   months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
   ones<-c(jan1,feb1,mar1,apr1,may1,jun1,jul1,aug1,sep1,oct1,nov1,dec1)
   sevens<-c(jan7,feb7,mar7,apr7,may7,jun7,jul7,aug7,sep7,oct7,nov7,dec7)
   thirtys<-c(jan30,feb30,mar30,apr30,may30,jun30,jul30,aug30,sep30,oct30,nov30,dec30)
   
   monthly<-data.frame("Month"=months,"1Q10"=ones,"7Q10"=sevens,"30Q5"=thirtys)
 }
   else {monthly<-data.frame(No.Flow.Data=character())}
   
   monthly
   
 })
 
 #data for shiny app view
 output$monthlys<-renderDataTable({
   monthly<-datatable(monthly(), options=list(pageLength = 12))
   
   monthly
 })
 
 #do seasonal calculations for 1Q10, 7Q10, and 30Q5
 seasonal<-eventReactive(input$goButton, {if(nrow(data())!=0){
   
 q<-data()[,c(1,2)]
 
 seas1<-round(dflow(x=q, m=1, r=10, yearstart=NA, yearend=NA, wystart=input$startm, wyend=input$endm),digits=0)
 seas7<-round(dflow(x=q, m=7, r=10, yearstart=NA, yearend=NA, wystart=input$startm, wyend=input$endm),digits=0)
 seas30<-round(dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart=input$startm, wyend=input$endm),digits=0)
 
 #combine into df
 seasonal<-data.frame("1Q10"=seas1,"7Q10"=seas7,"30Q5"=seas30)}
   
 else {seasonal<-data.frame(No.Flow.Data=character())}
 
 seasonal
 
 })
 
 #data for shiny app view
 output$seasonal<-renderDataTable({
   seasonal()
 })
 
 #report on missing flow dates
 missing<-eventReactive(input$goButton, {if(nrow(data())!=0) {
   
   if(input$type=="USGS"){q<-data()
   
   q$asDate<-as.Date(q$date)
   DateRange<-seq(min(q$asDate),max(q$asDate),by=1)
   Missing<-DateRange[!DateRange %in% q$asDate]
   framemissing<-data.frame(Missing)
   }
   
   else{framemissing<-subset(data(),is.na(data()$flow))}
   
 }
   
   else{framemissing<-data.frame(No.Flow.Data=character())}
   
   framemissing
 })
 
 #output for shiny app view
 output$missing<-renderDataTable({
   missing()
   })
 
##### Excel Output ####
 
#want a page with query, page with raw data, page with flow calcs (can probably put all flow cals on one page)
 
#put any styles here
 #no styles yet
 
param<-eventReactive(input$goButton, {
  
  wb<-createWorkbook()
  
  #create strings for input parameters
  type<-paste0("Station Type = ",toString(sprintf("'%s'", input$type)))
  stations<- paste0("Stations = ",toString(sprintf("'%s'", input$Station)))
  startdt<-paste0("Startdate = ",toString(sprintf("%s",input$startd)))
  enddt<-paste0("Enddate = ",toString(sprintf("%s",input$endd)))
  startmm<-paste0("Start Season = ",toString(sprintf("%s",input$startm)))
  endmm<-paste0("End Season = ",toString(sprintf("%s",input$endm)))
  
  addWorksheet(wb,"Search Criteria")
  
  #add title and data
  title<-"USGS Station Search Criteria"
  writeData(wb,sheet="Search Criteria",x=title,startRow=1,startCol=1)
  
  querydate<-paste0("Date of query, ",Sys.Date())
  writeData(wb,sheet="Search Criteria",x=querydate,startRow=2,startCol=1)
  
  writeData(wb,sheet="Search Criteria",x=type,startRow=4,startCol=1)
  writeData(wb,sheet="Search Criteria",x=stations,startRow=5,startCol=1)
  writeData(wb,sheet="Search Criteria",x=startdt,startRow=6,startCol=1)
  writeData(wb,sheet="Search Criteria",x=enddt,startRow=7,startCol=1)
  writeData(wb,sheet="Search Criteria",x=startmm,startRow=8,startCol=1)
  writeData(wb,sheet="Search Criteria",x=endmm,startRow=9,startCol=1)
  
  writeDataTable(wb,sheet="Search Criteria",x=info(),startRow=11,tableStyle="none")
  
  ##raw data sheet
  
  addWorksheet(wb,"Raw Data")
  
  writeData(wb,"Raw Data", startRow=1, x="All data is in cfs and all flow values are mean daily stream flow")
  writeDataTable(wb,"Raw Data",startRow=4, x=data(),tableStyle="none")
  
  #graphs data sheet
  addWorksheet(wb,"Graphs")
  
  #flow plot
  png("flowplot.png")
  print(flowplot())
  dev.off()
  insertImage(wb,"Graphs","flowplot.png",startRow=3, startCol=1)
  writeData(wb,"Graphs",startRow=1,startCol=1,x="Y axis is in log scale")
  
  #boxplot
  png("boxplot.png")
  print(boxplot())
  dev.off()
  insertImage(wb,"Graphs","boxplot.png",startRow=3, startCol=10)
  writeData(wb,"Graphs",startRow=1,startCol=10,x="Y axis is in log scale. Boxplots represent median and interquartile range (1st - 3rd quartiles). Minimum and maximum, shown as whiskers, represent values within 1.5 times the interquartile range.  Outliers are greater than 1.5 but less than 3 times the interquartile range. Red points are mean monthly flow.")
  
  ## flow calc data sheet
  addWorksheet(wb,"Flow Calculations")
  
  writeData(wb,"Flow Calculations", startRow=1, x="Calculations from flow data")
  
  writeData(wb,"Flow Calculations", startRow=3, x="1Q10 for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=3,startCol=2, x=oneQten())
  
  writeData(wb,"Flow Calculations", startRow=5, x="7Q10 for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=5,startCol=2, x=sevenQten())
  
  writeData(wb,"Flow Calculations", startRow=7, x= "30Q5 for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=7,startCol=2, x=thirtyQfive())
  
  writeData(wb,"Flow Calculations", startRow=9, x="harmonic mean for complete timeframe")
  writeData(wb,"Flow Calculations", startRow=9,startCol=2, x=harmonic())
  
  writeData(wb,"Flow Calculations", startRow=3, startCol=6, x="1Q10, 7Q10, and 30Q5 for each month of the year")
  writeDataTable(wb,"Flow Calculations", startRow=5, startCol=6, x=monthly(),tableStyle="none")
  
  writeData(wb,"Flow Calculations", startRow=3, startCol=12, x="1Q10, 7Q10, and 30Q5 for season specified")
  writeData(wb,"Flow Calculations", startRow=4, startCol=12, x=startmm)
  writeData(wb,"Flow Calculations", startRow=5, startCol=12, x=endmm)
  writeDataTable(wb,"Flow Calculations", startRow=6, startCol=12, x=seasonal(),tableStyle="none")
  
  #velocity data sheet
  addWorksheet(wb,"Velocity")
  writeDataTable(wb,"Velocity",startRow=2,x=velocity(),tableStyle="none")
  
  #depth data sheet
  addWorksheet(wb,"Depth")
  writeDataTable(wb,"Depth",startRow=2,x=depth(),tableStyle="none")
  
  #add missing data sheet
  addWorksheet(wb,"Missing Flow")
  writeData(wb,"Missing Flow",startRow=1,x="Data without flow information during the specified search window")
  writeData(wb,"Missing Flow",startRow=2,x="Water years with missing data are excluded from flow calculation analyses")
  writeDataTable(wb,"Missing Flow",startRow=3,x=missing(),tableStyle="none")
  
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
 
              
              
              
              
              
