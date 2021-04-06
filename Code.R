#loading packages needed
library(plotly)#for the interactive bar charts
library(shiny)#for the front end
library(leaflet)#for plotting map
library(shinythemes)#for giving front end a new look
library(readxl)#to read xlsx files
library(plyr)#for grouping tables
library(reshape)#for forming pivot tables
library(forecast)#for forecasting

#for getting max file iput size as 200 MB
options(shiny.maxRequestSize=200*1024^2)

#creating the tool
app=shinyApp(
  
  #creating the front end of the tool
  ui<-shinyUI(fluidPage(theme=shinytheme("superhero"),
                        
                        #for the title
                        titlePanel(title="Alarm Prediction and Resource Reallocation"),
                        
                        #for the side panel(input the files and other options) 
                        sidebarLayout(
                          sidebarPanel(h3("Input"),fileInput("previous","Upload the data of previous observations in csv format"),
                                       fileInput("new","Upload new data in xlsx format ",multiple = TRUE),
                                       textInput("date","Input the date of the file in mm/dd/yyyy.xlsx format"),#textOutput("filesadded"),
                                       fileInput("plotdata","Upload the geographical data for plotting in csv format"),
                                       fileInput("engineerdata","Upload the Lacwise no of engineers data in csv format"),
                                       helpText("Maximum input file size is 200 MB"),downloadButton("data_all","Download total data")
                                       ,uiOutput("V1"),uiOutput("V2"),uiOutput("V3"),uiOutput("V4")),
                          
                          #for the main working space having different tabs for different outputs
                          mainPanel(h3("Output"),tabsetPanel(type="tab",
                                                             
                                                             #for the forecasts                                  
                                                             tabPanel("Forecasts",value=1,tableOutput("forecastedresults"),downloadButton("forecast","Download"),h5("The duration is in minutes")),
                                                             
                                                             #for the plots
                                                             tabPanel("Plots for Alarms",value=2,h4("Total Alarms:"),tableOutput("total_alarms"),h4("Top Contributors"),
                                                                      tableOutput("contributors"),h5("Plot for lac and alarm combination"),plotOutput("plot")),
                                                             
                                                             #for the engineer recommendation
                                                             tabPanel("Recommendation",value=3,h4("Recommendation of Engineers"),tableOutput("recommendation"),downloadButton("engineers","Download")
                                                                      ,h5(" '-' indicates that these no of enggineers in that particular lac are extra and can be shifted to another lac")
                                                                      ,h5("The duration is in minutes")),
                                                             
                                                             #for the map representation
                                                             tabPanel("Map Representation for severity according to total counts per LAC",value=4,leafletOutput("map",width="100%",height=1350),
                                                                      h4("lacs in critical region:"),verbatimTextOutput("lacs_in_critical"),h4("lacs in moderately severe region")
                                                                      ,verbatimTextOutput("lacs_in_moderate")),
                                                             
                                                             #for the barplots
                                                             tabPanel("Barplots for describing Alarm constitution per LAC",value=5,h4("Stacked Barplots for Lac wise distribution of alarms"),
                                                                      plotlyOutput("barplot")),id="tabselected"))))),
  
  
  
  #the backend
  server<- shinyServer(function(input,output,session){
    
    #for retreiving previous data from input files
    previous1<-reactive({file1<-input$previous
    if (is.null(file1)){return ()}
    read.table(file=file1$datapath,sep=",",header=TRUE,check.names = FALSE)
    })
    
    #working on creating a pivot table from the previous data of STARTTIME~Lac+alarm
    previous11<-reactive({
      prev<-previous1()
      prev$count<-1
      prev<-prev[,c(2,8,9)]
      #prev$STARTTIME<-substr(prev$STARTTIME,1,10)
      print(length(unique(prev$lacal)))
      print(length(unique(prev$STARTTIME)))
      prev1<-aggregate(count~.,data=prev,FUN=sum)
      print(dim(prev1))
      print(colnames(prev1))
      x1<-unique(prev$STARTTIME)
      y1<-unique(prev$lacal)
      prev2<-merge(x1,y1)
      prev2$STARTTIME<-prev2$x
      prev2$lacal<-prev2$y
      prev2[,c('x','y')]<-NULL
      prev3<-merge(prev2,prev1,all.x=TRUE,all.y=FALSE)
      prev3$count[is.na(prev3$count)==TRUE]<-0
      table1<-cast(prev3,`STARTTIME`~`lacal`)
      table1
    })
    
    #for  retrieving the plot data from input files
    plot1<-reactive({file1<-input$plotdata
    if (is.null(file1)){return ()}
    read.table(file=file1$datapath,sep=",",header=TRUE)})
    
    #for retreiving the engineer data from the input files
    engineerdata1<-reactive({file1<-input$engineerdata
    if (is.null(file1)){return ()}
    read.table(file=file1$datapath,sep=",",check.names = FALSE)})
    
    #for reteiving the new data file
    neww1<-reactive({
      #for(i in 1:length(input$new[,1])){
      infile<-input$new#[i,1]
      file.rename(infile$datapath,paste(infile$datapath, ".xlsx", sep=""))
      #lst[[i]]<-
      read_excel(paste(infile$datapath, ".xlsx", sep=""),sheet="WCell",col_names = FALSE)#read.csv(input$files[[i, 'datapath']])
      #return(lst)}
    })
    
    #for getting the new xlsx file in workable format
    new1<-reactive({plot2<-plot1()
    wcellread1<-neww1()
    wcellall<-NULL
    #for (i in c(1:wcellread1)){
    wcellread<-wcellread1#[[i]]
    wcellread<-wcellread[-c(1:3),]
    colnames(wcellread)<-wcellread[1,]
    wcellread<-wcellread[-1,]
    wcellread<-wcellread[,c("WCELL","STARTTIME","DURATION IN MIN","ALNO","ALARM_DESCRIPTION","SUPPLEMENTARY_INFO")]
    #wcellread$date<-substr(infile$name,1,nchar(infile$name)-5)
    #wcellread$date<-gsub("-","/",wcellread$date,fixed=T)
    wcellread$date<-input$date
    wcellall<-rbind(wcellall,wcellread)#}
    wcellall$lac<-plot2[match(wcellall$WCELL,plot2$BTS_NAME),21]
    wcellall$STARTTIME<-substr(wcellall$STARTTIME,1,10)
    wcellall$STARTTIME<-as.Date(wcellall$STARTTIME,"%m/%d/%Y")
    wcellall$date<-as.Date(wcellall$date,"%m/%d/%Y")
    wcellall$date<-wcellall$date-1
    print(wcellall)
    print(plot2)
    wcellall<-wcellall[wcellall$STARTTIME==wcellall$date,]
    wcellall<-wcellall[is.na(wcellall$lac)==FALSE,]
    wcellall$lacal<-paste(wcellall$lac,wcellall$ALNO)
    wcellall$date<-NULL
    return(wcellall)
    
    })
    
    #forming a total pivot table combing previous and new data giving count of alarms as STARTTIME~Lac+Alarm combination
    totaldata<-reactive({
      new2<-new1()
      new2$count<-1
      new2<-new2[,c(2,8,9)]
      new3<-aggregate(new2$count~.,data=new2,FUN=sum)
      x1<-unique(new2$STARTTIME)
      y1<-unique(new2$lacal)
      new4<-merge(x1,y1)
      new4$STARTTIME<-new4$x
      new4$lacal<-new4$y
      new4[,c('x','y')]<-NULL
      new5<-merge(new4,new2,all.x=TRUE,all.y=FALSE)
      new5$count[is.na(new5$count)==TRUE]<-0
      table<-cast(new5,`STARTTIME`~`lacal`)
      print(table)
      previous2<-previous11()
      print(previous2)
      if (is.null(previous2)){return(table)
      } else {z<-rbind.fill(previous2,table)
      z[is.na(z)]<-0
      z}
    }) 
    
    #merging new and previous data for download that can be used as input for previous data for the next day
    dataall<-reactive({
      new2<-new1()
      previous2<-previous1()
      new2$date<-NULL
      print(colnames(previous2))
      print(colnames(new2))
      dataq<-rbind(previous2,new2)
      dataq
    })
    
    #taking out the average time from total merged data for each Lac+Alarm combination
    timeavg<-reactive({
      data<-dataall()
      #data<-data[is.na(data$`DURATION IN MIN`)==FALSE,]
      data$`DURATION IN MIN`<-as.numeric(data$`DURATION IN MIN`)
      data1<-aggregate(`DURATION IN MIN`~lacal,data=data,FUN=mean,na.action=na.pass,na.rm=TRUE)
      print(summary(data1))
      print(data)
      data1
    })
    
    #getting the forecasts on merged pivot table
    results1<-reactive({
      totaldata1<-totaldata()
      tm<-timeavg()
      print(dim(totaldata1))
      resultss2<-NULL   #initialising the results
      resultss2$lacal[c(1:ncol(totaldata1))]<-1
      resultss2$total_count_of_forecasts[c(1:ncol(totaldata1))]<-1
      resultss2$maximum_total_count_of_forecasts[c(1:ncol(totaldata1))]<-1
      resultss2<-as.data.frame(resultss2)
      
      #getting arima models
      for (i in c(2:ncol(totaldata1))){
        count<-ts(totaldata1[,i],frequency=365)
        model1<-auto.arima(count[1:nrow(totaldata1)])  
        result3<-forecast(model1,h=30)
        resultdf<-as.data.frame(result3)
        resultdf[resultdf<0]<-0
        resultdf[,c(1,3)]<-round(resultdf[,c(1,3)])
        
        #recording the results
        resultss2$lacal[i]<-colnames(totaldata1)[i]
        resultss2$total_count_of_forecasts[i]<-sum(resultdf[,1])
        resultss2$maximum_total_count_of_forecasts[i]<-sum(resultdf[,3])
      }
      #recording some more observations
      resultss2<-resultss2[c(2:ncol(totaldata1)),]
      resultss2$lac<-substr(resultss2$lacal,1,4)
      resultss2$average_count_for_one_day<-round(resultss2$total_count_of_forecasts/30)
      resultss2$maximum_count_for_one_day<-round(resultss2$maximum_total_count_of_forecasts/30)
      resultss2$duration_for_one<-tm[match(resultss2$lacal,tm$lacal),2]
      resultss2$total_duration_for_30days<-resultss2$total_count_of_forecasts*resultss2$duration_for_one
      resultss2$total_duration_maximum_for_30days<-resultss2$maximum_total_count_of_forecasts*resultss2$duration_for_one
      resultss2$duration_for_one_day<-resultss2$average_count_for_one_day*resultss2$duration_for_one
      resultss2$maximum_duration_for_one_day<-resultss2$maximum_count_for_one_day*resultss2$duration_for_one
      nr<-nrow(totaldata1)
      nc<-ncol(totaldata1)
      for (i in c(1:(nc-1))){
        resultss2$last_month_count[i]<-sum(totaldata1[c((nr-29):nr),i+1])
        resultss2$second_last_month_count[i]<-sum(totaldata1[c((nr-59):(nr-30)),i+1])
        resultss2$third_last_month_count[i]<-sum(totaldata1[c((nr-89):(nr-60)),i+1])
      }
      resultss2})
    
    
    #aggregating the results Lacwise
    lacresults1<-reactive({
      results2<-results1()
      results2$lacal<-NULL
      results2$duration_for_one<-NULL
      lacresults11<-aggregate(.~lac,data=results2,FUN=sum)
      lacresults11
    })
    
    #to download the overall merged data for input on the next day
    output$data_all<-downloadHandler(filename=function(){paste("Alldata",".csv",sep=",")
    },content=function(file){write.table(dataall(),file,sep=",",row.names=FALSE)})
    
    #for recommendation of engineers
    recommendation1<-reactive({
      lacresults2<-lacresults1()
      engineerdata2<-engineerdata1()
      
      #merging engineers lacwise
      lacresults2$engineers<-engineerdata2[match(lacresults2$lac,engineerdata2[,1]),2]
      print(summary(lacresults2$engineers))
      print(length(unique(engineer[,1])))
      
      #creating the recommendation table
      recommendationn<-lacresults2[,c(1,2,3,6,7,13)]
      recommendationn$engineers<-as.numeric(recommendationn$engineers)
      
      #getting average duration per engineer
      recommendationn$duration_per_engineer<-round(recommendationn$total_duration_for_30days/recommendationn$engineers)
      recommendationn$max_duration_per_engineer<-round(recommendationn$total_duration_maximum_for_30days/recommendationn$engineers)
      print(summary(recommendationn))
      median3<-as.numeric(summary(recommendationn$duration_per_engineer)[3])
      median4<-as.numeric(summary(recommendationn$max_duration_per_engineer)[3])
      mean1<-mean(recommendationn$duration_per_engineer)
      mean2<-mean(recommendationn$max_duration_per_engineer)
      x<-NULL
      y<-NULL
      z<-length(unique(lacresults2$lac))
      #getting optimum amunt of engineers 
      for (i in c(1:z)){
        x[i]<-as.numeric(round(recommendationn$total_duration_for_30days[i]/mean1))
        y[i]<-as.numeric(round(recommendationn$total_duration_maximum_for_30days[i]/mean2))
        if (x[i]<y[i])
        {recommendationn$recommended_engineers[i]<-x[i]#paste(x[i],"to",y[i])}
        } else if (x[i]>y[i]){
          recommendationn$recommended_engineers[i]<-y[i]#paste(y[i],"to",x[i])
        }
        else 
          recommendation$recommended_engineers[i]<-x[i]
        
      }
      print(summary(recommendationn))
      
      #getting delta of engineers required
      recommendationn1<-recommendationn[,c(1:6,9)]
      for (i in c(1:z)){
        if (recommendationn1$recommended_engineers[i]==0){recommendationn1$recommended_engineers[i]<-1}
        x[i]<-recommendationn1$recommended_engineers[i]#strsplit(recommendationn1$recommended_engineers[i],"to")[[1]][1]
        #if (x[i]==0){x[i]<-1}
        recommendationn1$needed_engineers[i]<-as.numeric(x[i])-as.numeric(recommendationn1$engineers[i])
      }
      print(summary(recommendationn1))
      return(recommendationn1)
    }) 
    
    #assigning input variables their work for output variables
    output$moderate<-renderText(input$moderate)
    output$critical<-renderText(input$critical)
    output$lac<-renderText(input$lac)
    
    #for the forecasts' output
    output$forecastedresults<-renderTable({lacresults2<-lacresults1()
    lacresults2})
    
    #for downloading the forecasts
    output$forecast<-downloadHandler(filename=function(){paste("Forecasts",".csv",sep=",")
    },content=function(file){write.table(lacresults1(),file,sep=",",row.names=FALSE)})
    
    #for rendering input dependent UI 
    output$V1<-renderUI({lacresults2<-lacresults1()
    sliderInput("moderate","select minimum no of alarms for moderately severe level",min=0,max=max(lacresults2[,2]),
                value=summary(lacresults2[,2])[3],step=1)})
    output$V2<-renderUI({lacresults2<-lacresults1()
    sliderInput("critical","select minimum no of alarms for critical level",min=0,max=max(lacresults2[,2]),
                value=summary(lacresults2[,2])[5],step=1)})
    output$V3<-renderUI({lacresults2<-lacresults1()
    selectInput("lac","Select lac for information",choices =c(unique(lacresults2$lac)),multiple = TRUE)})
    output$V4<-renderUI({results2<-results1()
    selectInput("lacal","Select a lac and alarm combination for plot",c(unique(results2$lacal)))
    }) 
    
    #for identifying lacs in different severity zones
    output$lacs_in_moderate<-renderText({moderate<-as.numeric(input$moderate)
    critical<-as.numeric(input$critical)
    lacresults2<-lacresults1()
    y<-lacresults2[,1][lacresults2[,2]>moderate&lacresults2[,2]<critical]
    paste(y)})  
    
    output$lacs_in_critical<-renderText({moderate<-as.numeric(input$moderate)
    critical<-as.numeric(input$critical)
    lacresults2<-lacresults1()
    y<-lacresults2[,1][lacresults2[,2]>critical]
    paste(y)})
    
    #for getting total alarms per lac
    output$total_alarms<-renderTable({
      df<-NULL
      lacresults2<-lacresults1()
      for (i in c(1:length(input$lac))){
        df$Lac[i]<-input$lac[i]
        df$Total_alarms[i]<-lacresults2[,2][lacresults2$lac==input$lac[i]]
      }
      df})
    
    #for getting arima plots for particular Lac+Alarm combination
    output$plot<-renderPlot({
      totaldata1<-totaldata()
      x<-input$lacal
      count<-ts(totaldata1[,x],frequency=365)
      library(forecast)
      model<-auto.arima(count[1:length(count)])
      resultant<-forecast(model,h=30)
      resultant1<-as.data.frame(resultant)
      plot(resultant,ylim=c(0,max(count)),main=paste(x,":",sum(round(resultant1[,1])),"-",sum(round(resultant1[,3]))),xlab="Time",ylab="Count")
    })
    
    #for getting recommendation table output
    output$recommendation<-renderTable({recommendation2<-recommendation1()
    recommendation2})
    output$engineers<-downloadHandler(filename=function(){paste("Recommendation of engineers",".csv",sep=",")
    },content=function(file){write.table(recommendation1(),file,sep=",",row.names=FALSE)})
    
    #for getting the output of top 10 contributing alarms of a particuar lac
    output$contributors<-renderTable({
      top_contributors<-NULL
      results2<-results1()
      for (i in c(1:length(input$lac))){
        df<-results2[results2[,4]==input$lac[i],c(1,2)]
        df$lacal<-as.character(df$lacal)
        df<-df[with(df,order(-df$total_count_of_forecasts)),]
        df<-df[df$total_count_of_forecasts!=0,]
        df<- subset(df,df$total_count_of_forecasts!=0)
        df$lac<-input$lac[i]
        if (nrow(df)<11){
          top_contributors<-rbind(top_contributors,df)
        } 
        else {
          top_contributors<-rbind(top_contributors,df[c(1:10),])
        }
      }
      
      top_contributors
    })
    
    #pal <-reactive({plot2<-plot1()colorFactor(palette = "YlGnBu",domain = plot2$severity)})
    
    #for map representation
    getColor <- reactive({
      plot2<-plot1()
      function(plot2) {
        sapply(plot2$severity, function(severity) {
          if(`severity`==1) {
            "blue"
          } else if(`severity`==1) {
            "brown"
          } else {"red"} })}
    })
    
    
    
    output$map <- renderLeaflet({
      #colorpal<-pal()
      getc<-getColor()
      lacresults2<-lacresults1()
      moderate<-as.numeric(input$moderate)
      critical<-as.numeric(input$critical)
      plot2<-plot1()
      plot2<-plot2[,c("BTS_NAME","Longitude","Latitude","LAC")]
      plot2<-plot2[!duplicated(plot2),]
      plot2$count<-lacresults2[match(plot2$LAC,lacresults2$lac),2]
      
      #assigning severity on basis of count
      plot2$severity[plot2$count>moderate&plot2$count<critical]<-2
      plot2$severity[plot2$count>critical]<-3
      plot2$severity[plot2$count<moderate]<-1
      plot2.df <- split(plot2,plot2$LAC )
      
      icons <- awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = getc(plot2.df) )
      
      l <- leaflet() %>% addTiles()
      
      names(plot2.df) %>%
        purrr::walk( function(df) {
          l <<- l %>%
            addAwesomeMarkers(data=plot2.df[[df]],
                              lng=~Longitude, lat=~Latitude,
                              label=~as.character(~LAC),
                              popup=~as.character(~LAC),icon=icons,
                              group = df,#color=~colorpal(plot2.df$severity),
                              clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                              labelOptions = labelOptions(noHide = F,
                                                          direction = 'auto'))
        })
      
      l %>%
        addLayersControl(
          overlayGroups = names(x1.df),
          options = layersControlOptions(collapsed =FALSE) #%>% setView(42, 16, 4)
        )})
    
    
    #for barplots showing top 5 contributors of alarms in a lac
    output$barplot<-renderPlotly({
      top_contributors<-NULL
      results2<-results1()
      for (i in c(1:length(input$lac))){
        df<-results2[results2[,4]==input$lac[i],c(1,2)]
        df$lacal<-as.character(df$lacal)
        df<-df[with(df,order(-df$total_count_of_forecasts)),]
        df<-df[df$total_count_of_forecasts!=0,]
        
        df$lac<-input$lac[i]
        if (nrow(df)<6){
          top_contributors<-rbind(top_contributors,df)
        } 
        else {
          top_contributors<-rbind(top_contributors,df[c(1:5),])
        }
      }
      plot_ly(top_contributors) %>% 
        add_trace(data = top_contributors, type = "bar", x = ~lac, y = ~total_count_of_forecasts, color = ~lacal) %>%  
        layout(barmode = "stack") 
    }) 
    
  })
)
