required <- c("shiny","shinydashboard","DT","plotly","dplyr","readr","ggplot2","scales","lubridate","randomForest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) stop("Install packages first: install.packages(c(",paste(sprintf('"%s"',missing),collapse=", "),"))")
library(shiny); library(shinydashboard); library(DT); library(plotly); library(dplyr)
library(readr); library(ggplot2); library(scales); library(lubridate)

DATA_FILE <- "data/air_quality.csv"
make_demo <- function(n=1800,seed=27) {
 set.seed(seed)
 date <- seq(as.POSIXct("2023-01-01 00:00:00",tz="UTC"),by="hour",length.out=n)
 season <- 10*sin(2*pi*yday(date)/365)
 rush <- ifelse(hour(date)%in%c(7,8,9,17,18,19),12,0)
 temp <- 25 + 7*sin(2*pi*yday(date)/365) + rnorm(n,0,3)
 humidity <- pmin(pmax(65 - .45*(temp-25)+rnorm(n,0,12),20),100)
 pm25 <- pmax(2,round(32+season+rush+rnorm(n,0,14),1))
 pm10 <- pmax(pm25+2,round(pm25*1.55+rnorm(n,0,12),1))
 no2 <- pmax(2,round(18+rush*.8+rnorm(n,0,8),1))
 so2 <- pmax(1,round(7+rnorm(n,0,3),1))
 co <- pmax(.1,round(.35+rush*.018+rnorm(n,0,.15),2))
 o3 <- pmax(1,round(24 + .7*temp - .2*humidity + rnorm(n,0,7),1))
 station <- sample(c("City Centre","Industrial Zone","Residential North","Traffic Corridor","Urban Park"),n,TRUE)
 tibble(Date=date,Station=station,PM2.5=pm25,PM10=pm10,NO2=no2,SO2=so2,CO=co,O3=o3,Temperature=round(temp,1),Humidity=round(humidity,1))
}
if(file.exists(DATA_FILE)) {
 raw <- tryCatch(read_csv(DATA_FILE,show_col_types=FALSE),error=function(e) make_demo())
 if(!nrow(raw)) raw<-make_demo()
} else raw<-make_demo()
names(raw)<-make.names(names(raw),unique=TRUE)
# Support typical pollutant and timestamp column variants.
rename_if_missing <- function(target,aliases) {
 if(!target%in%names(raw)) { hit<-intersect(make.names(aliases),names(raw)); if(length(hit)) names(raw)[match(hit[1],names(raw))]<<-target }
}
rename_if_missing("Date",c("Date","Datetime","Timestamp","date","time","Local.Time"))
rename_if_missing("Station",c("Station","City","Location","Site","station"))
rename_if_missing("PM.2.5",c("PM2.5","PM25","pm2_5","PM_2_5","pm25"))
rename_if_missing("PM10",c("PM10","pm10"))
rename_if_missing("NO2",c("NO2","no2"))
rename_if_missing("SO2",c("SO2","so2"))
rename_if_missing("CO",c("CO","co"))
rename_if_missing("O3",c("O3","Ozone","o3"))
rename_if_missing("Temperature",c("Temperature","Temp","temperature"))
rename_if_missing("Humidity",c("Humidity","RH","humidity"))
if(!"Date"%in%names(raw)) raw$Date<-seq(as.Date("2023-01-01"),by="day",length.out=nrow(raw))
raw$Date <- suppressWarnings(as.POSIXct(raw$Date,tz="UTC"))
if(all(is.na(raw$Date))) raw$Date<-as.POSIXct(seq_len(nrow(raw)),origin="2023-01-01",tz="UTC",unit="days")
if(!"Station"%in%names(raw)) raw$Station<-"All stations"
for(nm in c("PM.2.5","PM10","NO2","SO2","CO","O3","Temperature","Humidity")) {
 if(!nm%in%names(raw)) raw[[nm]]<-NA_real_
 raw[[nm]]<-suppressWarnings(as.numeric(raw[[nm]]))
}
raw$Station<-as.character(raw$Station)
aqi_band <- function(x) ifelse(is.na(x),"Unavailable",ifelse(x<=12,"Good",ifelse(x<=35.4,"Moderate",ifelse(x<=55.4,"Sensitive groups",ifelse(x<=150.4,"Unhealthy","Very unhealthy")))))
raw$PM25Band<-aqi_band(raw$PM.2.5)

ui <- dashboardPage(skin="blue",
 dashboardHeader(title="Air Quality Intelligence",titleWidth=290),
 dashboardSidebar(width=290,sidebarMenu(
  menuItem("Executive Overview",tabName="overview",icon=icon("tachometer-alt")),
  menuItem("Pollutant Trends",tabName="trends",icon=icon("chart-line")),
  menuItem("Station Comparison",tabName="stations",icon=icon("map-marker-alt")),
  menuItem("Prediction Lab",tabName="predict",icon=icon("brain")),
  menuItem("Data Explorer",tabName="data",icon=icon("table"))),
  tags$hr(),h4("Filters",style="padding-left:15px;color:#d9e7f2;"),
  selectInput("station","Monitoring station",choices=c("All stations",sort(unique(raw$Station)))),
  dateRangeInput("dates","Date range",start=as.Date(min(raw$Date,na.rm=TRUE)),end=as.Date(max(raw$Date,na.rm=TRUE))),
  selectInput("pollutant","Primary pollutant",choices=c("PM2.5"="PM.2.5","PM10"="PM10","NO₂"="NO2","SO₂"="SO2","CO"="CO","O₃"="O3"),selected="PM.2.5"),
  actionButton("reset","Reset filters",icon=icon("undo"),class="btn-primary")),
 dashboardBody(tags$style(HTML("
 .content-wrapper,.right-side{background:#f2f6fa}.main-header .logo{font-weight:700}
 .small-box,.box{border-radius:12px;box-shadow:0 5px 18px rgba(28,48,74,.08)}
 .content{padding:20px}.hero{padding:20px 24px;background:linear-gradient(120deg,#102a43,#176b87);color:white;border-radius:14px;margin-bottom:18px}
 .hero h2{margin-top:0;font-weight:700}.skin-blue .main-sidebar{background:#102a43}
 .skin-blue .main-header .logo,.skin-blue .main-header .navbar{background:#0b1f33}
 .skin-blue .sidebar-menu>li.active>a{border-left-color:#39c6c8}")),
 tabItems(
  tabItem(tabName="overview",
   fluidRow(box(width=12,solidHeader=FALSE,div(class="hero",h2("Air Quality Analysis & Prediction"),p("Monitor pollutant patterns, compare stations and explore a baseline predictive model.")))),
   fluidRow(valueBoxOutput("records",3),valueBoxOutput("pm25",3),valueBoxOutput("pm10",3),valueBoxOutput("stations",3)),
   fluidRow(box(title="Primary pollutant over time",width=8,status="primary",plotlyOutput("trend",height=330)),
    box(title="PM2.5 air-quality bands",width=4,status="primary",plotlyOutput("bands",height=330))),
   fluidRow(box(title="Pollutant profile",width=6,status="primary",plotlyOutput("pollutants",height=320)),
    box(title="Weather context",width=6,status="primary",plotlyOutput("weather",height=320)))),
  tabItem(tabName="trends",
   fluidRow(box(title="Pollutant time series",width=12,status="primary",plotlyOutput("timeseries",height=390))),
   fluidRow(box(title="Daily pattern",width=6,status="primary",plotlyOutput("daily",height=320)),
    box(title="Pollutant relationships",width=6,status="primary",plotlyOutput("correlation",height=320)))),
  tabItem(tabName="stations",
   fluidRow(box(title="Station comparison",width=7,status="primary",plotlyOutput("stationplot",height=360)),
    box(title="Station summary",width=5,status="primary",DTOutput("stationtable"))),
   fluidRow(box(title="Pollutant distribution by station",width=12,status="primary",plotlyOutput("boxplot",height=380)))),
  tabItem(tabName="predict",
   fluidRow(box(title="Prediction lab",width=12,status="primary",
    p("Train a Random Forest baseline to estimate PM2.5 from available pollutant and weather features. This is an exploratory model, not a regulatory forecast."),
    sliderInput("horizon","Rows to forecast from recent pattern",min=6,max=72,value=24,step=6),
    actionButton("train","Train / refresh model",icon=icon("play"),class="btn-primary"),
    br(),br(),uiOutput("modelnote"))),
   fluidRow(valueBoxOutput("modelrows",3),valueBoxOutput("rmse",3),valueBoxOutput("mae",3),valueBoxOutput("r2",3)),
   fluidRow(box(title="Observed vs predicted PM2.5 (holdout)",width=7,status="primary",plotlyOutput("predplot",height=350)),
    box(title="Feature importance",width=5,status="primary",plotlyOutput("importance",height=350))),
   fluidRow(box(title="Short-horizon scenario",width=12,status="primary",plotlyOutput("forecast",height=320)))),
  tabItem(tabName="data",fluidRow(box(title="Air quality observations",width=12,status="primary",
    p("Search, sort and export the filtered observations. Check units and data provenance before interpreting results."),DTOutput("table"))))
 )))
server <- function(input,output,session) {
 filtered <- reactive({
  d<-raw
  if(!is.null(input$station)&&input$station!="All stations") d<-d[d$Station==input$station,,drop=FALSE]
  if(!is.null(input$dates)&&length(input$dates)==2) d<-d[as.Date(d$Date)>=input$dates[1]&as.Date(d$Date)<=input$dates[2],,drop=FALSE]
  d
 })
 observeEvent(input$reset,{updateSelectInput(session,"station",selected="All stations");updateDateRangeInput(session,"dates",start=as.Date(min(raw$Date,na.rm=TRUE)),end=as.Date(max(raw$Date,na.rm=TRUE)));updateSelectInput(session,"pollutant",selected="PM.2.5")})
 plot_ts <- function(d,var) {
  d<-d%>%filter(!is.na(.data[[var]]))%>%arrange(Date)
  validate(need(nrow(d)>0,"No valid values for this pollutant in the selected filters."))
  p<-ggplot(d,aes(Date,.data[[var]],color=Station,text=paste0("Station: ",Station,"<br>Value: ",round(.data[[var]],2))))+geom_line(alpha=.65)+geom_smooth(aes(group=1),method="loess",se=FALSE,color="#E58E26",linewidth=.8)+labs(x=NULL,y=var,color="Station")+theme_minimal(base_size=12)
  ggplotly(p,tooltip="text")
 }
 output$records<-renderValueBox(valueBox(comma(nrow(filtered())),"Observations",icon=icon("database"),color="aqua"))
 output$pm25<-renderValueBox({d<-filtered();valueBox(if(all(is.na(d$PM.2.5)))"N/A" else paste0(round(mean(d$PM.2.5,na.rm=TRUE),1)," µg/m³"),"Mean PM2.5",icon=icon("wind"),color="green")})
 output$pm10<-renderValueBox({d<-filtered();valueBox(if(all(is.na(d$PM10)))"N/A" else paste0(round(mean(d$PM10,na.rm=TRUE),1)," µg/m³"),"Mean PM10",icon=icon("cloud"),color="purple")})
 output$stations<-renderValueBox({valueBox(n_distinct(filtered()$Station),"Monitoring stations",icon=icon("map-marker-alt"),color="teal")})
 output$trend<-renderPlotly({plot_ts(filtered(),input$pollutant)})
 output$timeseries<-renderPlotly({plot_ts(filtered(),input$pollutant)})
 output$bands<-renderPlotly({d<-filtered()%>%mutate(Band=aqi_band(PM.2.5))%>%count(Band);ggplotly(ggplot(d,aes(Band,n,fill=Band))+geom_col(show.legend=FALSE)+coord_flip()+labs(x=NULL,y="Observations")+theme_minimal())})
 output$pollutants<-renderPlotly({d<-filtered()%>%summarise(across(c(PM.2.5,PM10,NO2,SO2,CO,O3),~mean(.x,na.rm=TRUE)))%>%tidyr::pivot_longer(everything(),names_to="Pollutant",values_to="Mean");ggplotly(ggplot(d,aes(reorder(Pollutant,Mean),Mean,fill=Pollutant))+geom_col(show.legend=FALSE)+coord_flip()+labs(x=NULL,y="Mean concentration (source units)")+theme_minimal())})
 output$weather<-renderPlotly({d<-filtered()%>%filter(!is.na(Temperature),!is.na(PM.2.5));ggplotly(ggplot(d,aes(Temperature,PM.2.5,color=Humidity,text=paste0("Humidity: ",Humidity,"%")))+geom_point(alpha=.65)+geom_smooth(method="loess",se=FALSE,color="#E58E26")+labs(x="Temperature",y="PM2.5",color="Humidity")+theme_minimal(),tooltip=c("x","y","text"))})
 output$daily<-renderPlotly({d<-filtered()%>%mutate(Hour=lubridate::hour(Date))%>%group_by(Hour)%>%summarise(Mean=mean(.data[[input$pollutant]],na.rm=TRUE),.groups="drop");ggplotly(ggplot(d,aes(Hour,Mean))+geom_line(color="#176B87",linewidth=1)+geom_point(color="#39A9A8")+labs(x="Hour of day",y=input$pollutant)+theme_minimal())})
 output$correlation<-renderPlotly({d<-filtered()%>%select(any_of(c("PM.2.5","PM10","NO2","SO2","CO","O3","Temperature","Humidity")));d<-d[,vapply(d,is.numeric,logical(1)),drop=FALSE];validate(need(ncol(d)>=2,"Not enough numeric variables."));cm<-cor(d,use="pairwise.complete.obs");plot_ly(x=colnames(cm),y=rownames(cm),z=cm,type="heatmap",colors=c("#eaf2f8","#176b87"))%>%layout(xaxis=list(title=""),yaxis=list(title=""))})
 output$stationplot<-renderPlotly({d<-filtered()%>%group_by(Station)%>%summarise(Mean=mean(PM.2.5,na.rm=TRUE),N=n(),.groups="drop");ggplotly(ggplot(d,aes(reorder(Station,Mean),Mean,fill=Station))+geom_col(show.legend=FALSE)+coord_flip()+labs(x=NULL,y="Mean PM2.5")+theme_minimal())})
 output$stationtable<-renderDT({d<-filtered()%>%group_by(Station)%>%summarise(Observations=n(),PM25=mean(PM.2.5,na.rm=TRUE),PM10=mean(PM10,na.rm=TRUE),NO2=mean(NO2,na.rm=TRUE),.groups="drop");datatable(d,rownames=FALSE,options=list(dom="tip",pageLength=8))%>%formatRound(c("PM25","PM10","NO2"),2)})
 output$boxplot<-renderPlotly({d<-filtered()%>%filter(!is.na(.data[[input$pollutant]]));ggplotly(ggplot(d,aes(Station,.data[[input$pollutant]],fill=Station))+geom_boxplot(show.legend=FALSE,outlier.alpha=.25)+coord_flip()+labs(x=NULL,y=input$pollutant)+theme_minimal())})
 model <- eventReactive(input$train,{
  d<-filtered()%>%select(PM.2.5,PM10,NO2,SO2,CO,O3,Temperature,Humidity)%>%filter(!is.na(PM.2.5))
  d<-d%>%filter(if_all(everything(),~is.finite(.x)))
  validate(need(nrow(d)>=40,"At least 40 complete observations are needed to train the model."))
  # Holdout is time-ordered to reduce leakage from random shuffling.
  split<-max(20,floor(nrow(d)*.8));split<-min(split,nrow(d)-10)
  train<-d[seq_len(split),];test<-d[(split+1):nrow(d),]
  predictors<-setdiff(names(d),"PM.2.5")
  usable<-predictors[vapply(train[predictors],function(z)sd(z)>0,logical(1))]
  validate(need(length(usable)>=1,"Need at least one varying predictor in the selected data."))
  fit<-randomForest::randomForest(x=train[,usable,drop=FALSE],y=train$PM.2.5,ntree=250,importance=TRUE)
  pred<-predict(fit,test[,usable,drop=FALSE]);actual<-test$PM.2.5
  rmse<-sqrt(mean((actual-pred)^2));mae<-mean(abs(actual-pred));r2<-if(sd(actual)>0)cor(actual,pred)^2 else NA_real_
  # Baseline scenario forecast repeats recent observed weather/pollutant inputs, not a real atmospheric forecast.
  recent<-tail(d[,usable,drop=FALSE],input$horizon)
  if(nrow(recent)<input$horizon) recent<-recent[rep(seq_len(nrow(recent)),length.out=input$horizon),,drop=FALSE]
  forecast<-predict(fit,recent)
  list(fit=fit,actual=actual,pred=pred,rmse=rmse,mae=mae,r2=r2,n=nrow(d),importance=as.data.frame(randomForest::importance(fit)),forecast=data.frame(Step=seq_along(forecast),PredictedPM25=as.numeric(forecast)))
 },ignoreInit=TRUE)
 output$modelnote<-renderUI({if(input$train==0) return(tags$p("Click Train / refresh model to fit a baseline model on the selected observations."));m<-model();tags$p(paste("Trained on",m$n,"complete observations; metrics use a chronological holdout."))})
 output$modelrows<-renderValueBox({if(input$train==0)return(valueBox("—","Model status",icon=icon("brain"),color="aqua"));valueBox(comma(model()$n),"Model observations",icon=icon("database"),color="aqua")})
 output$rmse<-renderValueBox({if(input$train==0)return(valueBox("—","Holdout RMSE",icon=icon("calculator"),color="green"));valueBox(round(model()$rmse,2),"Holdout RMSE",icon=icon("calculator"),color="green")})
 output$mae<-renderValueBox({if(input$train==0)return(valueBox("—","Holdout MAE",icon=icon("ruler"),color="purple"));valueBox(round(model()$mae,2),"Holdout MAE",icon=icon("ruler"),color="purple")})
 output$r2<-renderValueBox({if(input$train==0)return(valueBox("—","Holdout R² proxy",icon=icon("chart-line"),color="teal"));valueBox(ifelse(is.na(model()$r2),"N/A",round(model()$r2,3)),"Holdout correlation²",icon=icon("chart-line"),color="teal")})
 output$predplot<-renderPlotly({validate(need(input$train>0,"Train the model to see holdout predictions."));m<-model();p<-data.frame(Observed=m$actual,Predicted=m$pred);ggplotly(ggplot(p,aes(Observed,Predicted))+geom_point(alpha=.65,color="#176B87")+geom_abline(slope=1,intercept=0,linetype="dashed",color="#E58E26")+labs(x="Observed PM2.5",y="Predicted PM2.5")+theme_minimal())})
 output$importance<-renderPlotly({validate(need(input$train>0,"Train the model to see feature importance."));m<-model();imp<-as.data.frame(m$importance);imp$Feature<-rownames(imp);metric<-if("%IncMSE"%in%names(imp))"%IncMSE" else names(imp)[1];ggplotly(ggplot(imp,aes(reorder(Feature,.data[[metric]]),.data[[metric]]))+geom_col(fill="#39A9A8")+coord_flip()+labs(x=NULL,y=metric)+theme_minimal())})
 output$forecast<-renderPlotly({validate(need(input$train>0,"Train the model to see the scenario projection."));m<-model();ggplotly(ggplot(m$forecast,aes(Step,PredictedPM25))+geom_line(color="#176B87",linewidth=1)+geom_point(color="#39A9A8")+labs(x="Future step (row)",y="Scenario PM2.5 estimate")+theme_minimal())})
 output$table<-renderDT({datatable(filtered(),rownames=FALSE,filter="top",extensions="Buttons",options=list(dom="Bfrtip",buttons=c("copy","csv","excel"),pageLength=15,scrollX=TRUE))})
}
shinyApp(ui,server)
