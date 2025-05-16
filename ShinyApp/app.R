
 library(shiny) 
 library(ggplot2) 
 library(shinythemes) 
 library(ggthemes) 
 library(DT)

 # Load list of models 
 load(file="models.RData") 
 lv.malli.value<-as.character(seq(tmp.mallit)) 
 names(lv.malli.value)<-sapply(tmp.mallit,function(x){as.character(formula(x))[3]}) 
 tmp.dt.AIC<-data.frame(Model=sapply(tmp.mallit,function(x){as.character(formula(x))[3]}),
                                      AIC=sapply(tmp.mallit,function(x)AIC(x))) 
 tmp.dt.AIC<-tmp.dt.AIC[order(tmp.dt.AIC$AIC),] 
 tmp.dt.AIC$Relative.likelihood<-exp((tmp.dt.AIC$AIC[1]-tmp.dt.AIC$AIC)/2) 
 # Time variable name 
 lv.i.coxph<-sum(class(tmp.mallit[[1]])%in%'coxph') 
 lv.time<-gsub(x=strsplit(x=strsplit(x=as.character(formula(tmp.mallit[[1]]))[2],split = "\\(")[[1]][2],
                          split=",")[[1]][1],pattern = "\\)",replacement = "") 
 # X-variable names 
 tmp.otsikko<-names(tmp.mallit) 
 if(is.null(tmp.otsikko)){Models.otsikko<-paste0("Model ",seq(length(tmp.mallit)))} 
 if(!is.null(tmp.otsikko)){Models.otsikko<-ifelse(tmp.otsikko=="",paste0("Model ",seq(length(tmp.mallit))),tmp.otsikko)} 
 # Osa 1 loppu 
lv.xvars<-c("SEX", "Age_Group", "Obesity", "PhysActiv", "KnowFeeding")
# Osa 1A alku 
 # Data frame containing predictions 
 tee.dt<-function(input){ 
     lv.dt<-expand.grid( 
 # Osa 1A loppu 
SEX=factor(input$SEX,levels=c("Male","Female")),
Age_Group=factor(input$Age_Group,levels=c("<1yo","1-2yo","3-4yo")),
Obesity=factor(input$Obesity,levels=c("No","Yes")),
PhysActiv=factor(input$PhysActiv,levels=c("No","Yes")),
KnowFeeding=factor(input$KnowFeeding,levels=c("Low","Intermediate","High"))# Osa 2 alku 
     ) 
     lv.class<-class(tmp.mallit[[as.numeric(input$Model)]]) 
     if(sum(lv.class%in%"coxph")>0){ 
         lv.pred<-predict(tmp.mallit[[as.numeric(input$Model)]],newdata=lv.dt,se.fit = TRUE,type="survival") 
     } 
     if(sum(lv.class%in%c("lm","glm"))>0){lv.pred<-predict(tmp.mallit[[as.numeric(input$Model)]],newdata=lv.dt,se.fit = TRUE,type="response")} 
     lv.dt$Predicted<-lv.pred$fit 
     lv.dt$se.fit<-lv.pred$se.fit 
     lv.dt$Predicted.lo<-lv.pred$fit-1.96*lv.pred$se.fit 
     lv.dt$Predicted.hi<-lv.pred$fit+1.96*lv.pred$se.fit 
     if(sum(lv.class%in%"coxph")>0){ 
          lv.dt$Predicted.lo<-ifelse(lv.dt$Predicted.lo<0,0,lv.dt$Predicted.lo) 
      } 
     lv.dt 
 } 
 # Define UI for application that draws a histogram 
 ui <- fluidPage(theme = shinytheme( 
 "readable"), 
     # Application title 
     titlePanel( 
 "Neurodevelopmental disorder in Children"), 
     # Sidebar with a slider input 
     sidebarLayout( 
         sidebarPanel( 
             #textAreaInput("AddPlot",label="Add plot script",value="",rows=3), 
             #actionButton("Toteuta", label="Submit"), 
             radioButtons("Model",label="Select model", 
                              choices=lv.malli.value,inline=FALSE), 
             # Osa 2 loppu 
radioButtons(inputId="SEX", label="Sex",
 choices=c("Male"="Male","Female"="Female"), selected = "Male"),
radioButtons(inputId="Age_Group", label="Age (groups)",
 choices=c("<1yo"="<1yo","1-2yo"="1-2yo","3-4yo"="3-4yo"), selected = "<1yo"),
radioButtons(inputId="Obesity", label="Obesity",
 choices=c("No"="No","Yes"="Yes"), selected = "No"),
radioButtons(inputId="PhysActiv", label="Physical Activity",
 choices=c("No"="No","Yes"="Yes"), selected = "No"),
radioButtons(inputId="KnowFeeding", label="Knowledge on Feeding",
 choices=c("Low"="Low","Intermediate"="Intermediate","High"="High"), selected = "Low"),            # Osa 3 alku 
         ), 
         mainPanel( 
             tabsetPanel(id='tabs', 
                 tabPanel("Plot",plotOutput("distPlot")), 
                 tabPanel("Data", dataTableOutput("Data")), 
                tabPanel("Summary", verbatimTextOutput("Summary")), 
                 tabPanel("AIC", htmlOutput("AICTab")), 
                 tabPanel("Info",htmlOutput("Info")) 
         ) 
     ) 
 )) 
 # Define server logic required to draw a histogram 
 server <- function(input, output, session) { 
  
     output$distPlot <- renderPlot({ 
         tmp.dt<-tee.dt(input) 
         #browser()
         colore<-hsv(runif(1), 1, 1)
          lv.txt<-paste("lv.p1<-ggplot(tmp.dt,aes(x=1,y=Predicted))+geom_line(color=colore)+geom_point(color=colore)+
                        geom_errorbar(aes(ymin = tmp.dt$Predicted.lo, ymax = tmp.dt$Predicted.hi),width = 0.1,color=colore)+
                        geom_ribbon(aes(ymin = tmp.dt$Predicted.lo, ymax = tmp.dt$Predicted.hi), alpha=.2,color=colore)+
                        coord_flip()+theme_bw()+
                        geom_text(aes(x=1.2,y=Predicted),label=paste0(round(tmp.dt$Predicted,2),
                        ' (',round(tmp.dt$Predicted.lo,2),'-',
                        round(tmp.dt$Predicted.hi,2),')'),size=3.5)+
                        scale_x_continuous(name='',limits=c(0.5,1.5),breaks=c(0,2))+
                        scale_y_continuous(name='Prediction',limits=c(-0.2,1.2),breaks=c(0,0.25,0.5,0.75,1))") 
         
          eval(parse(text=lv.txt)) 
          
         input$Toteuta 

         lv.p1 
     },width=800,height=500,res=300) 
     output$Data<-DT::renderDataTable({ 
         tmp.dt<-tee.dt(input) 
     tmp.title<-'Data and predictions' 
 DT::datatable(tmp.dt,
                           extensions = 'Buttons',escape=TRUE,
                           options = list(
                             pageLength = 50,
                             dom = 'Blfrtip',
                             buttons = list(
                               list(extend = 'copy', title = tmp.title),
                               list(extend = 'csv', title = tmp.title),
                               list(extend = 'excel', title = tmp.title),
                               list(extend = 'pdf', title = tmp.title)
                             )
                           )) 
     }) 
     output$AICTab<- renderPrint({ 
       knitr::kable(tmp.dt.AIC,format='html',caption='Compring models with AIC') 
      }) 
     output$Summary<- renderPrint({ 
      if(sum(class(tmp.mallit[[as.numeric(input$Model)]])%in%"coxph")>0)lv.testi<-cox.zph(tmp.mallit[[as.numeric(input$Model)]]) 
     else{lv.testi<-NULL} 
         tmp.ulos.S<-list(summary(tmp.mallit[[as.numeric(input$Model)]]),lv.testi) 
          if(is.null(lv.testi)) {names(tmp.ulos.S)<-c('Model','')} 
          if(!is.null(lv.testi)) {names(tmp.ulos.S)<-c('Model','Testing PH assumption')} 
         cat("\n Current model:" ,Models.otsikko[as.numeric(input$Model)],"\n\n") 
         tmp.ulos.S 
     }) 
     output$Info<-renderText({ 
          HTML( scan(quiet=TRUE,file="ModelInfo.html",what=""))}) 
 } 
 # Run the application 
 shinyApp(ui = ui, server = server) 
