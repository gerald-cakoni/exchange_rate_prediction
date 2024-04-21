#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## First specify the packages of interest
packages = c("readr","shiny","shinyWidgets","fpp3","randomForest","corrplot",
             "tidyverse","ggplot2","lubridate","forecast","correlation","dplyr", "DT","graphics",
             "shiny","MTS","shinythemes","tibble","vip","Metrics", "h2o", "data.table", "plotly")

## Now load or install&load all libraries neccessary to work
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

urlfile="https://raw.githubusercontent.com/gerald-cakoni/exchange_rate_prediction/main/KembimiValutor.csv"

te_dhena<-read_csv(url(urlfile))
View(te_dhena)# te_dhena <- read_csv("C:/Users/geral/Desktop/shiny/KembimiValutor.csv")
#View(te_dhena)
kembimi_valutor<-ts(te_dhena$kembimi_valutor,start =c(2016,1),frequency = 12)
norma_huave_euro<-ts(te_dhena$norma_huave_euro,start =c(2016,1),frequency = 12)
depozita_euro<-ts(te_dhena$depozita_euro,start =c(2016,1),frequency = 12)
ick<-ts(te_dhena$ick,start =c(2016,1),frequency = 12) 
norma_depozitave<-ts(te_dhena$norma_depozitave,start =c(2016,1),frequency = 12) 
depozita_leke<-ts(te_dhena$depozita_lek,start =c(2016,1),frequency = 12) 
shpenzimet_qeverise<-ts(te_dhena$shpenzimet_qeverise,start =c(2016,1),frequency = 12)  
ivt<-ts(te_dhena$ivt,start =c(2016,1),frequency = 12) 
hyrje<-ts(te_dhena$hyrje,start =c(2016,1),frequency = 12) 
dalje<-ts(te_dhena$dalje,start =c(2016,1),frequency = 12)  
importe<-ts(te_dhena$importe,start =c(2016,1),frequency = 12) 
eksporte<-ts(te_dhena$eksporte,start =c(2016,1),frequency = 12) 
diferenca_hyrje_dalje<-ts(te_dhena$diferenca_hyrje_dalje,start =c(2016,1),frequency = 12) #Diferenca hyrje dalje te udhetareve


train.kembimi<-head(kembimi_valutor,48) # 80% e vezhgimeve
test.kembimi<-tail(kembimi_valutor,12)   # 20% e vezhgimeve

logtrain.kembimi <- log(train.kembimi)
logtest.kembimi <- log(test.kembimi)



##Modeli ARIMA 
Modeli_ARIMA <- arima(logtrain.kembimi, order=c(0,0,1))

##Parashikoj 1 vit 

y_ARIMA <- predict(Modeli_ARIMA, n.ahead = 1*12)

y_ARIMA1 <- 2.718^y_ARIMA$pred

##Modeli SARIMA
Modeli_Sarima <- arima((logtrain.kembimi), c(0,3,0), seasonal = list(order= c(0,3,0), period = 12))

#parashikimi per 1 vit
y_Sarima <- predict(Modeli_Sarima, n.ahead = 1*12)

y_SARIMA1 <- 2.718^y_Sarima$pred



h2o.init(max_mem_size = "16G")
h2o.no_progress()

train_1<-head(te_dhena,48)
test_1<-tail(te_dhena,12)

nn.f = nnetar(ts(train_1$kembimi_valutor,start=2016,frequency = 12), xreg = ts(train_1$depozita_euro,start=2016,frequency = 12))
nn.f.f <- forecast(nn.f, xreg = ts(test_1$depozita_euro,start=c(2020,1),frequency=12), PI = F)

nn.f1 = nnetar(ts(train_1$kembimi_valutor,start=2016,frequency = 12), xreg = ts(train_1$ick,start=2016,frequency = 12))
nn.f1.f <- forecast(nn.f1, xreg = ts(test_1$ick,start=c(2020,1),frequency=12), PI = F)

# Define UI for application that draws a histogram

ui <- navbarPage(title =div(tags$a(img(src="kembimik.jpeg", height = 40, width = 100), href= "https://www.bankofalbania.org/Tregjet/Kursi_zyrtar_i_kembimit/"),
                            style = "position: relative; top: -5px;"), theme = shinytheme("cerulean"),
                 
                 
                 tabPanel(title = "Hyrje",
                          mainPanel(width=6,
                                    h1("Permbledhje", style = "color:black;"),
                                    p("Qellimi i ketij projekti eshte parashikimi i kursit te kembimit te lekut kundrejt euros. 
                                    Kembimi Valutor eshte nje koncept shume i rendesishem ne ekonomi pasi ne varesi te tij meren shume 
                                    vendime per investime strategjike, import dhe eksport mallrash. Ne vendin tone euro ka patur shume 
                                    luhatja si rezultat i ndryshimeve drastike qe kane ndodhur dhe kjo e ben ndoshta me te veshtire parashikimin, 
                                    por padyshim me te nevojshem. Disa arsye kryesore qe kane ndikuar kursin e kembimit jane: Rritja e numrit 
                                    te madh te udhetareve,rritja e import-eksportit me vendet ku perdoret euro, parate e pista dhe pastrimi i tyre etj. 
                                    Duke perdorur modelet ARIMA,SARIMA dhe modele te ML per te parashikuar serine kohore me sa me pak gabime 
                                    dhe me model sa me te pershtatshem. 
                                    Do zgjedhim ate model gabim sa me te vogel qe parashikon me sakte.",style = "font-size: 22px;")
                          )),
                 
                 
                 
                 
                 
                 navbarMenu(title = "Analiza e te Dhenave",
                            tabPanel(title = "Seri Kohore",  
                                     
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                                    
                                                    selectInput("variabli", "Variabli:",
                                                                
                                                                list("Kembimi Valutor" = "kembimi_valutor",
                                                                     "Norma Huave ne Euro"="norma_huave_euro",
                                                                     "Depozita Euro"="depozita_euro",
                                                                     "Indeksi i Cmimeve te Konsumit"="ick",
                                                                     "Norma e Depozitave"="norma_depozitave",
                                                                     "Depozita ne Leke"="depozita_leke",
                                                                     "Shpenzimet e Qeverise"="shpenzimet_qeverise",
                                                                     "Indeksi i Vleres ne Tregti"="ivt",
                                                                     "Hyrjet e Udhetareve"="hyrje",
                                                                     "Daljet e Udhetareve"="dalje",
                                                                     "Importe"="importe",
                                                                     "Eksporte"="eksporte"
                                                                     
                                                                )))
                                       
                                       ,
                                       mainPanel(width=6,
                                                 tabsetPanel(type = "tabs",
                                                             tabPanel("Seria", plotOutput("Seplot")),
                                                             tabPanel("Seasonal Plot", plotOutput("Splot")),
                                                             tabPanel("Autokorrelacionet", plotOutput("Aplot")),
                                                             tabPanel("PACF", plotOutput("Pacf")),
                                                             tabPanel("Plotly", plotlyOutput("Pplotly"))
                                                 ))))
                            
                            
                            ,
                            tabPanel(title = "Mesatarja e Levizshme",
                                     sidebarLayout(
                                       sidebarPanel(width=3, 
                                                    sliderInput("n", "Caktimi i koeficientit:",min = 1, max = 18, value = 12, step = 1))
                                       
                                       
                                       
                                       ,
                                       mainPanel(width=6,
                                                 
                                                 tabPanel("Mesatarja e Levizshme",plotOutput("mav"))
                                       )))
                 ),
                 navbarMenu(title = "Modelet dhe Parashikimi",
                            tabPanel(title = "Modeli SARIMA",
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                                    numericInput("ahead", "Numri i muajve per te parashikuar:", 24)
                                                    
                                       )
                                       ,
                                       mainPanel(width=6,
                                                 tabPanel("Modeli",plotOutput("SarimaForecastPlot"),verbatimTextOutput("s"))
                                                 
                                       ))
                                     
                            ),
                            tabPanel(title = "Modeli ARIMA",
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                                    numericInput("ahead", "Numri i muajve per te parashikuar:", 24)
                                       )
                                       ,
                                       mainPanel(width=6,
                                                 tabPanel("Modeli",plotOutput("arima"),verbatimTextOutput("xxx"))
                                                 
                                       ))),
                            tabPanel(title = "Modeli ML",   mainPanel(width=6,
                                                                      tabPanel(title ="Modeli", plotOutput("plot"),textOutput("value6"))
                                                                      
                            )),
                            tabPanel("Parashikimi NNAR ne lidhje me temperaturen mesatare",plotOutput("NN1ForecastPlot"),verbatimTextOutput("summ1")),
                            tabPanel("Miresia e modeleve", tableOutput("view1"))
                            
                            
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel(title = "Perfundime",
                          mainPanel(width=6,
                                    h4("Permbledhje", style = "color:blue;font-weight: bold; font-size: 28px;"),
                                    p(""),
                                    p("Ky studim tregon ecurine e kembimit valutor nder vite dhe premisat per ecurine e tij. 
                                      Ecuria paraqitet me trend te lehte renes per shkak te rritjes gjithnje e me teper 
                                      te investimeve ne euro dhe rritjes se vazhdueshme te fluksit te udhetareve ne Shqiperi, 
                                      kjo sjell me shume euro ne treg duke ulur kembimin valutor. Modeli me i mire rezultoi SARIMA 
                                      si edhe modelet e rjtave nervore. Duke mare ne konsiderate edhe variablat e tjere nuk parashikohet 
                                      nje ndyshim i madh ne trendin e kembimit valutor ne vitet e ardhshme.",style = "font-size: 18px;"),
                                    h4("Mangesite",style = "color:red;font-weight: bold; font-size: 28px;"),
                                    p(""),
                                    p("Mangesia krysore e projektit eshte mungesa e te dhenave te plota. 
                                      Do te ishte me mire po te ishin marre me shume te dhena ne shqyrtim. 
                                      Nje tjeter problem shume i rendesishem ne gjykimin tim eshte edhe mungesa totale 
                                      per arsye objektive e te dhenave per variabla qe mund te ndikonin tek kembimi valutor. 
                                      Si nje nder te dhenat kryesore mund te permen parate informale ne euro qe futen ne vendin tone. 
                                      Psh. Ne vitin 2016 kembimi valutor pesoi nje ulje te ndjeshme per shkak te parase informale ne tregun e kanabisit. 
                                      Shqyrtimi i te gjithe ketyre variablave do nxirrte rezultate edhe me te sakta.",style = "font-size: 18px;"))
                          
                          
                 ), 
                 tabPanel(title = "Te Dhena", 
                          
                          sidebarLayout(
                            sidebarPanel(width=20,
                                         conditionalPanel(
                                           'input.dataset === "te_dhena"',
                                           checkboxGroupInput("show_vars", "Zgjidhni Variablin:",
                                                              names(te_dhena), selected = names(te_dhena))
                                         )
                            ),
                            mainPanel(
                              tabsetPanel(
                                id = 'dataset',
                                tabPanel("te_dhena", DT::dataTableOutput("mytable1"))
                                
                              )))),
                 
                 
                 
                 tabPanel(title = "Referenca",
                          
                          mainPanel(
                            h3("Databazat jane mare nga linqet e meposhtem", style = "color:purple;"),
                            p(""),
                            tags$ul( 
                              
                              tags$li(class= "li-custom", tags$a(href="http://www.instat.gov.al/", 
                                                                 "INSTAT",  class="externallink"),style = "font-size: 18px;"),
                              p(""),
                              
                              tags$li(class= "li-custom", tags$a(href="https://www.bankofalbania.org/",
                                                                 "Banka e Shqiperise",  class="externallink"),style = "font-size: 18px;"),
                              p(""),
                              
                              tags$li(class= "li-custom", tags$a(href="https://ec.europa.eu/eurostat", 
                                                                 "EUROSTAT",  class="externallink"),style = "font-size: 18px;")
                            ),
                            p(""),
                            
                            h3("LEKSIONE", style = "color:purple;"),
                            p("- Leksione te shkruara: Autor - Llukan Puka",style = "font-size: 18px;"),
                            p("- Leksione te shkruara: Autor - Eralda Gjika (Dhamo)",style = "font-size: 18px;"),
                            
                            h3("LIBRA", style = "color:purple;"),
                            
                            tags$ul( 
                              
                              tags$li(class= "li-custom", tags$a(href="https://otexts.com/fpp3/",  "FPP3",  
                                                                 class="externallink"),"Forecasting: Principles and Practice (2nd ed), Rob J Hyndman and George Athanasopoulos",style = "font-size: 18px;"),
                              p(""),
                              tags$li(class= "li-custom", tags$a(href="https://www.stat.pitt.edu/stoffer/tsa4/",  "TSA4",  
                                                                 class="externallink"),"Time Series Analysis and Its Applications, With R Examples - 4th Edition SHUMWAY & STOFFER'S",style = "font-size: 18px;")
                            ),
                            
                            h3("WEBSITE", style = "color:purple;"),
                            tags$ul( 
                              
                              tags$li(class= "li-custom", tags$a(href="https://rpubs.com/",  "R PUBS",  
                                                                 class="externallink"),style = "font-size: 18px;"),
                              p(""),
                              tags$li(class= "li-custom", tags$a(href="https://stackoverflow.com/",  "StackOverFlow",  
                                                                 class="externallink"),style = "font-size: 18px;")
                            )  
                          )
                 )
)




server <- function(input, output) {
  
  #TE DHENA
  te_dhenat1 = te_dhena[sample(nrow(te_dhena), 72), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(te_dhenat1[, input$show_vars, drop = FALSE])
  })
  
  
  #TE DHENA
  
  getDataset <- reactive({
    
    if (input$variabli=="kembimi_valutor"){
      return(kembimi_valutor)
    }
    else if (input$variabli=="norma_huave_euro") {
      return(norma_huave_euro)
    }
    
    else if (input$variabli=="depozita_euro") {
      return(depozita_euro)
    }
    
    else if (input$variabli=="ick") {
      return(ick)
    }
    
    else if (input$variabli=="norma_depozitave") {
      return(norma_depozitave)
    }
    
    else if (input$variabli=="depozita_leke") {
      return(depozita_leke)
    }
    
    
    else if (input$variabli=="shpenzimet_qeverise") {
      return(shpenzimet_qeverise)
    }
    
    
    else if (input$variabli=="ivt") {
      return(ivt)
    }
    
    
    else if (input$variabli=="hyrje") {
      return(hyrje)
    }
    
    
    else if (input$variabli=="dalje") {
      return(dalje)
    }
    
    else if (input$variabli=="importe") {
      return(importe)
    }
    
    else if (input$variabli=="eksporte") {
      return(eksporte)
    }
    
    
    
  })
  
  
  datasetInput <- reactive({
    switch(input$dataset,
           "te_dhena" = te_dhena
    )
  })
  
  
  #ANALIZA E TE DHENAVE
  
  output$Splot <- renderPlot({
    data <- getDataset()
    seasonplot(data,ylab = input$variabli, xlab = "Muaji",main = input$variabli,year.labels = TRUE, year.labels.left = TRUE, col = 2:6, pch=19)
    
  })
  
  output$Seplot <- renderPlot({
    data <- getDataset()
    plot(data,ylab = input$variabli, xlab = "Muaji",main = input$variabli,year.labels = TRUE, year.labels.left = TRUE, col = 2:6, pch=19)
    
  })
  
  output$Aplot <- renderPlot({
    data <- getDataset()
    acf(data,ylab = input$variabli, xlab = "Muaji",main = input$variabli,year.labels = TRUE, year.labels.left = TRUE, col = 2:6, pch=19)
    
  })
  
  output$mav <- renderPlot({
    data <- getDataset()
    autoplot(ma(data, order = input$n, centre = TRUE)) 
    #ggseasonplot, gglagplot
  })
  
  
  
  output$Pacf <- renderPlot({
    
    plot(pacf(getDataset(),lag.max=300,main = getDataset()))
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "te_dhena" = te_dhena
    )
  })
  
  output$Pplotly <- renderPlot({
    data <- getDataset()
    plot_ly(data())
    
  })
  
  
  #MODELET E PARASHIKIMIT
  
  output$SarimaForecastPlot <- renderPlot({
    fit <- arima(getDataset(), c(0,1,1), seasonal = list(order= c(1,1,1), period = 12))
    plot(forecast(fit,h=input$ahead))
    
  })
  
  
  output$s <- renderPrint(
    forecast(arima(getDataset(), c(0,1,1), seasonal = list(order= c(1,1,1), period = 12)),h=input$ahead)
    
  )
  ################################################################################
  output$FPlot <- renderPlot({
    
    autoplot(nnetforecast2,ylab="Konsumi")+autolayer(ts(test_1$Konsumi_energjise,start=c(2020,1),frequency = 12),series="Data")
    
  })
  
  output$summ2 <- renderPrint({
    summary(nnetforecast2) 
  })
  
  
  
  #################################################################################
  
  output$SMLPlot <- renderPlot({
    fit <- arima(getDataset(), c(0,1,1), seasonal = list(order= c(1,1,1), period = 12))
    plot(forecast(fit,h=input$ahead))
    
  })
  
  
  
  
  
  output$view1 <- renderTable({
    
    
    ARIMA=summary(Modeli_ARIMA)
    SARIMA=summary(Modeli_Sarima)
    ARIMA_1=mse(logtest.kembimi,y_ARIMA1)
    SARIMA_1=mse(logtest.kembimi,y_SARIMA1)
    
    Final = as.data.frame(t(c(ARIMA$aic,SARIMA$aic)))
    Final %>% rename(
      
      SARIMA_aic = V1,
      ARIMA_aic = V2
      
    )
    
  })
  
  output$plot <- renderPlot({
    
    E<-as.tibble(datasetInput())
    trajnimi<-head(datasetInput(),48)
    testimi<-tail(datasetInput(),12)
    
    y <- "kembimi_valutor"
    x <- setdiff(names(trajnimi %>% as.h2o()),c(y,"Koha"))
    #Modeli random forest
    rft_model <- 
      h2o.randomForest(
        x = x, 
        y = y, 
        training_frame = trajnimi %>% as.h2o(),
        nfolds = 10,
        ntrees = 500,
        stopping_metric = "RMSE",
        stopping_rounds = 10,
        stopping_tolerance = 0.005,
        seed = 1975
      )
    
    #Modeli gradient boosting machine
    gbm_model <-  
      h2o.gbm(
        x = x, 
        y = y, 
        training_frame = as.h2o(trajnimi),
        nfolds = 10,
        ntrees = 500,
        stopping_metric = "RMSE",
        stopping_rounds = 10,         
        stopping_tolerance = 0.005,
        seed = 1975
      )
    
    #Modeli linear i pergjithesuar (glm)
    glm_model <- 
      h2o.glm(
        x = x, 
        y = y, 
        training_frame = as.h2o(trajnimi),
        nfolds = 10,
        family = "gaussian",
        seed = 1975
      )
    p_glm <- vip(glm_model) + ggtitle("GLM")
    p_rft <- vip(rft_model) + ggtitle("RF")
    p_gbm <- vip(gbm_model) + ggtitle("GBM")
    grid.arrange(p_glm, p_rft, p_gbm, nrow = 2)
    
    
  })
  
  
  output$NN1ForecastPlot <- renderPlot({
    
    autoplot(nn.f1.f)+ylab("Sale")+autolayer(ts(test_1$kembimi_valutor,start=c(2020,1),frequency = 12),series="Data")
    
    
    
  })
  
  output$summ1 <- renderPrint({
    summary(nn.f1.f)
  })
  
  
  output$arima <- renderPlot({
    model <- auto.arima(getDataset())
    plot(forecast(model,h=input$ahead))
    
  }) 
  
  output$xxx <- renderPrint(
    forecast(auto.arima(getDataset()),h=input$ahead)
    
  )
  
}


shinyApp(ui, server)