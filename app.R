library(shiny)
library(FactoMineR)
library(Factoshiny)
library(ggplot2)

#click on the "Quit the app" button
shinyApp(
  
  ui = shinyUI(htmlTemplate("template.html")),
  
  server = shinyServer(function(input, output) {
    
          
          
  ############################
         
  
  #######################################

              # Input: Select a file ----
              output$file1 <- renderUI(fileInput("file1", "Choose your CSV File",
                                                 accept = c(
                                                   "text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
              ))
              
             
              # Input: Checkbox if file has header ----
              output$head <- renderUI(checkboxInput("header", "Header", TRUE))
              
              # Input: Select separator ----
              output$sep<- renderUI(radioButtons("sep", "Separateur",
                           choices = c(Virgule = ",",
                                       Point_virgule = ";",
                                       Tabulation = "\t"),
                           selected = ","))
              
              # Input: Select quotes ----
              output$quote <- renderUI(radioButtons("quote", "Quote",
                           choices = c(Rien = "",
                                       "Double Quote" = '"',
                                       "Unique Quote" = "'"),
                           selected = '"'))
              
             
              # Input: Select number of rows to display ----
              output$dis <- renderUI(radioButtons("disp", "Display",
                           choices = c(En_tête = "head",
                                       Tout_le_fichier = "all"),
                           selected = "head"))
              
            
            
            # Main panel for displaying outputs ----
            
          
          output$table <- renderDataTable({
            
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            
            #req(input$file1)
            
            df <- getdata()
            if(is.null(df)){
              
            }
            
            if(input$disp == "head") {
              return(head(df))
            }
            else {
              return(df)
            }
            
          })
          
          
          
  ##########################################
          
          
          getdata <- reactive({
            infile <- input$file1
            if (is.null(infile)){
              decathlon[,1:10]
            }else{
              read.csv(infile$datapath, header = input$header,
                       sep = input$sep,
                       quote = input$quote,stringsAsFactors=F)
            }
           
          })
          
          #getdata = reactive({
              
          #    if(is.null(reactive(input$file1$datapath))){
          #      data1 <- data.frame(read.csv('www/california_housing_train.csv'))
                
          #    }else{
                
          #      data1 <- getfile()
                                          
          #    }
          #  return(data1[1:20,])
          #})
          
          #data1 <- datasets::attitude
          #data1 <- data.frame(c)   
           data2 <- data.frame(read.csv('www/california_housing_train.csv'))
           #res.PCA1 <- PCA(data2, graph = FALSE)
           #data1 <- reactive(getdata())
           #res.PCA<-PCA(decathlon,quali.sup=c(13),quanti.sup=c(11,12),graph=FALSE)
           #res.PCA<-reactive(resPCA(data1(),graph=FALSE))
          
           #output$table <- renderDataTable(data1[1:50,] , options = list(paging = TRUE , searching = TRUE))
           
           output$title <- renderText('Shiny Custom HTML Template')
           
           output$summary <- renderPrint({
             data1 <- reactive(getdata())
             if (is.null(data1)){
               summary(PCA(data2,graph= FALSE))
             } 
             else{

               summary(PCA(data1(), graph = FALSE))
             }
           })
           
           #output$y <- renderUI(selectInput('var1',inputId = "Variable abscisse",
           #            label = "x:",
           #            choices = c(names(data1))))
          # output$x <- renderUI(selectInput('var2' ,inputId = "Variable abscisse",
          #                                      label = "x:",
          #                                      choices = c(names(data1))))
           
           #output$plot1 <- renderPlot(plot(data1[,c(2,3)]))
           res.PCA<-PCA(decathlon,quali.sup=c(13),quanti.sup=c(11,12),graph=FALSE)
           
           output$plot1 <- renderPlot(plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP",col.quanti.sup='#0000FF'))
           
           #output$plot2 <- renderPlot(plot(data1[,c(3,4)]))
           
           output$plot2 <- renderPlot(plot.PCA(res.pca,title="Graphe des individus de l'ACP"))
           
           output$desc <- renderPrint(dimdesc(res.pca)) 
           
           output$plot3 <- renderPlot(barplot(res.pca$eig[,1], main="Valeurs propres",names.arg=paste("dim",1:nrow(res.pca$eig))))
           output$plot4 <- renderPlot(
                                        plot(res.pca$eig[,1], main="Critère du coude",type="b", axes=T,xlab="",yla="valeurs propres")
                                      )
           #output$btn <- renderUI(actionButton("go", "Tracez la courbe" ,class = "btn btn-primary"))
           
           output$menu1 <- renderUI(tabsetPanel(
             tabPanel("résumé de variables",
                      h4("Description"),verbatimTextOutput('summary')
             ),
             tabPanel("Valeurs Propres", verbatimTextOutput('chemin'),
                      h4("Représentation graphique (Histogramme)"),plotOutput('plot3'), plotOutput('plot4')),
             tabPanel("Descritption complète des variables", verbatimTextOutput('desc'))
        
           ))
           
           output$regression <- renderPrint(input$btn)
           
           # Droite de regression
           #output$regression<-renderPrint(lm(data1$input$var1 ~ data1$input$var2))
           
           # Coeffient de la droite de regresssion a et b
           
           
           #vars <- (
             #data1 <- reactive(getdata())
             #if (is.null(reactive(getdata()))){
              # reactive(setdiff(names(data2), "TSEST"))
             #} 
             #else{
            # reactive(setdiff(names(reactive(getdata())), "TSEST"))
             #}
          # )
               
            vars<- setdiff(names(data2), "TSEST")
          
           output$y <- renderUI({
             
             if (is.null(reactive(getdata()))){
              selectInput('ycol', 'Y Variable', setdiff(names(data2), "TSEST"))
             } 
             else{
               selectInput('ycol', 'Y Variable', setdiff(names(getdata()), "TSEST"))
             }
           })
           
           #selectInput('xcol', 'X Variable', vars))
           output$x <- renderUI({
             
             if (is.null(reactive(getdata()))){
               textOutput('xcol' ,'Importer des données por ls visualiser')
             } 
             else{
               selectInput('xcol', 'X Variable', setdiff(names(getdata()), "TSEST"))
             }
           })
           output$z <- renderUI(numericInput('clusters', 'Cluster count', 3, min = 1, max = 9))
           #output$y <- renderUI(dataTableOutput(reactive(getdata())))
           
           #server
           selectedData <- reactive({
             
             
             {
               
               if (is.null(reactive(getdata()))){
                 data2[, c(input$xcol, input$ycol)]
               } 
               else{
                 getdata()[, c(input$xcol, input$ycol)]
               }
             }
             
           })
           
           clusters <- reactive({
             kmeans(selectedData(), input$clusters)
           })
           # coeeficeints de la droit de regression
           
           output$regression <-renderPrint(lm(selectedData()))
           model <- reactive(lm(selectedData()))
           output$plot5 <- renderPlot({
             #☺palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
             
             par(mar = c(5.1, 4.1, 0, 1))
             plot(selectedData(),
                  col = clusters()$cluster,
                  pch = 20, cex = 3)
             points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
             abline(lm(selectedData()),col='red')
           })
           
           
           
           
           
          
           
           
  })
  
)
