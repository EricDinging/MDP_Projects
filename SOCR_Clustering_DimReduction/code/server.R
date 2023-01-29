library(ClusterR)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(kernlab)
library(plotly)
library(ppclust)
source("ui.R")

options(shiny.maxRequestSize=50*1024^2)  # Limit file import to 50 MB

server <- shinyServer(function(input, output, session){
  df <- reactive({
    if (is.null(input$csv_file))
      return(read.csv("Mall_Customers.csv"))
    data<-read.csv(input$csv_file$datapath)
    return(data)})
  
  observe({
    # Can also set the label and select items
    if(ncol(df()) < 2){
      return
    }
    if(ncol(df()) > 2){
      updateSelectInput(session, "inSelect3",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_Spectral",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_Kmeans",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
      updateSelectInput(session, "inSelect3_Cmeans",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=names(df())[length(names(df()))-2]
      )
    } else{
      updateSelectInput(session, "inSelect3",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=tail(names(df()),1)
      )
      updateSelectInput(session, "inSelect3_Spectral",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=tail(names(df()),1)
      )
      updateSelectInput(session, "inSelect3_Kmeans",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=tail(names(df()),1)
      )
      updateSelectInput(session, "inSelect3_Cmeans",
                        label = paste("Select which columns to use:"),
                        choices = names(df()),selected=tail(names(df()),1)
      )
    }
    updateSelectInput(session, "inSelect",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    updateSelectInput(session, "inSelect_Spectral",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_Spectral",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    
    
    updateSelectInput(session, "inSelect_Kmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_Kmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )
    
    updateSelectInput(session, "inSelect_Cmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=tail(names(df()),1)
    )
    updateSelectInput(session, "inSelect2_Cmeans",
                      label = paste("Select which columns to use:"),
                      choices = names(df()),selected=names(df())[length(names(df()))-1]
    )

  })
  
  output$rawdata <- renderTable(df())
  
  
  output$Gaussian_clusterchart <- plotly::renderPlotly({
    data_input <- df()
    data_input <- data_input[complete.cases(data_input), ]
    if (input$GMM_plot == "2D"){
      if(ncol(data_input) < 2){
        stop("Need at least two columns in dataset!")
      }
      if(input$inSelect2 == input$inSelect){
        stop("Choose different columns of dataset!")
      }
    data_input_plot <- data_input[input$inSelect]
    data_input_plot[input$inSelect2] <- data_input[input$inSelect2]
    names(data_input_plot)[1] <- "X1"
    names(data_input_plot)[2] <- "X2"
    if(is.character(data_input_plot$X1) || is.character(data_input_plot$X2)){
      stop("Gaussian Mixture currently does not support string input.")
    }
    gmm <- GMM(data_input_plot,input$clustnum_GMM)
    group <- predict(gmm,data_input_plot)
    data_input_plot$group <- group
    names(data_input_plot)[3] <- "group"
    ggplot(data=data_input_plot, mapping=aes(x=data_input_plot$X1, y=data_input_plot$X2,color=as.factor(data_input_plot$group))) + geom_point(size=2) + labs(x="input data last column", y = "input data last but second column", colour = "Clusters")
    } 
    else{
      if(ncol(data_input) < 3){
        stop("Need at least three columns in dataset!")
      }
      if(input$inSelect == input$inSelect2 || input$inSelect == input$inSelect3 || input$inSelect2 == input$inSelect3){
        stop("Choose different columns of dataset!")
      }
      data_input_plot_3D <- data_input[input$inSelect]
      data_input_plot_3D[input$inSelect2] <- data_input[input$inSelect2]
      data_input_plot_3D[input$inSelect3] <- data_input[input$inSelect3]
      names(data_input_plot_3D)[1] <- "X1"
      names(data_input_plot_3D)[2] <- "X2"
      names(data_input_plot_3D)[3] <- "X3"
      if(is.character(data_input_plot_3D$X1) || is.character(data_input_plot_3D$X2) || is.character(data_input_plot_3D$X3)){
        stop("Gaussian Mixture currently does not support string input.")
      }
      gmm <- GMM(data_input_plot_3D,input$clustnum_GMM)
      group <- predict(gmm,data_input_plot_3D)
      data_input_plot_3D$group <- group
      names(data_input_plot_3D)[4] <- "group"
      plot_ly(x=data_input_plot_3D$X1, y=data_input_plot_3D$X2, z=data_input_plot_3D$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_3D$group))
    }
    })
  
  
  output$kmeans_clusterchart <- plotly::renderPlotly({
    data_input <-df()
    if (input$kmeans_plot == "2D"){
      if(ncol(data_input) < 2){
        stop("Need at least two columns in dataset!")
      }
      if(input$inSelect_Kmeans == input$inSelect2_Kmeans){
        stop("Choose different columns of dataset!")
      }
    data_input_plot_Kmeans <- data_input[input$inSelect_Kmeans]
    data_input_plot_Kmeans[input$inSelect2_Kmeans] <- data_input[input$inSelect2_Kmeans]
    
    names(data_input_plot_Kmeans)[1] <- "X1"
    names(data_input_plot_Kmeans)[2] <- "X2"
    spec_group <- kkmeans(data.matrix(data_input_plot_Kmeans),centers=input$clustnum_kmeans)[1:nrow(data_input_plot_Kmeans)]
    data_input_plot_Kmeans$group <- spec_group
    names(data_input_plot_Kmeans)[3] <- "group"
    ggplot(data=data_input_plot_Kmeans, mapping=aes(x=data_input_plot_Kmeans$X1, y=data_input_plot_Kmeans$X2,color=as.factor(data_input_plot_Kmeans$group))) + geom_point(size=2) + labs(x="input data last column", y = "input data last but second column", colour = "Clusters")
    }
    else{
      if(ncol(data_input) < 3){
        stop("Need at least three columns in dataset!")
      }
      if(input$inSelect_Kmeans == input$inSelect2_Kmeans || input$inSelect2_Kmeans == input$inSelect3_Kmeans || input$inSelect_Kmeans == input$inSelect3_Kmeans){
        stop("Choose different columns of dataset!")
      }
      data_input_plot_3D_K <- data_input[input$inSelect_Kmeans]
      data_input_plot_3D_K[input$inSelect2_Kmeans] <- data_input[input$inSelect2_Kmeans]
      data_input_plot_3D_K[input$inSelect3_Kmeans] <- data_input[input$inSelect3_Kmeans]      
      names(data_input_plot_3D_K)[1] <- "X1"
      names(data_input_plot_3D_K)[2] <- "X2"
      names(data_input_plot_3D_K)[3] <- "X3"
      spec_group <- kkmeans(data.matrix(data_input_plot_3D_K),centers=input$clustnum_kmeans)[1:nrow(data_input_plot_3D_K)]
      data_input_plot_3D_K$group <- spec_group
      names(data_input_plot_3D_K)[4] <- "group"
      plot_ly(x=data_input_plot_3D_K$X1, y=data_input_plot_3D_K$X2, z=data_input_plot_3D_K$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_3D_K$group))
    }
    })
  
  output$clusterchart_spectral <- plotly::renderPlotly({
    data_input <-df()
    if (input$Spectral_plot == "2D"){
      if(ncol(data_input) < 2){
        stop("Need at least two columns in dataset!")
      }
      if(input$inSelect_Spectral == input$inSelect2_Spectral){
        stop("Choose different columns of dataset!")
      }
    data_input_plot_Spectral <- data_input[input$inSelect_Spectral]
    data_input_plot_Spectral[input$inSelect2_Spectral] <- data_input[input$inSelect2_Spectral]    
    names(data_input_plot_Spectral)[1] <- "X1"
    names(data_input_plot_Spectral)[2] <- "X2"
    spec_group <- specc(data.matrix(data_input_plot_Spectral),centers=input$clustnum_spec)[1:nrow(data_input_plot_Spectral)]
    data_input_plot_Spectral$group <- spec_group
    names(data_input_plot_Spectral)[3] <- "group"
    ggplot(data=data_input_plot_Spectral, mapping=aes(x=data_input_plot_Spectral$X1, y=data_input_plot_Spectral$X2,color=as.factor(data_input_plot_Spectral$group))) + geom_point(size=2) + labs(x="input data last column", y = "input data last but second column", colour = "Clusters")
    }else{
      if(ncol(data_input) < 3){
        stop("Need at least three columns in dataset!")
      }
      if(input$inSelect_Spectral == input$inSelect2_Spectral || input$inSelect_Spectral == input$inSelect3_Spectral || input$inSelect2_Spectral == input$inSelect3_Spectral){
        stop("Choose different columns of dataset!")
      }
      data_input_plot_Spec <- data_input[input$inSelect_Spectral]
      data_input_plot_Spec[input$inSelect2_Spectral] <- data_input[input$inSelect2_Spectral]
      data_input_plot_Spec[input$inSelect3_Spectral] <- data_input[input$inSelect3_Spectral]
      names(data_input_plot_Spec)[1] <- "X1"
      names(data_input_plot_Spec)[2] <- "X2"
      names(data_input_plot_Spec)[3] <- "X3"
      spec_group <- specc(data.matrix(data_input_plot_Spec),centers=input$clustnum_spec)[1:nrow(data_input_plot_Spec)]
      data_input_plot_Spec$group <- spec_group
      names(data_input_plot_Spec)[4] <- "group"
      plot_ly(x=data_input_plot_Spec$X1, y=data_input_plot_Spec$X2, z=data_input_plot_Spec$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Spec$group))
    }
      })
  output$clusterchart_cmeans <- plotly::renderPlotly({
    data_input <-df()
    if (input$cmeans_plot == "2D"){
      if(ncol(data_input) < 2){
        stop("Need at least two columns in dataset!")
      }
      if(input$inSelect_Cmeans == input$inSelect2_Cmeans){
        stop("Choose different columns of dataset!")
      }
      data_input_plot_Cmeans <- data_input[input$inSelect_Cmeans]
      data_input_plot_Cmeans[input$inSelect2_Cmeans] <- data_input[input$inSelect2_Cmeans]    
      names(data_input_plot_Cmeans)[1] <- "X1"
      names(data_input_plot_Cmeans)[2] <- "X2"
      cmeans_group <- fcm(data_input_plot_Cmeans,centers=input$clustnum_cmeans)$cluster
      data_input_plot_Cmeans$group <- cmeans_group
      names(data_input_plot_Cmeans)[3] <- "group"
      ggplot(data=data_input_plot_Cmeans, mapping=aes(x=data_input_plot_Cmeans$X1, y=data_input_plot_Cmeans$X2,color=as.factor(data_input_plot_Cmeans$group))) + geom_point(size=2) + labs(x="input data last column", y = "input data last but second column", colour = "Clusters")
    }else{
      if(ncol(data_input) < 3){
        stop("Need at least three columns in dataset!")
      }
      if(input$inSelect_Cmeans == input$inSelect2_Cmeans || input$inSelect2_Cmeans == input$inSelect3_Cmeans || input$inSelect_Cmeans == input$inSelect3_Cmeans){
        stop("Choose different columns of dataset!")
      }
      data_input_plot_C <- data_input[input$inSelect_Cmeans]
      data_input_plot_C[input$inSelect2_Cmeans] <- data_input[input$inSelect2_Cmeans]
      data_input_plot_C[input$inSelect3_Cmeans] <- data_input[input$inSelect3_Cmeans]
      names(data_input_plot_C)[1] <- "X1"
      names(data_input_plot_C)[2] <- "X2"
      names(data_input_plot_C)[3] <- "X3"
      cmeans_group <- fcm(data_input_plot_C,centers=input$clustnum_cmeans)$cluster
      data_input_plot_C$group <- cmeans_group
      names(data_input_plot_C)[4] <- "group"
      plot_ly(x=data_input_plot_C$X1, y=data_input_plot_C$X2, z=data_input_plot_C$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_C$group))
    }
  })
  
  result <- reactive({
    data_input <- df()
    if(ncol(data_input) < 2){
      stop("Need at least two columns in dataset!")
    }
    if(input$GMM_plot == "3D" || input$Spectral_plot == "3D" || input$cmeans_plot == "3D" || input$kmeans_plot == "3D"){
      if(ncol(data_input) < 3){
        stop("Need at least three columns in dataset!")
      }
    }
    if (input$GMM_plot == "2D"){
      if(input$inSelect == input$inSelect2){
        stop("Choose different columns of dataset for Gaussian Mixture clustering!")
      }
    data_GMM <- data_input[input$inSelect]
    data_GMM[input$inSelect2] <- data_input[input$inSelect2]
    gmm <- GMM(data_GMM,input$clustnum_GMM)
    result_GMM <- predict(gmm,data_GMM)
    } else{
      if(input$inSelect == input$inSelect2 || input$inSelect2 == input$inSelect3 || input$inSelect == input$inSelect3){
        stop("Choose different columns of dataset for Gaussian Mixture clustering!")
      }
      data_GMM <- data_input[input$inSelect]
      data_GMM[input$inSelect2] <- data_input[input$inSelect2]
      data_GMM[input$inSelect3] <- data_input[input$inSelect3]
      gmm <- GMM(data_GMM,input$clustnum_GMM)
      result_GMM <- predict(gmm,data_GMM)
    }
    if (input$kmeans_plot == "2D"){
      if(input$inSelect_Kmeans == input$inSelect2_Kmeans){
        stop("Choose different columns of dataset for K-means clustering!")
      }
      data_Kmeans <- data_input[input$inSelect_Kmeans]
      data_Kmeans[input$inSelect2_Kmeans] <- data_input[input$inSelect2_Kmeans]
      result_kmean <- kkmeans(data.matrix(data_Kmeans), centers = input$clustnum_kmeans)
    } else{
      if(input$inSelect_Kmeans == input$inSelect2_Kmeans || input$inSelect2_Kmeans == input$inSelect3_Kmeans || input$inSelect_Kmeans == input$inSelect3_Kmeans){
        stop("Choose different columns of dataset for K-means clustering!")
      }
      data_Kmeans <- data_input[input$inSelect_Kmeans]
      data_Kmeans[input$inSelect2_Kmeans] <- data_input[input$inSelect2_Kmeans]
      data_Kmeans[input$inSelect3_Kmeans] <- data_input[input$inSelect3_Kmeans]
      result_kmean <- kkmeans(data.matrix(data_Kmeans), centers = input$clustnum_kmeans)
    }
    if(input$Spectral_plot == "2D"){
      if(input$inSelect_Spectral == input$inSelect2_Spectral){
        stop("Choose different columns of dataset for Spectral clustering!")
      }
      data_Spectral <- data_input[input$inSelect_Spectral]
      data_Spectral[input$inSelect2_Spectral] <- data_input[input$inSelect2_Spectral]
    result_spec <- specc(data.matrix(data_Spectral), centers = input$clustnum_spec)
    } else{
      if(input$inSelect_Spectral == input$inSelect2_Spectral || input$inSelect2_Spectral == input$inSelect3_Spectral || input$inSelect_Spectral == input$inSelect3_Spectral){
        stop("Choose different columns of dataset for Spectral clustering!")
      }
      data_Spectral <- data_input[input$inSelect_Spectral]
      data_Spectral[input$inSelect2_Spectral] <- data_input[input$inSelect2_Spectral]
      data_Spectral[input$inSelect3_Spectral] <- data_input[input$inSelect3_Spectral]
      result_spec <- specc(data.matrix(data_Spectral), centers = input$clustnum_spec)
    }
    if(input$cmeans_plot == "2D"){
      if(input$inSelect_Cmeans == input$inSelect2_Cmeans){
        stop("Choose different columns of dataset for C-means clustering!")
      }
      data_Cmeans <- data_input[input$inSelect_Cmeans]
      data_Cmeans[input$inSelect2_Cmeans] <- data_input[input$inSelect2_Cmeans]
      result_cmeans <- fcm(data.matrix(data_Cmeans), centers = input$clustnum_cmeans)$cluster
    } else{
      if(input$inSelect_Cmeans == input$inSelect2_Cmeans || input$inSelect2_Cmeans == input$inSelect3_Cmeans || input$inSelect_Cmeans == input$inSelect3_Cmeans){
        stop("Choose different columns of dataset for C-means clustering!")
      }
      data_Cmeans <- data_input[input$inSelect_Cmeans]
      data_Cmeans[input$inSelect2_Cmeans] <- data_input[input$inSelect2_Cmeans]
      data_Cmeans[input$inSelect3_Cmeans] <- data_input[input$inSelect3_Cmeans]
      result_cmeans <- fcm(data.matrix(data_Cmeans), centers = input$clustnum_cmeans)$cluster
    }
    data_input$Gaussian_result <- as.integer(result_GMM)
    data_input$Kmean_result <- result_kmean[1:nrow(data_input)]
    data_input$Spectral_result <- result_spec[1:nrow(data_input)]
    data_input$cmeans_result <- as.integer(result_cmeans)
    return(data_input)
  })
  output$clustered_data <- renderTable(result())
  
  output$download <- downloadHandler(
    filename = function(){"processed.csv"},
    content = function(fname){
      write.csv(result(), fname)
    }
  )
}
)

shinyApp(ui = ui, server = server)