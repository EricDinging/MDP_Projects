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
  
  data_Kmeans_result <- reactive({
    data_input <- df()
    if (input$kmeans_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect_Kmeans != input$inSelect2_Kmeans, "Choose different columns of dataset!")
      )
      data_input_plot_Kmeans <- data.frame(X1 = data_input[input$inSelect_Kmeans], X2 = data_input[input$inSelect2_Kmeans])
      colnames(data_input_plot_Kmeans) = c("X1", "X2")
      data_input_plot_Kmeans$group <- kkmeans(data.matrix(data_input_plot_Kmeans),centers=input$clustnum_kmeans)[1:nrow(data_input_plot_Kmeans)]
      return(data_input_plot_Kmeans)
    } else {
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_Kmeans != input$inSelect2_Kmeans && input$inSelect_Kmeans != input$inSelect3_Kmeans && input$inSelect2_Kmeans != input$inSelect3_Kmeans, "Choose different columns of dataset!")
      )
      data_input_plot_3D_K <- data.frame(X1 = data_input[input$inSelect_Kmeans], X2 = data_input[input$inSelect2_Kmeans], X3 = data_input[input$inSelect3_Kmeans])
      colnames(data_input_plot_3D_K) = c("X1", "X2", "X3")
      data_input_plot_3D_K$group <- kkmeans(data.matrix(data_input_plot_3D_K),centers=input$clustnum_kmeans)[1:nrow(data_input_plot_3D_K)]
      return(data_input_plot_3D_K)
    }
  })
  
  data_Guassian_result <- reactive({
    data_input <- df()
    data_input <- data_input[complete.cases(data_input), ]
    if (input$GMM_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect != input$inSelect2, "Choose different columns of dataset!")
      )
      data_input_plot <- data.frame(X1 = data_input[input$inSelect], X2 = data_input[input$inSelect2])
      colnames(data_input_plot) = c("X1", "X2")
      if(is.character(data_input_plot$X1) || is.character(data_input_plot$X2)){
        stop("Gaussian Mixture currently does not support string input.")
      }
      gmm <- GMM(data_input_plot,input$clustnum_GMM)
      data_input_plot$group <- predict(gmm,data_input_plot)
      return(data_input_plot)
    } 
    else{
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect != input$inSelect2 && input$inSelect != input$inSelect3 && input$inSelect2 != input$inSelect3, "Choose different columns of dataset!")
      )
      data_input_plot_3D <- data.frame(X1 = data_input[input$inSelect], X2 =  data_input[input$inSelect2], X3 = data_input[input$inSelect3])
      colnames(data_input_plot_3D) <- c("X1, X2, X3")
      validate(
        need(!is.character(data_input_plot$X1) && !is.character(data_input_plot$X2), "Gaussian Mixture currently does not support string input.")
      )
      gmm <- GMM(data_input_plot_3D,input$clustnum_GMM)
      data_input_plot_3D$group <- predict(gmm,data_input_plot_3D)
      return(data_input_plot_3D)
    }
  })
  
  data_Spectral_result <- reactive({
    data_input <-df()
    if (input$Spectral_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect_Spectral != input$inSelect2_Spectral, "Choose different columns of dataset!")
      )
      data_input_plot_Spectral <- data.frame(X1 = data_input[input$inSelect_Spectral], X2 = data_input[input$inSelect2_Spectral])
      colnames(data_input_plot_Spectral) <- c("X1", "X2")
      data_input_plot_Spectral$group <- specc(data.matrix(data_input_plot_Spectral),centers=input$clustnum_spec)
      return(data_input_plot_Spectral)
    }else{
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_Spectral != input$inSelect2_Spectral && input$inSelect_Spectral != input$inSelect3_Spectral && input$inSelect2_Spectral != input$inSelect3_Spectral, "Choose different columns of dataset!")
      )
      data_input_plot_Spectral <- data.frame(X1 = data_input[input$inSelect_Spectral], X2 = data_input[input$inSelect2_Spectral], X3 = data_input[input$inSelect3_Spectral])
      colnames(data_input_plot_Spectral) <- c("X1", "X2", "X3")
      data_input_plot_Spectral$group <- specc(data.matrix(data_input_plot_Spec),centers=input$clustnum_spec)
      return(data_input_plot_Spectral)
    }
  })
  
  data_Cmeans_result <- reactive({
    data_input <-df()
    if (input$cmeans_plot == "2D"){
      validate(
        need(ncol(data_input) >= 2, "Need at least two columns in dataset!")
      )
      validate(
        need(input$inSelect2_Cmeans != input$inSelect_Cmeans, "Choose different columns of dataset!")
      )
      data_input_plot_Cmeans <- data.frame(X1 = data_input[input$inSelect_Cmeans], X2 = data_input[input$inSelect2_Cmeans])
      colnames(data_input_plot_Cmeans) <- c("X1", "X2")
      data_input_plot_Cmeans$group <- fcm(data.matrix(data_input_plot_Cmeans),centers=input$clustnum_cmeans)$cluster
      return(data_input_plot_Cmeans)
    }else{
      validate(
        need(ncol(data_input) >= 3, "Need at least three columns in dataset!")
      )
      validate(
        need(input$inSelect_Cmeans != input$inSelect2_Cmeans && input$inSelect_Cmeans != input$inSelect3_Cmeans && input$inSelect2_Cmeans != input$inSelect3_Cmeans, "Choose different columns of dataset!")
      )
      data_input_plot_Cmeans <- data.frame(X1 = data_input[input$inSelect_Cmeans], X2 = data_input[input$inSelect2_Cmeans], X3 = data_input[input$inSelect3_Cmeans])
      colnames(data_input_plot_Cmeans) <- c("X1", "X2", "X3")
      data_input_plot_Cmeans$group <- fcm(data.matrix(data_input_plot_Cmeans),centers=input$clustnum_cmeans)$cluster
      return(data_input_plot_Cmeans)
    }
  })
  
  output$kmeans_clusterchart <- plotly::renderPlotly({
    data_input_plot_Kmeans <- data_Kmeans_result()
    if (input$kmeans_plot == "2D"){
      plot_ly(x=data_input_plot_Kmeans$X1, y=data_input_plot_Kmeans$X2, z=data_input_plot_Kmeans$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot_Kmeans$group))
    }
    else{
      plot_ly(x=data_input_plot_Kmeans$X1, y=data_input_plot_Kmeans$X2, z=data_input_plot_Kmeans$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Kmeans$group))
    }
  })
  
  
  output$Gaussian_clusterchart <- plotly::renderPlotly({
    data_input_plot <- data_Guassian_result()
    if (input$GMM_plot == "2D"){
      plot_ly(x=data_input_plot$X1, y=data_input_plot$X2, z=data_input_plot$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot$group))
    } 
    else{
      plot_ly(x=data_input_plot$X1, y=data_input_plot$X2, z=data_input_plot$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot$group))
    }
  })
  
  output$clusterchart_spectral <- plotly::renderPlotly({
    data_input_plot_Spectral <- data_Spectral_result()
    if (input$Spectral_plot == "2D"){
      plot_ly(x=data_input_plot_Spectral$X1, y=data_input_plot_Spectral$X2, z=data_input_plot_Spectral$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot_Spectral$group))
    }else{
      plot_ly(x=data_input_plot_Spectral$X1, y=data_input_plot_Spectral$X2, z=data_input_plot_Spectral$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Spectral$group))
    }
  })
  
  output$clusterchart_cmeans <- plotly::renderPlotly({
    data_input_plot_Cmeans <- data_Cmeans_result()
    if (input$cmeans_plot == "2D"){
      plot_ly(x=data_input_plot_Cmeans$X1, y=data_input_plot_Cmeans$X2, z=data_input_plot_Cmeans$X3, type="scatter", marker=list(size = 12), mode="markers", color=as.factor(data_input_plot_Cmeans$group))
    }else{
      plot_ly(x=data_input_plot_Cmeans$X1, y=data_input_plot_Cmeans$X2, z=data_input_plot_Cmeans$X3, type="scatter3d", mode="markers", color=as.factor(data_input_plot_Cmeans$group))
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
    result_kmean <- data_Kmeans_result()$group
    result_GMM <- data_Guassian_result()$group
    result_spec <- data_Spectral_result()$group
    result_cmeans <- data_Cmeans_result()$group
    data_input$Gaussian_result <- as.integer(result_GMM)
    data_input$Kmean_result <- result_kmean[1:nrow(data_input)]
    data_input$Spectral_result <- result_spec[1:nrow(data_input)]
    data_input$cmeans_result <- as.integer(result_cmeans)
    return(data_input)
  })
  output$clustered_data <- renderTable(result())
  
  output$download <- downloadHandler(
    filename = "processed.csv",
    content = function(fname){
      write.csv(result(), fname)
    }
  )
}
)

shinyApp(ui = ui, server = server)