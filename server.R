library(shiny)
library(mclust)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

paramNames <- c("xcol", "ycol", "clusters", "observationsNo")

shinyServer(function(input, output, session) {
    
    getParams <- function(prefix) {
        input[[paste0(prefix, "_recalc")]]
        
        params <- lapply(paramNames, function(p) {
            input[[paste0(prefix, "_", p)]]
        })
        names(params) <- paramNames
        params
    }

    
    dataListA <- reactive(do.call(prepareDataA, getParams("a")))
    dataListB <- reactive(do.call(prepareDataB, getParams("b")))
    
    output$a_plot1 <- renderPlot({
        plotDataA(dataListA())
    })
    output$b_plot1 <- renderPlot({
        plotDataB(dataListB())
    })

    output$msgA <- renderText({ 
        paste0("Computation time: ", round(dataListA()[[6]], 2), "[ms]")
    })
    output$msgB <- renderText({ 
        paste0("Computation time: ", round(dataListB()[[6]], 2), "[ms]")
    })

})

validateobservationsNoParameter <- function(
    observationsNoParameter,
    clusterNumber,
    irisLength
    ) {
    
    observationsNoErrorMsg <-
        paste("The value of the 'Number of observations' parameter can not be",
              paste0(observationsNoParameter, "."),
              "It should take the integers from", clusterNumber,
              "(number of clusters)", "to", paste0(irisLength, "."))
    
    if(!is.numeric(observationsNoParameter) ||
       (round(observationsNoParameter) != observationsNoParameter) ||
       observationsNoParameter < clusterNumber ||
       observationsNoParameter > irisLength
    ) {
        stop(observationsNoErrorMsg)
    }
    
}


prepareDataA <- function(xcol, ycol, clusters, observationsNo) {

    validateobservationsNoParameter(observationsNo, clusters, dim(iris)[1])
    
    selectedData <- iris[1:observationsNo, c(xcol, ycol)]
    
    timeBegin <- Sys.time()
    meansClusters <- kmeans(selectedData, clusters)
    timeEnd <- Sys.time()
    
    parList <- list(xcol, ycol, clusters, selectedData, meansClusters,
                    as.numeric(timeEnd - timeBegin)*1000)
}

prepareDataB <- function(xcol, ycol, clusters, observationsNo) {
    
    validateobservationsNoParameter(observationsNo, clusters, dim(iris)[1])
    
    selectedData <- iris[1:observationsNo, c(xcol, ycol)]
    
    timeBegin <- Sys.time()
    mClust <- Mclust(selectedData, G = clusters)
    timeEnd <- Sys.time()
    
    parList <- list(xcol, ycol, clusters, selectedData, mClust,
                    as.numeric(timeEnd - timeBegin)*1000)
}

plotDataA <- function(dataList) {

        par(mar = c(5.1, 4.1, 0, 1))
        plot(dataList[[4]],
             col = dataList[[5]]$cluster,
             pch = 20, cex = 3)
        points(dataList[[5]]$centers, pch = 4, cex = 4, lwd = 4)
        grid()
    
}

plotDataB <- function(dataList) {
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(dataList[[5]], what = c("classification"), main = "")
    grid()
    
}
