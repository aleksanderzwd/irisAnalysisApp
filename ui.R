library(shiny)

renderInputElements <- function(prefix) {
    wellPanel(
        fluidRow(
            column(6,
                   selectInput(paste0(prefix, "_", "xcol"), "X Variable",
                               names(iris[ , 1:4])),
                   numericInput(paste0(prefix, "_", "observationsNo"),
                                "Number of observations",
                                dim(iris)[1], min = 1, max = dim(iris)[1])
            ),
            column(6,
                   selectInput(paste0(prefix, "_", "ycol"), "Y Variable",
                               names(iris[ , 1:4]),
                               selected=names(iris)[[2]]),
                   sliderInput(paste0(prefix, "_", "clusters"),
                               "Number of clusters",
                               min = 1,  max = 9,  value = 3)
            )
        ),
        p(actionButton(paste0(prefix, "_", "recalc"),
                       "Re-run simulation", icon("random")
        ))
    )
}

# Define UI for application that plots random distributions
shinyUI(fluidPage(theme="simplex.min.css",
                  tags$style(type="text/css",
                             "label {font-size: 12px;}",
                             ".recalculating {opacity: 1.0;}"
                  ),
                  
                  # Application title
                  tags$h2("irisAnalysisApp - the Iris Dataset Analysis Application"),
                  p("The",
                    tags$a(href="https://www.coursera.org/", "Coursera"),
                    "Developing Data Products Course Assignment"),
                  hr(),
                  p("K-means and Gaussian mixture models are two canonical",
                    "approaches to clustering, i.e. dividing data points",
                    "into meaningful groups.",
                    "The irisAnalysisApp application compares the two methods",
                    "of clustering on the Iris dataset."),
                  p("Application documentation:",
                    tags$a(href = "irisAnalysisApp.pdf", "Documentation (PDF)")),
                  hr(),
                  
                  fluidRow(
                      column(6, tags$h3("Method: K-means")),
                      column(6, tags$h3("Method: Gaussian mixture models"))
                  ),
                  fluidRow(
                      column(6, renderInputElements("a")),
                      column(6, renderInputElements("b"))
                  ),
                  
                  fluidRow(
                      column(6,
                             plotOutput("a_plot1", height = "600px")
                      ),
                      column(6,
                             plotOutput("b_plot1", height = "600px")
                      )
                  ),
                  
                  fluidRow(
                      column(6,
                             verbatimTextOutput("msgA")
                      ),
                      column(6,
                             verbatimTextOutput("msgB")
                      )
                  )
                  
))
