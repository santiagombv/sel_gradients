#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(MASS)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Gradientes de selección"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("b1", label = "beta 1", min=-2, max=2, value = 0, step = 0.1),
            sliderInput("b2", label = "beta 2", min=-2, max=2, value = 0, step = 0.1), 
            width=3),
        
        mainPanel(
            plotOutput(outputId = "pob"), width=7)
    )
)


# Define server logic required to draw a histogram
server<-function(input, output) {
    output$pob <- renderPlot({
        X<-matrix(c(1,0,0,1),2,2)
        MVN <- mvrnorm(n = 1000, mu = c(0,0), Sigma=X)
        mvn <- data.frame(x1 = MVN[,1], x2 = MVN[,2])
        
        t.kde <- kde2d(mvn$x1, mvn$x2, n = 50)   # from MASS package
        col2 <- heat.colors(length(t.kde$z))[rank(t.kde$z)]
        persp(x=t.kde, col =col2, theta = 20)
        
        #ggplot(data=mvn, aes(x=x1, y=x2)) + geom_hex() + theme_bw()

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
