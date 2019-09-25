library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Gradientes de selección"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("b1", label = "beta 1", min=-0.95, max=0.95, value = 0, step = 0.1),
            sliderInput("b2", label = "beta 2", min=-0.95, max=0.95, value = 0, step = 0.1),
            sliderInput("g1", label = "gamma 11", min=-0.95, max=0.95, value = 0, step = 0.1),
            sliderInput("g2", label = "gamma 22", min=-0.95, max=0.95, value = 0, step = 0.1), 
            sliderInput("g3", label = "gamma 12", min=-0.95, max=0.95, value = 0, step = 0.1),
            width=3),
        
        mainPanel(
            plotOutput(outputId = "pob"), width=7)
    )
)


# Define server logic required to draw a histogram
server<-function(input, output) {
    output$pob <- renderPlot({
        P<-matrix(c(1,0,0,1),2,2)
        MVN <- mvrnorm(n = 1000, mu = c(0,0), Sigma=P)
        mvn <- data.frame(x1 = MVN[,1], x2 = MVN[,2])
        
        beta <- c(input$b1, input$b2)
        gamma <- matrix(c(input$g1, input$g3, input$g3, input$g2),2,2)
        deltaZ <- P%*%beta
        deltaP <- t(P)%*%gamma%*%P - deltaZ%*%t(deltaZ)
        P2 <- P + deltaP # P after selection

        MVN2 <- mvrnorm(n = 1000, mu = c(0,0)+c(deltaZ[1,], deltaZ[2,]), Sigma=P2)
        mvn2 <- data.frame(x1 = MVN2[,1], x2 = MVN2[,2])
        mvn3 <- rbind.data.frame(mvn, mvn2)
        mvn3$pob <- c(rep("antes", 1000), rep("despues", 1000))
        
        ggplot(mvn3, aes(x=x1, y = x2)) + 
            geom_density2d(aes(color=pob)) + 
            theme_bw() + xlim(-4,4) + ylim(-4,4) +
            theme(legend.position = "bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
