library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Visualising the t-statistic and p-values"),
    
    hr(),
    h4("The plot below shows the Student's t-distribution. Use the slider at the bottom to change the t-score in the histogram, and see how the probability of sampling above and below the t-score changes. Use the pull down menu to switch to a two-tailed distribution to see the probability of sampling a value as more extreme than your z-score in either direction. Use the numeric input to change the degrees of freedom."),
    hr(),
    
    plotOutput("distPlot"),
    
    fluidRow(
        sliderInput("t", label = "t", width="100%", min = -4, 
                    max = 4, value = 0, step = 0.01)
        ),
    fluidRow(
        column(4, offset = 1,
               selectInput("tailed", "Tailed",
                           c("One-tailed" = "1", 
                             "Two-tailed" = "2"),
                           selected = "1"),
               
        ),
        column(1, offset = 0),
        column(4, offset = 1,
               numericInput("df", label = "Degrees of Freedom", width="100%",
                           min = 1, max = Inf, value = 4, step = 1)
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    mbox <- function(x0, x1, y0, y1){
        xx <- seq(from=x0, to=x1, length.out = 100);
        yy <- seq(from=y0, to=y1, length.out = 100);
        xd <- c(rep(x0, 100), xx, rep(x1,100), rev(xx));
        yd <- c(yy, rep(y1,100), rev(yy), rep(y0, 100));
        return(list(x=xd, y=yd));
    }
    
    output$distPlot <- renderPlot({
        if(input$tailed == "1"){
            zz  <- input$t;
            vv  <- input$df;
            xx  <- seq(from = -5, to = 5, by = 0.0001);
            yy  <- dt(xx, df = vv);
            par(mar = c(5, 5, 1, 1), lwd = 3);
            plot(x = xx, y = yy, type = "l", lwd = 4, cex.lab = 2, cex.lab = 2,
                 cex.axis = 2, ylab = "Probability density", ylim = c(0, 0.4),
                 xlab = "t-score");
            polygon(c(xx[xx>=zz], max(xx)), c(yy[xx==max(xx)], yy[xx>=zz]), 
                    col="#E69F00");
            polygon(c(min(xx), xx[xx < zz]), c(yy[xx < zz], yy[xx==min(xx)]), 
                    col="#56B4E9");
            points(x = xx, y = yy, type = "l", lwd = 4);
            tbox <- mbox(x0 =-5, x1 = -2.5, y0 = 0.15, y1 = 0.3);
            text(x = mean(c(-5, -2.5)), y = 0.325, labels = "Probability blue",
                 cex = 1.5);
            polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#56B4E9");
            tbox <- mbox(x0 =2.5, x1 = 5, y0 = 0.15, y1 = 0.3);
            text(x = mean(c(5, 2.5)), y = 0.325, labels = "Probability brown",
                 cex = 1.5);
            polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#E69F00");
            pbrown   <- round(1 - pt(q = zz, df = vv), digits = 3);
            pblue    <- round(1 - pbrown, digits = 3);
            if(zz >= 0){
                text(x = mean(c(-5, -2.5)), y = mean(c(0.15, 0.3)), 
                     labels = pblue, cex = 2);
                text(x = mean(c(5, 2.5)), y = mean(c(0.15, 0.3)), 
                     labels = paste("P =", pbrown), cex = 2);
            }else{
                text(x = mean(c(-5, -2.5)), y = mean(c(0.15, 0.3)), 
                     labels = paste("P =", pblue), cex = 2);
                text(x = mean(c(5, 2.5)), y = mean(c(0.15, 0.3)), 
                     labels = pbrown, cex = 2);
            }
        }else{
            zz  <- input$t;
            if(zz < 0){
                zz <- -1 * zz;
            }
            z2  <- -1*zz;
            vv  <- input$df;
            xx  <- seq(from = -5, to = 5, by = 0.0001);
            yy  <- dt(xx, df = vv);
            par(mar = c(5, 5, 1, 1), lwd = 3);
            plot(x = xx, y = yy, type = "l", lwd = 4, cex.lab = 2, cex.lab = 2,
                 cex.axis = 2, ylab = "Probability density", ylim = c(0, 0.4),
                 xlab = "t-score");
            polygon(c(xx[xx>=zz], max(xx)), c(yy[xx==max(xx)], yy[xx>=zz]), 
                    col="#E69F00");
            polygon(c(min(xx), xx[xx < zz]), c(yy[xx < zz], yy[xx==min(xx)]), 
                    col="#56B4E9");
            polygon(c(min(xx), xx[xx < z2]), c(yy[xx < z2], yy[xx==min(xx)]), 
                    col="#E69F00");
            points(x = xx, y = yy, type = "l", lwd = 4);
            tbox <- mbox(x0 =-5, x1 = -2.5, y0 = 0.15, y1 = 0.3);
            text(x = mean(c(-5, -2.5)), y = 0.325, labels = "Probability blue",
                 cex = 1.5);
            polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#56B4E9");
            tbox <- mbox(x0 =2.5, x1 = 5, y0 = 0.15, y1 = 0.3);
            text(x = mean(c(5, 2.5)), y = 0.325, labels = "Probability brown",
                 cex = 1.5);
            polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#E69F00");
            pbrown   <- 2 * round(1 - pt(q = zz, df = vv), digits = 3);
            pblue    <- round(1 - pbrown, digits = 3);
            text(x = mean(c(-5, -2.5)), y = mean(c(0.15, 0.3)), 
                 labels = pblue, cex = 2);
            text(x = mean(c(5, 2.5)), y = mean(c(0.15, 0.3)), 
                 labels = paste("P =", pbrown), cex = 2);
        }
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


