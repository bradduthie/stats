library(shiny);

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualising the Chi-square statistic and p-values"),

    hr(),
    h4("The plot below shows the Chi-square distribution. Use the slider at the bottom to change the Chi-square value, then see how the probability of sampling this Chi-square value or higher changes. Use the numeric input to change the degrees of freedom."),
    hr(),

    plotOutput("distPlot"),

    fluidRow(
        sliderInput("cs", label = "Chi-square", width="100%", min = 0,
                    max = 20, value = 1, step = 0.1)
    ),
    fluidRow(
        column(1, offset = 0),
        column(4, offset = 1,
               numericInput("df", label = "degrees of freedom (df)", width="100%",
                           min = 1, max = Inf, value = 4, step = 1),
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
        cs  <- input$cs;
        df  <- input$df;
        zz  <- pchisq(q = cs, df = df);
        xx  <- seq(from = 0.0001, to = 20, by = 0.0001);
        yy  <- dchisq(x = xx, df = df);
        o1  <- which(xx >= cs);
        b1  <- which(xx <= cs);
        par(mar = c(5, 5, 1, 1), lwd = 3);
        mxy <- max(yy[1:length(xx)]);
        ymx <- 0.2;
        if(mxy > 0.2){
          ymx <- mxy;
        }
        if(df == 1){
          ymx   <- 3;
          ycurr <- yy[xx==cs];
          plot(x = xx, y = yy, type = "l", lwd = 4, cex.lab = 2, cex.lab = 2,
               cex.axis = 2, ylab = "Probability density", ylim = c(0, ymx),
               xlab = "Chi-square value", xlim = c(0, 4));
          polygon(c(xx[o1], max(xx), rev(xx[o1])),
                  c(ycurr[1], yy[o1], rep(0, length(xx[o1]))),
                  col="#E69F00", lwd = 4);
          mxx <- which(xx == max(xx[b1]));
          polygon(c(xx[b1], max(xx[b1]), rev(xx[b1])),
                  c(yy[b1], yy[mxx], rep(0, length(b1))),
                  col="#56B4E9", lwd = 4);
          bx1y <- ymx;
          bx2y <- 0.91 * ymx;
          bx3y <- 0.81 * ymx;
          bx4y <- 0.90 * ymx;
          t1y  <- 0.955 * ymx;
          t2y  <- 0.855 * ymx;
          points(x = xx, y = yy, type = "l", lwd = 4);
          tbox <- mbox(x0 = 1, x1 = 3, y0 = bx2y, y1 = bx1y);
          text(x = 2, y = t1y, labels = "", cex = 1.5);
          polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#56B4E9");
          tbox <- mbox(x0 = 1, x1 = 3, y0 = bx3y, y1 = bx4y);
          text(x = 2, y = t2y, labels = "", cex = 1.5);
          polygon(x=tbox$x, y=tbox$y, lwd = 3, border="black", col="#E69F00");
          pbrown   <- round(1 - pchisq(q = cs, df = df), digits = 3);
          pblue    <- round(1 - pbrown, digits = 3);
          text(x = 2, y = t1y,
               labels = paste("P =", pblue), cex = 2);
          text(x = 2, y = t2y,
               labels = paste("P =", pbrown), cex = 2);
        }else{
          ycurr <- yy[xx==cs];
          plot(x = xx, y = yy, type = "l", lwd = 4, cex.lab = 2, cex.lab = 2,
               cex.axis = 2, ylab = "Probability density", ylim = c(0, ymx),
               xlab = "Chi-square value", xlim = c(0, 20));
          polygon(c(xx[o1], max(xx), rev(xx[o1])),
                  c(ycurr[1], yy[o1], rep(0, length(xx[o1]))),
                  col="#E69F00", lwd = 4);
          mxx <- which(xx == max(xx[b1]));
          polygon(c(xx[b1], max(xx[b1]), rev(xx[b1])),
                  c(yy[b1], yy[mxx], rep(0, length(b1))),
                  col="#56B4E9", lwd = 4);
          bx1y <- ymx;
          bx2y <- 0.91 * ymx;
          bx3y <- 0.81 * ymx;
          bx4y <- 0.90 * ymx;
          t1y  <- 0.955 * ymx;
          t2y  <- 0.855 * ymx;
          points(x = xx, y = yy, type = "l", lwd = 4);
          tbox <- mbox(x0 = 16, x1 = 20, y0 = bx2y, y1 = bx1y);
          text(x = 10, y = t1y, labels = "", cex = 1.5);
          polygon(x=tbox$x, y=tbox$y, lwd=3, border="black", col="#56B4E9");
          tbox <- mbox(x0 = 16, x1 = 20, y0 = bx3y, y1 = bx4y);
          text(x = 10, y = t2y, labels = "", cex = 1.5);
          polygon(x=tbox$x, y=tbox$y, lwd = 3, border="black", col="#E69F00");
          pbrown   <- round(1 - pchisq(q = cs, df = df), digits = 3);
          pblue    <- round(1 - pbrown, digits = 3);
          text(x = 18, y = t1y,
               labels = paste("P =", pblue), cex = 2);
          text(x = 18, y = t2y,
               labels = paste("P =", pbrown), cex = 2);
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)


