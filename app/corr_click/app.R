library(shiny)
library(ggplot2)


ui <- fluidPage(
    fluidRow(
        column(width = 8,
               h4("Click plot to add points"),
               actionButton("rem_point", "Remove Last Point"),
               actionButton("cor_val", "Show correlation coefficient"),
               actionButton("rm_cval", "Hide correlation coefficient"),
               actionButton("pvalue",  "Show p-value of correlation test"),
               actionButton("rm_pval", "Hide p-value of correlation test"),
               plotOutput("plot1", click = "plot_click")),
        column(width = 3,
               h4("Table of points on plot"),
               tableOutput("table"))
    )
)

server = function(input, output){
    
    values    <- reactiveValues()
    values$DT <- data.frame(x = numeric(),
                            y = numeric())
    
    output$plot1 = renderPlot({
        
        ggplot(values$DT, aes(x = x, y = y)) +
            lims(x = c(0, 100), y = c(0, 100)) +
            theme_bw() + theme(panel.border = element_blank(), 
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), 
                               axis.line = element_line(colour = "black"),
                               legend.position = "none",
                               text = element_text(size = 20),
                               axis.text = element_text(size = 20)) +
            geom_point(colour = "black", size = 3) + 
            xlab("X variable") + ylab("Y variable") +
            annotate("text", x = 0, y = 95, label = values$rval, size = 10, 
                     hjust = 0) +
            annotate("text", x = 75, y = 95, label = values$pval, size = 10, 
                     hjust = 0)
    })
    
    observeEvent(input$plot_click, {
        add_row <- data.frame(x = input$plot_click$x,
                              y = input$plot_click$y)
        values$DT <- rbind(values$DT, add_row)
    })
    
    observeEvent(input$rm_cval, {
        values$rval <- "";
    })
    
    observeEvent(input$cor_val, {
        rrep <- NA;
        if(dim(values$DT)[1] > 1){
            rval <- cor(values$DT[,1], values$DT[,2]);
            rval <- round(rval, digits = 3);
            rrep <- paste("r = ", rval, sep = "");
            values$rval <- rrep;
        }
    })
    
    observeEvent(input$pvalue, {
        prep <- NA;
        if(dim(values$DT)[1] > 1){
            pval <- cor.test(values$DT[,1], values$DT[,2])$p.value;
            pval <- round(pval, digits = 3);
            prep <- paste("p = ", pval, sep = "");
            values$pval <- prep;
        }
    })
    
    observeEvent(input$rm_pval, {
        values$pval <- "";
    })
    
    observeEvent(input$rem_point, {
        rem_row <- values$DT[-nrow(values$DT), ]
        values$DT <- rem_row
    })
    
    output$table <- renderTable({
        values$DT[,1:2];
    })
}

shinyApp(ui, server)