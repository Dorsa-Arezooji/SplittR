# SplittR - Split Your Files!
# Dorsa Arezooji -- https://github.com/Dorsa-Arezooji/SplittR

library(shiny)
library(shinythemes)
library(dplyr)

ui = fluidPage(
    theme = shinythemes::shinytheme("darkly"),
    titlePanel("SplittR - Split Your Files!"), h1(""),
    sidebarLayout(
        sidebarPanel(
            fileInput("input_file", "Choose .txt File",
                      multiple = FALSE,
                      accept = c('.txt', 'text', 'text/plain')),
            hr(),
            
            # Input: Checkbox if file has header
            #checkboxInput("header", "Header", TRUE),
            
            # Input: New record identifier
            textInput("rec_start", "Record Identifier", width = '200px', placeholder = 'e.g. # or "GROUP" or rec, ...'),
            hr(),
            
            # Input: Number of output files
            sliderInput("n_files", "Number of output files:", 2, 24, 1, 0),
            hr(),
            
            div(style="display:inline-block",actionButton("go", "Split!", class = "btn-success", )),
            div(style="display:inline-block",downloadButton("download_files", ".zip", class = "btn-info"))
            
        ),
        
        mainPanel(
            h2("Welcome to Splitter!"),
            h2(""),
            h4("It can split your large text files into smaller files with equal number of records!"),
            h4(""),
            h4("For more, check out ", a("my github page", href = "https://github.com/Dorsa-Arezooji"),
               "or go to ", a("my website", href = "https://Dorsa-Arezooji.github.io"), "."),
            HTML("<h4>&#169; Dorsa Arezooji, 2020 <br><br><br></h4>"),
            h3("How does it work?"),
            h3(""),
            HTML("<h4> <b>1.</b> Browse your local directory to find and upload your text file
                 <br><br> <b>2.</b> Type the record identifier, i.e a string or character that each new record starts with, as it appears in your file without any extra quotations
                 <br><br> <b>3.</b> Use the slider to choose the number of output splits
                 <br><br> <b>4.</b> Click on the Split! button to split the file
                 <br><br> <b>5.</b> Once the split is done, you can see the results below - If you receive an error, make sure to enter the record identifier correctly!
                 <br><br> <b>6.</b> Use the download button to download a zip file containing your split files!
                 <br><br><br><br> <i><b>Note:</b> The order of your records and the format/text of your files will be consistent with the input file!</i>
                 </h4>"),
            hr(),
            h3('Results'),
            tableOutput("contents")
        )
    )
)


server = function(input, output) {
    
    splittr = eventReactive(input$go,{
        req(input$input_file)
        data = readLines(input$input_file$datapath)
        df = as.data.frame(data)
        
        n_rec_input = sum(grepl(input$rec_start, df[[1]]))
        if(n_rec_input == 0) return()
        n_rec_output = as.integer(floor(n_rec_input / input$n_files))
        n_rec_output_last = n_rec_input - input$n_files * n_rec_output
        
        x = as.data.frame(grepl(input$rec_start, df[[1]]))
        xx = x
        for (i in 1:nrow(x)){
            xx[[1]][i] = sum(x[[1]][1:i])
        }
        
        output_list = list()
        i = 1
        while (i <= input$n_files - 1){
            output_list[i] = df %>% slice(min(which(xx[[1]] == n_rec_output * (i - 1) + 1)):max(which(xx[[1]] == n_rec_output * i)))
            i = i + 1
        }
        output_list[input$n_files] = df %>% slice(min(which(xx[[1]] == n_rec_output * (input$n_files - 1) + 1)):nrow(xx))
        res = list('n_rec_input' = n_rec_input, 'n_rec_output' = n_rec_output, 'output_list' = output_list)
        return(res)
    })
    
    output$download_files = downloadHandler(
        filename = function() {
        paste("output", "zip", sep=".")
    },
    content = function(fname) {
        r = splittr()
        results = r[[3]]
        file_list = c()
        for (i in 1:input$n_files){
            write.table(results[[i]], file = paste(i, '.txt'), row.names=FALSE, col.names=FALSE, quote = FALSE)
            file_list = c(file_list, paste(i, '.txt'))
        }
        zip(zipfile = fname, files = file_list, flags = "-j")
    },
    contentType = "zip"
    )
    
    output$contents = renderTable({
        out_df = data.frame('Number of output files' = input$n_files, "Number of input records" = splittr()[[1]], 'N output records' = splittr()[[2]])
        return(out_df)
    })
}

shinyApp(ui, server)
