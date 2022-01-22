add_eeda_style <- function (main = TRUE, ...) 
{
  output <- character()
  output <- append(output, "<style type=\"text/css\">\n")
  if (isTRUE(main)) {
    # output <- append(output, "/* Overwrite flexdashboard readable theme */\n    body {\n    background: #ffffff;\n    }\n\n    .navbar-brand  {\n    /* align flexdashboard title (aka brand) with logo */\n    padding: 15px 0px 0px 15px;\n    }\n\n    pre {\n    background-color:white; border:0; /* background for sidebar*/\n    }\n\n    /* Adjust ### headers created in flexdashboard */\n    .chart-title {\n    /* border-bottom: 1px solid #d7d7d7; */\n    border-bottom: none;\n    color: #000000;\n    font-family: Arial, Helvetica, sans-serif;\n    font-size: 22px;\n    font-weight: 700;\n    padding: 7px 0px 0px 7px;\n    margin-top: 60px; /* fixes relative link landing on page and hiding heading*/\n    }\n\n    /* Adjust #### headers created in flexdashboard */\n    h4 {\n    /* border-bottom: 1px solid #d7d7d7; */\n    border-bottom: none;\n    color: #505050;\n    font-family: Arial, Helvetica, sans-serif;\n    font-size: 18px;\n    font-weight: 600;\n    padding: 75px 0px 0px 7px;\n    margin-top: -10px; /* fixes relative link landing on page and hiding heading*/\n    }\n\n\n    /* Adjust ##### headers created in flexdashboard */\n    h5 {\n    /* border-bottom: 1px solid #d7d7d7; */\n    border-bottom: none;\n    color: #808080;\n    font-family: Arial, Helvetica, sans-serif;\n    font-size: 16px;\n    font-weight: 500;\n    padding: 16px 0px 0px 14px;\n    }\n\n    p {\n    padding: 0px 0px 0px 20px;\n    }\n\n    a:link {\n    color: blue;\n    }\n\n    a:visited {\n    color: blue;\n    }\n\n    .table {\n    margin-left: 100px\n    }\n\n\n    .table>thead>tr>th {\n    border-color: black;\n    }\n\n\n    /* Remove borders within the table body */\n    .table>tbody>tr>td {\n    border: none;\n    }\n\n\n    /* Add a top border to the table header row */\n    .table thead tr:first-child {\n    border-top: 2px solid black;\n    }\n\n\n    /* Add a bottom border to the table body\n    .table tbody tr:last-child {\n    border-bottom: 2px solid black;\n    }\n    */\n\n    /* Make the table header row a normal weight; not bold */\n    .table th{\n    font-weight: normal;\n    }\n\n\n    /* Make the caption italic and black */\n    .table caption{\n    font-style: italic;\n    color: black;\n    }\n\n    .image-container {\n    background-position: center top !important;\n    }\n\n    .scroll_chunk {\n    max-height: 425px;\n    float: left;\n    overflow-y: auto;\n    }\n\n    mark {\n    background-color: yellow;\n    color: black;\n    }")

    
    
    
    
    
    
    
      }
  dotArgs <- list(...)
  for (f in dotArgs) {
    output <- append(output, readLines(f))
  }
  output <- append(output, "</style>\n")
  output <- paste(output, sep = "\n")
  cat(output)
  return(invisible(output))
}