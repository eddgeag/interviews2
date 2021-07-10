

##### Web crawler #####

#============#
# If required download libraries

libraries <- c("stringr", "readtext", "rvest", "plyr", "dplyr", "tidyr")
check.libraries <-
  is.element(libraries, installed.packages()[, 1]) == FALSE
libraries.to.install <- libraries[check.libraries]
if (length(libraries.to.install != 0)) {
  install.packages(libraries.to.install)
}

success <-
  sapply(libraries,
         require,
         quietly = FALSE,
         character.only = TRUE)
if (length(success) != length(libraries)) {
  stop("A package failed to return a success in require() function.")
}

#============#

# Extract values of interest function
extract.items <- function(contenido,
                          columna = 1,
                          titulo = NULL,
                          orde = NULL) {
  # If titulo and orde are present, extract one record title and order
  # title and order belongs to index of the list obtained by strsplit
  # If exists orde and titulo, wha t we do is clean the string trimws
  # split the string by double whitespce
  #remove empty records from the list produced (1)
  if (!is.null(titulo) && !is.null(orde)) {
    record <- lapply(strsplit(trimws(contenido), "  ", fixed = T),
                     function(z) {
                       z[!is.na(z) & z != ""]
                     })
    # obtain the order and the title
    orden <-
      as.numeric(gsub("([0-9]+).*$", "\\1", record[[1]][orde]))
    title <- record[[1]][titulo]
    # return as a dataframe the output
    return(data.frame(orden = orden, titles = title))
    
  } else{
    # do the same with all the columns in (1)
    output.list <-
      lapply(strsplit(contenido[, columna], "  ", fixed = T), trimws)
    output.list <-
      lapply(output.list, function(z) {
        z[!is.na(z) & z != ""]
      })
    output.number <- as.numeric(gsub("([0-9]+).*$", "\\1",
                                     unlist(lapply(output.list, function(x)
                                       x[1]))))
    # if columna = 1 extract the points and comments of the news
    if (columna == 1) {
      return(data.frame(points = output.number))
      
    } else if (columna == 3) {
      #if columna = 3 extract the title and order
      titles <- unlist(lapply(output.list, function(x)
        x[3]))
      
      orden <- as.numeric(gsub("([0-9]+).*$", "\\1",
                               unlist(lapply(output.list, function(x)
                                 x[2]))))
      
      mydf <-
        data.frame(orden = orden,
                   title = titles,
                   comments = output.number)
    }
  }
  
}

#===The webcrawler of ycombinator===#
# The only input is how many records you want from the specificl url
# The output is a csv file

webcrawler <- function(lineas) {
  # Main code
  url = "https://news.ycombinator.com"
  html_document <- read_html(url) # read the url
  
  
  # extract body of html- Taking into account the newline \n
  title_xpath <- "//body"
  title_text <- html_document %>%
    html_node(xpath = title_xpath) %>%
    html_text(trim = T) %>%
    paste0(collapse = "\n")
  
  # process file
  content <-
    unlist(strsplit(title_text, "\n")) #split into list by newlines
  content <- strsplit(content, "[|]")[-(1:4)] # filter rows
  # first 30 entries get 31 to avoid the character "more" then remove
  content.df <- ldply(content[1:(lineas + 1)], rbind)
  content.df <- content.df[1:lineas,]
  colnames(content.df) <- c("points", "hide", "comments") #names of df
  
  # which index has missing data or unordered
  
  w.na <- which(is.na(content.df$comments))
  
  # extract number of comments title and order
  
  df.temp <- extract.items(contenido = content.df, columna = 3)
  
  # extract points by entry
  
  df.puntos <- extract.items(contenido = content.df, columna = 1)
  
  # bind points, comments, titles and order
  
  datos <- bind_cols(df.temp, df.puntos)
  # there are 2 records unordered on the df due to bad script in html
  #The first one and one that varies
  # first record
  first.record <-
    extract.items(
      contenido = content.df[1, 1],
      columna = 1,
      orde = 1,
      titulo = 2
    )
  weird.record <-
    extract.items(
      contenido = content.df[w.na[2], 2],
      columna = 1,
      orde = 2,
      titulo = 3
    )
  
  # add data properly on those records missplaced
  datos[w.na[1], 1:2] <- first.record
  datos[w.na[2], 1:2] <- weird.record
  datos$points[w.na] <- NA
  write.table(datos, file = paste0("./first", lineas, "records.csv", sep =
                                     ","))
  return(datos)
}



datos<-webcrawler(30)
print(datos)
