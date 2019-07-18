library(RSelenium)
library(rvest)
library(dplyr)




#start RSelenium


rD  <- rsDriver(port = 4530L, browser = "firefox", version = "latest", chromever = "latest",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)



remDr <- rD[["client"]]



for (i in 1:7){
  
  tvs_url <- "https://www.falabella.com.pe/falabella-pe/category/cat210477/TV-Televisores?page="
  
  tvs_url_i <- paste0(tvs_url,i)
  
  remDr$navigate(tvs_url_i)
  
  Sys.sleep(10)
  
  page_source<-remDr$getPageSource()
  
  product_info <- function(node){
    precio_antes <- html_nodes(node, '.fb-price-list p:nth-child(2)') %>% html_text
    precio_actual <- html_nodes(node, '.fb-price-list p:nth-child(1)') %>% html_text 
    producto <- html_nodes(node,"div.section__pod-top-title div.LinesEllipsis") %>% html_text
    marca <- html_nodes(node,"div.section__pod-top-brand") %>% html_text
    
    precio_antes <-   gsub("\\S\\/\\. ", "", precio_antes)
    precio_actual <-   gsub("\\S\\/\\. ", "", precio_actual)
    
    
    data.frame(
      ecommerce = "falabella",
      producto = producto,
      precio_antes = ifelse(length(precio_antes)==0, NA, precio_antes),
      precio_actual = ifelse(length(precio_actual)==0, NA, precio_actual), 
      marca = ifelse(length(marca)==0, NA, marca),
      stringsAsFactors=F
    )
    
    
  }
  
  
  
  doc <- read_html(iconv(page_source[[1]]), to="UTF-8") %>% 
         html_nodes("#all-pods")
  
  
  assign(paste('falabella_tvs_pag_', i, sep=''), lapply(doc, product_info) %>%
           bind_rows())
  
}


### Combining the dfs #######


falabella_tvs <- rbind(falabella_tvs_pag_1, falabella_tvs_pag_2, falabella_tvs_pag_3,
                    falabella_tvs_pag_4, falabella_tvs_pag_5, falabella_tvs_pag_6,
                    falabella_tvs_pag_7)







falabella_tvs$precio_antes <- gsub(",", "", falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- gsub(",", "", falabella_tvs$precio_actual)


falabella_tvs$precio_antes <- gsub("S/", "", falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- gsub("S/", "", falabella_tvs$precio_actual)



falabella_tvs$precio_antes <- trimws(falabella_tvs$precio_antes, which = "both")
falabella_tvs$precio_actual <- trimws(falabella_tvs$precio_actual, which = "both")


falabella_tvs$precio_antes <- gsub(" \\(Normal\\)", "", falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- gsub(" \\(Internet\\)", "", falabella_tvs$precio_actual)


falabella_tvs$precio_antes <- as.numeric(falabella_tvs$precio_antes)
falabella_tvs$precio_actual <- as.numeric(falabella_tvs$precio_actual)



falabella_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", falabella_tvs$producto)



falabella_tvs$pulgadas <- sub('"', "", falabella_tvs$pulgadas)
falabella_tvs$pulgadas <- sub("'", "", falabella_tvs$pulgadas)


falabella_tvs$pulgadas <- as.character(falabella_tvs$pulgadas)

falabella_tvs <- falabella_tvs[!(nchar(falabella_tvs$pulgadas)>2),]


falabella_tvs <- as.data.frame(apply(falabella_tvs[,],2,tolower))


falabella_tvs$pulgadas <- sub("'", "", falabella_tvs$pulgadas)



##### Dif de precios
### Se har� cuando se junten los datos de los 3 ecommerce

# falabella_tvs <- falabella_tvs %>%
#         mutate(dif.precios = precio_antes - precio_actual)
#         
# 
# 
# falabella_tvs$dif.precios <- ifelse(is.na(falabella_tvs$dif.precios), 0, falabella_tvs$dif.precios)




### Ordenar columnas

falabella_tvs <- falabella_tvs[,c(1,5,2,3,4,6)]

###


file <- paste(as.character(Sys.Date()), "falabella-tvs", sep = "-")

falabella_tvs_csv <- paste(file, "csv", sep = ".")

write.csv(falabella_tvs, falabella_tvs_csv, row.names = F)

