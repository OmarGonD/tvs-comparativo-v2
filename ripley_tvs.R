library(RSelenium)
library(rvest)
library(dplyr)



#start RSelenium


rD  <- rsDriver(port = 4530L, browser = "firefox", version = "latest", chromever = "latest",
                geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
                verbose = TRUE, check = TRUE)



remDr <- rD[["client"]]




for (i in 1:6){
  
  tvs_url <- "https://simple.ripley.com.pe/tecnologia/tv-y-cine-en-casa/televisores?page="
    
  tvs_url_i <- paste0(tvs_url,i)
  
  remDr$navigate(tvs_url_i)
  
  Sys.sleep(10)
  
  page_source<-remDr$getPageSource()
  
  product_info <- function(node){
    precio_antes <- html_nodes(node, 'ul.catalog-prices__list li.catalog-prices__list-price') %>% html_text
    precio_actual <- html_nodes(node, 'ul.catalog-prices__list li.catalog-prices__offer-price') %>% html_text 
    producto <- html_nodes(node,"div.catalog-product-details__name") %>% html_text
    
    
    precio_antes <-   gsub("\\S\\/\\. ", "", precio_antes)
    precio_actual <-   gsub("\\S\\/\\. ", "", precio_actual)
    
    
    data.frame(
      ecommerce = "ripley",
      producto = producto,
      precio_antes = ifelse(length(precio_antes)==0, NA, precio_antes),
      precio_actual = ifelse(length(precio_actual)==0, NA, precio_actual), 
      #tarjeta.ripley = ifelse(length(r.tarjeta)==0, NA, r.tarjeta),
      stringsAsFactors=F
    )
    
    
  }
  
  
  
  doc <- read_html(iconv(page_source[[1]]), to="UTF-8") %>% 
    html_nodes("div.catalog-product-details")
  
  
  assign(paste('ripley_tvs_pag_', i, sep=''), lapply(doc, product_info) %>%
           bind_rows())
 
}


### Combining the dfs #######


ripley_tvs <- rbind(ripley_tvs_pag_1, ripley_tvs_pag_2,
                    ripley_tvs_pag_3, ripley_tvs_pag_4, ripley_tvs_pag_5, ripley_tvs_pag_6)






ripley_tvs$precio_antes <- trimws(ripley_tvs$precio_antes, which = "both")

ripley_tvs$precio_actual <- trimws(ripley_tvs$precio_actual, which = "both")



ripley_tvs$precio_antes <- gsub(",", "", ripley_tvs$precio_antes)
ripley_tvs$precio_actual <- gsub(",", "", ripley_tvs$precio_actual)


ripley_tvs$precio_antes <- gsub("S/", "", ripley_tvs$precio_antes)
ripley_tvs$precio_actual <- gsub("S/", "", ripley_tvs$precio_actual)



ripley_tvs$precio_antes <- as.numeric(ripley_tvs$precio_antes)
ripley_tvs$precio_actual <- as.numeric(ripley_tvs$precio_actual)

ripley_tvs$precio_antes <- ifelse(is.na(ripley_tvs$precio_antes), ripley_tvs$precio_actual, ripley_tvs$precio_antes) 



#Sino tiene precio_actual es porque el producto está agotado

ripley_tvs <- ripley_tvs[!is.na(ripley_tvs$precio_actual),]

sum(is.na(ripley_tvs$precio_actual))

ripley_tvs$producto <- gsub("â�???�", '"',ripley_tvs$producto)


##### Marca

ripley_tvs$marca <- ifelse(grepl("samsung", ripley_tvs$producto, ignore.case = T), "samsung",ripley_tvs$producto) 

ripley_tvs$marca <- ifelse(grepl("lg", ripley_tvs$producto, ignore.case = T), "lg",ripley_tvs$marca) 

ripley_tvs$marca <- ifelse(grepl("haier", ripley_tvs$producto, ignore.case = T), "haier",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("panasonic", ripley_tvs$producto, ignore.case = T), "panasonic",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("sony", ripley_tvs$producto, ignore.case = T), "sony",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("sharp", ripley_tvs$producto, ignore.case = T), "sharp",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("aoc", ripley_tvs$producto, ignore.case = T), "aoc",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("philips", ripley_tvs$producto, ignore.case = T), "philips",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("hyundai", ripley_tvs$producto, ignore.case = T), "hyundai",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("hisense", ripley_tvs$producto, ignore.case = T), "hisense",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("orizon", ripley_tvs$producto, ignore.case = T), "orizon",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("imaco", ripley_tvs$producto, ignore.case = T), "imaco",ripley_tvs$marca)

ripley_tvs$marca <- ifelse(grepl("daewoo", ripley_tvs$producto, ignore.case = T), "daewoo",ripley_tvs$marca)




ripley_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", ripley_tvs$producto)



ripley_tvs$pulgadas <- sub('"', "", ripley_tvs$pulgadas)
ripley_tvs$pulgadas <- sub("'", "", ripley_tvs$pulgadas)



### To lower 


ripley_tvs <- as.data.frame(apply(ripley_tvs[,],2,tolower))

ripley_tvs$pulgadas <- as.character(ripley_tvs$pulgadas)

ripley_tvs <- ripley_tvs[nchar(ripley_tvs$pulgadas)==2,]

ripley_tvs$pulgadas <- as.numeric(ripley_tvs$pulgadas)






##### Dif de precios
### Se har� cuando se junten los datos de los 3 ecommerce

# ripley_tvs <- ripley_tvs %>%
#         mutate(dif.precios = precio_antes - precio_actual)
#         
# 
# 
# ripley_tvs$dif.precios <- ifelse(is.na(ripley_tvs$dif.precios), 0, ripley_tvs$dif.precios)




### Ordenar columnas

ripley_tvs <- ripley_tvs[,c(1,5,2,3,4,6)]

###

str(ripley_tvs)


file <- paste(as.character(Sys.Date()), "ripley-tvs", sep = "-")

ripley_tvs_csv <- paste(file, "csv", sep = ".")


write.csv(ripley_tvs, ripley_tvs_csv, row.names = F)

