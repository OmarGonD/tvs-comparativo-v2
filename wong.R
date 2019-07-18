library(httr)   # for `GET`
library(glue)   # for `glue`, which allows cleaner syntax than `paste0`
library(purrr)  # for `map_df` to map over list and return as dataframe
library(dplyr)  # for `bind_cols`

i <- 0
cont_list <- list()

# Send requests and append data `cont_list` until fewer than 50 items returned.
repeat {
  url <- glue("https://www.wong.pe/api/catalog_system/pub/products/search/",
              "?&fq=C:/1000144/1000098/&_from={i}&_to={i + 49}")
  cont <- content(GET(url))
  cont_list <- c(cont_list, cont)
  if (length(cont) < 50) break
  i <- i + 50
}

# Names of desired data.
datl <- list(l1 = c("brand", "productName"),
             l2 = c("Price", "ListPrice", "AvailableQuantity"))

# Extract data 
wong_tvs <- map_df(cont_list,
                 ~ bind_cols(source = "wong", .[datl$l1],
                             .$items[[1]]$sellers[[1]]$commertialOffer[datl$l2])) %>%
          set_names("ecommerce", "marca", "producto", "precio_actual", "precio_antes", "unidades")

wong_tvs <- tvs_df[, c(1,2,3,5,4)]

wong_tvs$pulgadas <- sub(".*?(\\d+['\"]).*", "\\1", wong_tvs$producto)

wong_tvs$pulgadas <- sub('"', "", wong_tvs$pulgadas)
wong_tvs$pulgadas <- sub("'", "", wong_tvs$pulgadas)

wong_tvs$pulgadas <- as.character(wong_tvs$pulgadas)


wong_tvs <- wong_tvs[nchar(wong_tvs$pulgadas)<=2,]


### To lower 


wong_tvs <- as.data.frame(apply(wong_tvs[,],2,tolower))

file <- paste(as.character(Sys.Date()), "wong-tvs", sep = "-")

wong_tvs_csv <- paste(file, "csv", sep = ".")


write.csv(wong_tvs, wong_tvs_csv, row.names = F)
