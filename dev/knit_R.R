


list.files(here::here())[grep(".Rmd", list.files(here::here()))] %>%
  sapply(., function(x) {
    knitr::purl(
      input = x,
      output = paste0("r_files/",x)
    )
  })

list.files(here::here("r_files"))[grep(".Rmd", list.files(here::here("r_files")))] %>% 
  sapply( function(x) {
    file.remove(paste0("r_files/",x))
  })
