


list.files(here::here())[grep(".Rmd", list.files(here::here()))] %>%
  sapply(., function(x) {
    knitr::purl(
      input = x,
      output = paste0("r_files/",x)
    )
  })
