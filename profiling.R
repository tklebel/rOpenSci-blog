library(jstor)
library(microbenchmark)
library(tidyverse)
library(xml2)
source("old_functions.R")

jst_example("sample_with_references.xml") %>% 
  read_xml() %>% 
  as_list() %>% 
  find_meta()


microbenchmark::microbenchmark(
  old = jst_example("sample_with_references.xml") %>% 
    read_xml() %>% 
    as_list() %>% 
    find_meta(),
  new = jst_get_article(jst_example("sample_with_references.xml"))
)



profvis::profvis({
  rerun(50,
        jst_get_article(jst_example("sample_with_references.xml")))
                  })
  
  
