library(officer)
library(magrittr)

emptyPres <- read_pptx()

slide1 <- emptyPres %>%
  add_slide(layout = "Title and Content", master = "Office Theme")

slide1 <- slide1 %>% 
  ph_with_text(type = "title", str = "A title") %>%
  ph_with_text(type = "ftr", str = "A footer") %>%
  ph_with_text(type = "dt", str = format(Sys.Date())) %>%
  ph_with_text(type = "sldNum", str = "slide 1") %>%
  ph_with_text(str = "Hello world", type = "body")

print(slide1, target = 'test.pptx')