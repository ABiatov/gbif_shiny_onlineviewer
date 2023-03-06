# https://mran.microsoft.com/snapshot/2018-06-09/web/packages/officer/vignettes/word.html


library(magrittr)
library(officer)

my_doc <- read_docx() 
styles_info(my_doc)

### Let’s create an image from a plot ####
src <- tempfile(fileext = ".png")
png(filename = src, width = 5, height = 6, units = 'in', res = 300)
barplot(1:10, col = 1:10)
dev.off()

### add that image to the document ####

my_doc <- my_doc %>% 
  body_add_img(src = src, width = 5, height = 6, style = "centered") %>% 
  body_add_par("Hello world!", style = "Normal") %>% 
  body_add_par("", style = "Normal") %>% # blank paragraph
  body_add_table(iris, style = "table_template")

### Write the Word file ####

print(my_doc, target = "temp/example.docx")

## Example 2 ####

if( require("ggplot2") ){
  gg <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + 
    geom_point()
  
  read_docx() %>% 
    body_add_par(value = "Table of content", style = "heading 1") %>% 
    body_add_toc(level = 2) %>% 
    body_add_break() %>% 
    
    body_add_par(value = "dataset iris", style = "heading 2") %>% 
    body_add_table(value = head(iris), style = "table_template" ) %>% 
    
    body_add_par(value = "plot examples", style = "heading 1") %>% 
    body_add_gg(value = gg, style = "centered" ) %>% 
    
    print(target = "temp/body_add_demo.docx")
}
## [1] 


