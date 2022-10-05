# R TIPS ----
# TIP 034 | mmtable2: ggplot2 syntax for tables ----
#
# 👉 For Weekly R-Tips, Sign Up Here: https://learn.business-science.io/r-tips-newsletter

# LIBRARIES ----

# remotes::install_github("ianmoran11/mmtable2")

devtools::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(gt)
library(tidyverse)

mpg


# 1.0 Data Transformation -----

data_wrangled <- mpg %>%
  group_by(manufacturer, cyl) %>%
  summarise(across(.cols = c(cty, hwy), .fns = mean)) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(cty, hwy),
    names_to = "fuel_economy_type",
    values_to = "fuel_economy"
  )

data_wrangled


# 2.0 Table Main ----
main_table <- data_wrangled %>%
  mutate(fuel_economy = round(fuel_economy, 1)) %>%
  
  mmtable(cells = fuel_economy, table_name = "Fuel Economy") +
  
  # Specify Headers
  header_top(manufacturer) +
  header_left(cyl) +
  header_left_top(fuel_economy_type) +
  
  
  # Specify formatting
  header_format(manufacturer, list(cell_text(transform = "capitalize"))) +
  header_format(fuel_economy_type, list(cell_text(transform = "uppercase"))) +
  table_format(
    locations = list(
      cells_body(rows = c(2, 6))
    ),
    style     = list(
      cell_borders(sides = "top", color = "grey")
    )
  )

# 3.0 Modify with GT Table ----
main_table %>%
  gt::tab_header(
    title = "Fuel Economy by Car Manufacturer",
    subtitle = "Audi, VW, and Honda are leaders in Fuel Economy"
  )


#From https://ianmoran11.github.io/mmtable2/articles/Some-minimal-examples.html 
library(gapminder)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(gt)
library(mmtable2)

#Adding headers
gm_df <- gapminder_mm %>% filter(var != "Life expectancy")
style_list <- list(cell_borders(sides = "top",color = "grey"))

gm_table <- 
  gm_df %>% 
  mmtable(cells = value) +
  header_left(year) +
  header_top(country) +
  header_left_top(var)  +
  header_top_left(continent) + 
  header_format(var, scope = "table", style = style_list)

print(gm_table)

#Combining tables
##Side by side
ex1 <- t1 + t2
print(ex1)

##On top of one another
ex2 <- t1 / t3
print(ex2)

##Integrating tables
ex3 <- t1 * t3 * t4 *  t2
print(ex3)

##Formatting tables
gm_table_formatted <- 
  gapminder_mm %>% 
  filter(var != "Life expectancy") %>% 
  mmtable(cells = value) +
  header_top(year) +
  header_left(country) +
  header_top_left(var)  +
  header_left_top(continent)  +
  cells_format(cell_predicate = T, style = list(cell_text(align = "right"))) +
  header_format(header = year, style = list(cell_text(align = "right"))) +
  header_format("all_cols", style = list(cell_text(weight = "bolder"))) +
  header_format("all_rows", style = list(cell_text(weight = "bolder"))) +
  header_format(continent, scope= "table", 
                style = list(cell_borders(sides = "top",color = "grey"))) 

print(gm_table_formatted)

#Merged header columns
row_list <- cells_body(rows = c(1,5))
col_list <- cells_body(columns = c(3,5,7,9,11))
style_list <- list(cell_borders(sides = "top",color = "grey"))
style_list2<- list(cell_borders(sides = "left",color = "grey"))
gm_df <- gapminder_mm %>% filter(var != "Life expectancy")
style_list3 = list(cell_text(align = "left"))

gm_table_merged <- 
  gm_df %>% 
  mmtable(cells = value) +
  header_left(year) +
  header_top(country) +
  header_left_top(var)  +
  header_top_left(continent)  +
  header_format(continent,style_list3 ) +
  header_merged_cols()

print(gm_table_merged)

#Alternative pipe syntax: adding the add_ prefix to functions allows use of %>% in place of +
gm_table_piped <- 
  gapminder_mm %>% 
  filter(var != "Life expectancy") %>% 
  mmtable(cells = value, use_default_formats = T) %>% 
  add_header_top(year) %>% 
  add_header_left(country) %>% 
  add_header_top_left(var)  %>% 
  add_header_left_top(continent)  %>% 
  add_cells_format(cell_predicate = T, style = list(cell_text(align = "right"))) %>% 
  add_header_format(header = year, style = list(cell_text(align = "right"))) %>% 
  add_header_format("all_cols", style = list(cell_text(weight = "bolder"))) %>% 
  add_header_format("all_rows", style = list(cell_text(weight = "bolder"))) %>% 
  add_header_format(continent, scope= "table", 
                    style = list(cell_borders(sides = "top",color = "grey"))) 

print(gm_table_piped)
