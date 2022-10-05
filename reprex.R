library(tidyverse)
library(lubridate)

#Creating reprex with real data
Prod_data<- Production_master5[1:35,]%>%
  select(Artikel, Produktionsplats.x, Producerad_mangd, Size, Startad_dt, Avslutad_dt)
names(Prod_data)[1:6]<- c("Order", "Prod_line", "Produced", "Size", "Started", "Ended")
dput(Prod_data)  

Prod_data_ex<-structure(list(Order = c(27380, 27388, 27395, 27381, 27389, 27382, 
                                       27396, 27397, 27393, 27392, 27383, 27384, 27385, 27386, 27398, 
                                       27409, 27410, 27411, 27412, 27416, 27420, 27421, 27417, 27418, 
                                       27432, 27433, 27419, 27413, 27399, 27414, 27415, 27428, 27424, 
                                       27429, 27431), 
                             Prod_line = c("FL 5S", "FL 5", "FL 25", "FL 5S", 
                                           "FL 5", "FL 5S", "FL 25", "FL 5", "FL 5", "FL 5", "FL 5S", "FL 5S", 
                                           "FL 5S", "FL 5S", "FL 5", "FL 5S", "FL 5S", "FL 5S", "FL 5S", 
                                           "FL 25", "FL 5S", "FL 5S", "FL 25", "FL 25", "FL 5S", "FL 5S", 
                                           "FL 25", "FL 5S", "FL 5", "FL 5S", "FL 5S", "FL 5S", "FL 25", 
                                           "FL 5S", "FL 5"), 
                             Produced = c(5400, 6373, 1440, 6372, 864, 5400, 
                                          288, 1080, 864, 5402, 3240, 864, 5293, 2700, 11547, 4427, 3672, 
                                          864, 2651, 96, 648, 1620, 96, 480, 2160, 1363, 480, 4320, 11528, 
                                          4320, 864, 12745, 600, 3782, 6373), 
                             Size = c("005", "005", "025","005", "005", "005", "025", "005", "005", "005", "005", "005", 
                                      "005", "005", "001", "005", "005", "005", "005", "025", "005", 
                                      "005", "025", "025", "005", "005", "025", "005", "001", "005", 
                                      "005", "005", "025", "005", "005"), 
                             Started = structure(c(1587969512, 1587970869, 1587972010, 1587984935, 1587998820, 1587999421, 1588003819, 
                                                   1588004233, 1588008484, 1588013628, 1588014019, 1588022405, 1588024038, 
                                                   1588062901, 1588063616, 1588070291, 1588076547, 1588083066, 1588085684, 
                                                   1588090216, 1588093080, 1588094491, 1588094881, 1588100218, 1588101215, 
                                                   1588105405, 1588105729, 1588110352, 1588146549, 1588147618, 1588155560, 
                                                   1588157595, 1588172423, 1588179841, 1588180603), tzone = "UTC", class = c("POSIXct", 
                                                                                                                             "POSIXt")), Ended = structure(c(1587984935, 1587998820, 1588003819, 
                                                                                                                                                             1587999421, 1588004233, 1588014019, 1588090216, 1588008484, 1588013628, 
                                                                                                                                                             1588063616, 1588022405, 1588024038, 1588062901, 1588070291, 1588146549, 
                                                                                                                                                             1588076547, 1588083066, 1588085684, 1588093080, 1588094881, 1588094491, 
                                                                                                                                                             1588101215, 1588100218, 1588105729, 1588105405, 1588110352, 1588172423, 
                                                                                                                                                             1588147618, 1588180603, 1588155560, 1588157595, 1588179841, 1588188834, 
                                                                                                                                                             1588191295, 1588241708), tzone = "UTC", class = c("POSIXct", 
                                                                                                                                                                                                               "POSIXt"))), row.names = c(NA, -35L), class = c("tbl_df", "tbl", "data.frame"))

Prod_data_recpies<- Prod_data_ex%>%
  mutate(interval= interval(Started, Ended))%>%
  mutate(recipe= ifelse(Size=="001", map_int(interval,~ any(int_overlaps(.x, interval[Size=="001"]))),
                        ifelse(Size=="003", map_int(interval,~ any(int_overlaps(.x, interval[Size=="003"]))), "005")))


#Solution from Stackoverflow using data.table from @user12728748 
library(data.table)
dt <- as.data.table(Prod_data_ex)
dt[, Ended := Ended - 1] # prevent overlaps of consecutive time intervals
setkeyv(dt, c("Started", "Ended"))
dt[, Recipe := foverlaps(dt, dt[Prod_line == "FL 5"], type = "any", which = TRUE)[,
                                                                                  list(list(unique(yid[yid != xid]))), by = xid][, -1]]
dt$Recipe <- sapply(dt$Recipe, function(x) {
  paste(unique(dt[Prod_line == "FL 5"][x]$Size), collapse = ",")
})
dt[, Ended := Ended + 1][] # add back the second
