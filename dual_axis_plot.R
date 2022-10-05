library(data.table)
library(lubridate)
library(ggplot2)
library(ggExtra)

darkblue <- "#092869"   #NHS SCOTLAND DARK BLUE
lightblue <- "#0391BF"   # NHS SCOTLAND LIGHT BLUE

# make the link dynamic
part1 <- "https://www.opendata.nhs.scot/dataset/"
part2 <- "b318bddf-a4dc-4262-971f-0ba329e09b87/"
part3 <- "resource/427f9a25-db22-4014-a3bc-893b68243055/"
part4 <- "download/trend_ca_"
part5 <- ".csv"
today <- gsub('-','',as.character(Sys.Date()))
link <- paste0(part1, part2, part3, part4, today, part5, sep = '')
dates <- seq.Date(as.Date(Sys.Date()-28), as.Date(Sys.Date() - 1), by = '1 day')
DT <- data.table::fread(link)
DT[, Date := lubridate::ymd(Date)]

positives <- DT[Date >= as.Date(Sys.Date() -28) & CAName == 'Glasgow City',
                .(Date, CAName, DailyPositive, 
                  FirstInfections, Reinfections)] # grab a handful of relevant columns

#fwrite(positives, 'positives.csv') # in case you want to come back to the same data later

positives[,PercentReinfections := (Reinfections/DailyPositive)]

#The basic col plot
p1 <- ggplot(positives, aes(Date, DailyPositive, fill = 'DailyPositive')) +
  geom_col() +
  geom_col(aes(Date, Reinfections, fill = 'Reinfections')) +
  theme_minimal() +
  ggExtra::removeGrid() +
  theme(legend.position = 'top',
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        legend.title = element_text(size = 11),
        plot.title = element_text(hjust = 0.0),
        plot.caption = element_text(hjust = 0),
        axis.title.y.right = element_text( angle = 90))
print(p1)

#Add the secondary axis 
p1 <- p1 +
  geom_line(mapping = aes(Date, PercentReinfections * 10000),
            colour = 'grey70', size = 1.2) +
  geom_point(mapping = aes(Date, PercentReinfections * 10000),
             colour = 'grey70', size = 2) +
  scale_x_date(breaks = '2 days', date_labels = '%d %b %y') +
  scale_y_continuous(sec.axis = ggplot2::sec_axis(~. / 10000,
                                                  name = "Percentage being reinfections",
                                                  labels = scales::label_percent()))

#Clean up the axis and shift colors
p1 <- p1 +
  scale_fill_manual(name = NULL,
                    guide = "legend",
                    values = c("DailyPositive" = lightblue,
                               "Reinfections" = darkblue,
                               "PercentReinfections" = "white"),
                    labels = c('Daily cases',
                               'Daily reinfections',
                               'Percent of reinfections'))

p1 <- p1 +
  guides(fill = guide_legend(override.aes = list(linetype = c(0, 0, 1),
                                                 shape = c(NA, NA, 16)
  ))) 

p1 <- p1 +  labs(x = "Date",
                 y = " Count of cases",
                 caption = 'Source: Public Health Scotland',
                 title = "28 Day Cases by episode and reinfection rate") +
  ggExtra::rotateTextX()

print(p1)
