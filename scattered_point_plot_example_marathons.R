library(tidyverse)
library(ggbeeswarm)
file_name <- file.choose()
df1 <- read.csv(file_name, header = TRUE, stringsAsFactors = FALSE)
# aggregate M and F to a new category called Gender
df1$Gender <- ifelse(startsWith(df1$Category,"F"),"F","M")
# format Date column to POSIXct
df1$Time <- as.POSIXct(strptime(df1$Time, format = "%H:%M:%S"))
# if Time doesn't exist, use Net.Time
df1$Time <- as.POSIXct(strptime(df1$Net.Time, format = "%H:%M:%S"))
orig_var <- as.POSIXct("00:00:00", format = "%H:%M:%S")

p1 <- ggplot( data = df1, aes(x = Category,y = Time, color = Category)) + 
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun = mean, geom = "point", size=2, colour = "black", alpha = 0.5) +
  scale_y_datetime(date_labels = "%H:%M:%S", limits = c(orig_var,NA)) +
  theme(legend.position = "none")
# instead of finishing time, let's look at pace (min/km)
df1$Pace <- as.numeric(difftime(df1$Time, orig_var) / 21.1) * 3600
df1$Pace <- as.POSIXct(df1$Pace, origin = orig_var, format = "%H:%M:%S")

p2 <- ggplot( data = df1, aes(x = Category,y = Pace, color = Category)) + 
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun = mean, geom = "point", size=2, colour = "black", alpha = 0.5) +
  scale_y_datetime(date_labels = "%M:%S", limits = c(orig_var,NA)) +
  theme(legend.position = "none")
# calculate speeds rather than pace
df1$Speed <- 21.1 / as.numeric(difftime(df1$Time, orig_var))

p3 <- ggplot( data = df1, aes(x = Category, y = Speed, color = Category)) + 
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun = mean, geom = "point", size=2, colour = "black", alpha = 0.5) +
  ylim(0,NA) + ylab("Speed (km/h)") +
  theme(legend.position = "none")
# now make the same plots but by Gender rather than Category

p4 <- ggplot( data = df1, aes(x = Gender,y = Time, color = Gender)) + 
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun = mean, geom = "point", size=2, colour = "black", alpha = 0.5) +
  scale_y_datetime(date_labels = "%H:%M:%S", limits = c(orig_var,NA)) +
  theme(legend.position = "none")

p5 <- ggplot( data = df1, aes(x = Gender,y = Pace, color = Gender)) + 
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun = mean, geom = "point", size=2, colour = "black", alpha = 0.5) +
  scale_y_datetime(date_labels = "%M:%S", limits = c(orig_var,NA)) +
  theme(legend.position = "none")

p6 <- ggplot( data = df1, aes(x = Gender, y = Speed, color = Gender)) + 
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun = mean, geom = "point", size=2, colour = "black", alpha = 0.5) +
  ylim(0,NA) + ylab("Speed (km/h)") +
  theme(legend.position = "none")