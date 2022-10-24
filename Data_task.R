# Prepare ####

#Libraries
if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')# for Data Wrangling and visualization
if(!require(ggrepel)) install.packages('ggrepel', repos = 'http://cran.us.r-project.org')# for labels
if(!require(png)) install.packages('png', repos = 'http://cran.us.r-project.org') #for read PNG
if(!require(grid)) install.packages('grid', repos = 'http://cran.us.r-project.org') # for render bitmap image like PNG

#load the dataset
data_task_oct22 <- read.csv('https://raw.githubusercontent.com/joaquinloncon/data_task/main/data_task_oct22.csv')

#Create a custom theme
my_theme <- function(
        
    #FACET TITLES OPTIONS
    strip.background = element_rect(fill = 'black',
                                    colour = 'lightblue',
                                    size = 1,
                                    linetype = 'solid'), # Box with group names when facet_wrap
    
    strip.text = element_text(size = 12,
                              face = 'bold'), # text of facet
    
    #TITLE OPTIONS
    title = element_text(color = 'lightblue',
                         size = 15,
                         face = 'bold'), 
    
    #AXIS OPTIONS
    axis.title = element_text(color = 'lightblue',
                              size = 12,
                              face = 'bold'), 
    
    axis.text = element_text(color = 'lightblue',
                             size = 12,
                             face = 'bold'), 
    
    axis.line = element_line(color = 'lightblue'), 
    axis.ticks = element_line(color = 'lightblue'), 
    axis.text.x = element_text(), 
    axis.text.y = element_text(), 
    
    #GRID OPTIONS
    panel.grid.minor.x = element_blank(), #Hide the vertical gridlines
    panel.grid.major.x = element_line('gray36'), 
    panel.grid.minor.y = element_line('gray26'), #colour the horizontal  gridlines
    panel.grid.major.y = element_line('gray26'), 
    panel.background = element_rect(fill = 'gray7'),
    plot.background = element_rect(fill = 'gray4'),
    
    #LEGEND OPTIONS
    legend.background = element_rect(fill = 'black',
                                     linetype = 'solid',
                                     colour  = 'lightblue',
                                     size = 1), 
    
    legend.text = element_text(color = 'lightblue',
                               size = 10,
                               face = 'bold'), 
    
    legend.key = element_rect(fill = 'black',
                              color = NA), 
    
    legend.position = c(.2, .8)
){    
    theme(
        #FACET TITLES OPTIONS        
        strip.background = strip.background, 
        strip.text = strip.text, 
        
        #TITLE OPTIONS
        title = title, 
        
        #AXIS OPTIONS
        axis.title = axis.title, 
        axis.text = axis.text, 
        axis.line = axis.line, 
        axis.ticks = axis.ticks, 
        axis.text.x = axis.text.x, 
        axis.text.y = axis.text.y, 
        
        #GRID OPTIONS
        panel.grid.minor.x = panel.grid.minor.x, 
        panel.grid.major.x = panel.grid.major.x, 
        panel.grid.minor.y = panel.grid.minor.y, 
        panel.grid.major.y = panel.grid.major.y, 
        panel.background = panel.background, 
        plot.background = plot.background, 
        
        #LEGEND OPTIONS
        legend.background = legend.background, 
        legend.text = legend.text, 
        legend.key = legend.key, 
        legend.position = legend.position)
}

#change local sys to get month and day of the week in English
my_sys <- Sys.getlocale('LC_TIME') #save local system
Sys.setlocale('LC_TIME', 'English') #change it to English 

# Pre-Process ####
str(data_task_oct22)

data_task_oct22$date <-  as.Date(data_task_oct22$date) #chr to Date
data_task_oct22$package <-  factor(data_task_oct22$package) #chr to Factor

# Check for NAs
sum(is.na(data_task_oct22)) #We have Nas, but there are few of them, only 44

data_task_oct22[!complete.cases(data_task_oct22), ]

# Analyse the Nas
data_task_oct22 %>% 
    group_by(package) %>% 
    summarise(Na_percentage = sum(is.na(count))/length(count)*100)

# Since there are few of them for every package and for simplicity I'll remove them
data_task_oct22 <- data_task_oct22 %>% 
    filter(!is.na(count))

# Check for duplicated
sum(duplicated(data_task_oct22)) #no duplicated

# Check for omitted dates
dates_in_data <- data_task_oct22$date
date_range <- seq(min(dates_in_data), max(dates_in_data), by = 1) 
date_range[!date_range %in% dates_in_data]
# We have a week in July for which we implicitly do not have data

#use the same colours for each package
length(unique(data_task_oct22$package)) #16 packages, need 16 colours
mycolours <- c('red', 'cyan', 'green', 'orange', 
               'yellow', 'white', 'pink', 'darkturquoise', 
               'firebrick1', 'gold', 'lightskyblue', 'darkviolet', 
               'darkslategray1', 'darkorange', 'chartreuse', 'mediumorchid1')

# Aggregation & visualization ####

# create a top of the most downloaded package in 2022 to sort the plots
top <- data_task_oct22 %>%
    group_by(package) %>% 
    summarise(cummulative = sum(count)) %>% 
    arrange(desc(cummulative))

head(top)

# first glance to get a general idea of the data
# plot
data_task_oct22 %>% 
    mutate(package = factor(package, levels = top$package)) %>% #sort by top downloaded packages
    ggplot(aes(date, count, col = package))+
    geom_line(alpha = .3)+
    geom_smooth(se = F, span = .1, size = .5)+
    scale_x_date(date_labels = '%b', date_breaks = '1 month')+
    scale_color_manual(values = mycolours)+
    labs(title = '', x = '', y = 'Downloads')+
    my_theme(legend.position = 'none', 
             strip.text = element_text(color = 'lightblue', size = 10, face = 'bold'), 
             axis.text.y = element_text(size = 8, face = 'bold'), 
             axis.text.x = element_text(angle = 45, hjust = 1, size = 6))+
    facet_wrap(~package)

# Specific visualization of the evolution of download numbers for each package throughout the year
lapply(p <- top$package, function(p){
    
    p_mycolours <- mycolours[which(p  == top$package)] #get the same colours as before
    
    # plot
    data_task_oct22 %>% 
        filter(package %in% p) %>%
        ggplot(aes(date, count))+
        geom_line(aes(linetype = 'Downloads'), size = 1.3, alpha = .3, col = p_mycolours)+
        geom_smooth(aes(linetype = 'Trend'), method = 'loess', se = F, span = .1, size = 1.3, col = p_mycolours)+
        labs(title = paste0('Downloads of the ', p, ' package in 2022'), x = '', y = 'Downloads')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')+
        scale_linetype_manual(name = 'Data', labels = c('Downloads', 'Trend'), values = c(1, 3))+
        geom_label_repel(aes(date, count, label = ifelse(date  == last(date), count, NA)),
                         fontface = 'bold', fill = 'black', col = p_mycolours, size = 5, nudge_x = -10, min.segment.length = unit(0, 'lines'))+
        my_theme()
    
})

# Number of downloads of the shiny package per day of the week

#get the official logo
url <- "https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/shiny.png"
file <- tempfile() #create a temporary file
download.file(url, file, mode="wb") #download as .png (need wb to work)

image <- readPNG(file) #read the image

#change transparency (alpha)
img <- matrix(rgb(image[, , 1],#red
                  image[, , 2],#green
                  image[, , 3],#blue
                  image[, , 4] * 0.1),# alpha (0.-)
              nrow = dim(image)[1])

# plot
data_task_oct22 %>% 
    filter(package == 'shiny') %>% 
    mutate(wday = factor(weekdays(date), 
                         levels = c('Monday', 'Tuesday', 
                                    'Wednesday', 'Thursday', 
                                    'Friday', 'Saturday', 'Sunday'))) %>% #change levels to get the days sorted
    group_by(wday) %>% 
    summarise(average = mean(count), 
              maximum = max(count), 
              minimum = min(count)) %>% 
    ggplot()+
    annotation_custom(rasterGrob(img, just = 'center'))+
    geom_errorbar(aes(x = wday, y = average, ymin = minimum, ymax = maximum, col = 'Max & Min'), size = 1.5, alpha = 1)+
    geom_text(aes(x = wday, label = maximum, y = maximum), vjust = -.5, col = 'lightskyblue')+
    geom_text(aes(x = wday, label = minimum, y = minimum), vjust = 1.5, col = 'lightskyblue')+
    geom_label_repel(aes(x = wday, label = round(average), y = average),col = 'white', fill = 'black', hjust=0, direction="x")+
    geom_point(aes(x = wday, y = average, col = 'Average'), size = 5, shape = 18)+
    scale_color_manual(name = 'Statistic', labels = c('Average', 'Max & Min'), values = c('white', 'lightskyblue'))+
    labs(title = 'Shiny downloads per day of the week', x = '', y = 'Downloads')+
    my_theme(legend.position = c(0.8, 0.8), 
             panel.grid.minor.x = element_blank(), 
             panel.grid.major.x = element_blank(), 
             panel.grid.minor.y = element_blank(), 
             panel.grid.major.y = element_blank(), 
             axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = 'bold'))

Sys.setlocale('LC_TIME', my_sys) #come back to our locale system