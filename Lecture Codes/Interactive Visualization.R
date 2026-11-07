# Import data
cv_states <- read.csv( 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')

# format the date
cv_states$date <- as.Date(cv_states$date, format="%Y-%m-%d")

# create most recent day data frame
cv_states_today <- subset(cv_states, date==max(cv_states$date))

# Create Scatterplot
library(plotly)

cv_states_today |> 
  plot_ly(x = ~cases, y = ~deaths, 
          type = 'scatter',
          mode = 'markers',
          color = ~state,
          colors = "Blues") # shades of blue

# Adjust the size
cv_states_today |> 
  plot_ly(x = ~cases, y = ~deaths, 
          type = 'scatter',
          mode = 'markers',
          color = ~state,
          size = ~fips,
          sizes = c(5, 70),
          marker = list(sizemode='diameter', opacity=0.5))

# Hover Information 
cv_states_today |> 
  plot_ly(x = ~cases, y = ~deaths, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~fips, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste( paste(state, ":", sep=""), #(sep= - separation "", mean nothing will be separated)
                         paste(" Cases per 100k: ", cases, sep="") ,
                         paste(" Deaths per 100k: ", deaths, sep=""),
                         paste(" CFR (%): ", fips,sep=""),
                         sep = "<br>")) #(make a new line break, <br>: break)

# Layout Fxn- set labels
cv_states_today |> 
  plot_ly(x = ~cases, y = ~deaths, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~fips, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste( paste(state, ":", sep=""), #(sep= - separation "", mean nothing will be separated)
                         paste(" Cases per 100k: ", cases, sep="") ,
                         paste(" Deaths per 100k: ", deaths, sep=""),
                         paste(" CFR (%): ", fips,sep=""),
                         sep = "<br>")) |> 
  layout(title = "Cases, Deaths, and Naive Case Fatality Rate for US States",
         yaxis = list(title = "Cases"), 
         xaxis = list(title = "Deaths"),
         hovermode = "compare")

# 3D scatterplot
cv_states_today |> 
  plot_ly(x = ~cases, y = ~deaths, z = ~state,
          type = 'scatter3d', mode = 'markers', color = ~state,
          size = ~fips, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste( paste(state, ":", sep=""), 
                         paste(" Cases per 100k: ", cases, sep="") ,
                         paste(" Deaths per 100k: ", deaths, sep=""),
                         paste(" CFR (%): ", fips,sep=""), 
                         sep = "<br>")) |> 
  layout(title = "Cases, Deaths, and Naive Case Fatality Rate for US States",
         yaxis = list(title = "Cases"), 
         xaxis = list(title = "Deaths"),
         hovermode = "compare")

# Scatterplot with ggplotly
library(ggplot2)
p <- ggplot(cv_states_today, aes(x=deaths, y=cases, size=fips)) + 
  geom_point() + geom_smooth()
ggplotly(p)

# Add annotations (ISSUE)
p <- ggplot(cv_states_today, aes(x=deaths, y=cases, size=fips)) + 
  geom_point() + 
  geom_smooth() +
  annotate("text", 
           x = max(cv_states_today$deaths),
           y = max(cv_states_today$cases),
           label = paste('Cor =', cor(cv_states_today$deaths, cv_states_today$cases)))

# Line plot
cv_states |>  
  plot_ly(x = ~date, y = ~deaths, color = ~state, type = "scatter", mode = "lines",
          hoverinfo = 'text',
          text = ~paste(paste(state, ":", sep=""), 
                        paste("Date: ", date, sep=""),
                        paste(" Deaths (total): ", deaths, sep=""), 
                        paste(" Deaths per 100k: ", deaths, sep=""), sep = "<br>"))

# ggplot line plot
g1 = plot_ly(cv_states, x = ~date, y = ~deaths, color = ~state, type = "scatter", mode = "lines")
g2 = ggplot(cv_states, aes(x = date, y = cases, color = state)) +
  geom_line() + geom_point(size = .5, alpha = 0.5)
g2_plotly <- ggplotly(g2)
subplot(g1, g2_plotly)

# Creates line plot with hover fxn
g1 = ggplot(cv_states, aes(x = date, y = cases, color = state, 
                           text=paste(paste(state, ":", sep=""), # text= allows for hover fxn 
                                      paste("Date: ", date, sep=""),
                                      paste(" Cases (total): ", cases, sep=""),
                                      sep = "<br>") )) + 
  geom_line() + geom_point(size = .5, alpha = 0.5) 
ggplotly(g1, tooltip = "text")

# Creater histograms (ISSUE)
g1 <- cv_states_today |> 
  plot_ly(x = ~new_deaths, type = "histogram", xbins = list(size = 1, end=30 ))
g2 <- cv_states_today |> ggplot( aes(x=new_deaths)) + geom_histogram(binwidth=1)
g2_plotly <- ggplotly(g2)
subplot(g1, g2_plotly)

# Heat Maps 
library(tidyr)
cv_states_mat <- cv_states |> 
  select(state, date, deaths) |> 
  filter(date>"2020-03-31") |> 
  filter(deaths < 10)
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat,
                                            names_from = state,
                                            values_from = deaths))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
head(cv_states_mat2)

# produce actual heat map 
data <- as.matrix(cv_states_mat2)

plot_ly(x=colnames(data), y=rownames(data), # columns x rows y, z object itself (data)
        z=~data,
        type="heatmap", # actually create the heat map from the data table
        showscale=T) # legend 

# 3D surface (ISSUE)
plot_ly(x=colnames(cv_states_mat2), y=rownames(cv_states_mat2),
        z=~cv_states_mat2,
        type="surface",
        showscale=F)
data

# Color Maps
cv_CFR <- cv_states_today |> select(state, fips) # select data

# Get state abbreviations and map to state names
library(tidyverse)
st_crosswalk <- tibble(state = state.name) |>
  bind_cols(tibble(abb = state.abb)) |> 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))
cv_CFR2 <- left_join(cv_CFR, st_crosswalk, by = "state")
cv_CFR2$state.name <- cv_CFR2$state
cv_CFR2$state <- cv_CFR2$abb
cv_CFR2$abb <- NULL

# Create hover text (ISSUE)
cv_CFR2$hover <- with(cv_CFR, paste(state.name, '<br>', "CFR:", fips))

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Create the map
fig <- plot_geo(cv_CFR2, locationmode = 'USA-states') |> 
  add_trace(
    z = ~fips, 
    text = ~hover, 
    locations = ~state,
    color = ~fips, 
    colors = 'Blues'
  )
fig <- fig |> colorbar(title = "CFR")
fig <- fig |> layout(
  title = paste('CFR by State as of', Sys.Date(), '<br>(Hover for value)'),
  geo = set_map_details
)
fig

# Interactive tables 
library(DT)
cv_california <- cv_states |> filter(state=="California") |> select(date,cases,deaths,fips)
datatable(cv_california)

# save and embedd 
library(htmlwidgets)
p <- plot_ly(x = rnorm(100))
saveWidget(p, "p1.html", selfcontained = F, libdir = "lib")
saveWidget(p, "p2.html", selfcontained = F, libdir = "lib")
