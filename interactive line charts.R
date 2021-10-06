#options(repos = getOption("repos")["CRAN"])
install.packages("pacman")
pacman::p_load(tidyverse,echarts4r,nycflights13)
#echarts4r::e_common(font_family = "helvetica", theme = "westeros")
pacman::p_load(webshot2,htmlwidgets)
remotes::install_github("rstudio/webshot2")
library(webshot2)

#### Soluções interessantes que aprendi/revisei ####
#transmutar dataset altamente granular para semanal
#echarts4r
#match de string com várias possibilidades
#pivot longer
#as.Date
#signif
#salvando ggplots em PDF em alta qualidade com CairoPDF


#### Flights 2013 per airport ####
flights_ts <- nycflights13::flights %>% 
  transmute(week = as.Date(cut(time_hour, "week")), dep_delay, origin) %>% 
  group_by(origin, week) %>% # works with echarts
  summarise(dep_delay = sum(dep_delay, na.rm = TRUE))

flights_ts %>% 
  e_charts(x = week) %>% 
  e_datazoom(
    type = "slider", 
    toolbox = FALSE,
    bottom = -5
  ) %>% 
  e_tooltip() %>% 
  e_title("Departure delays by airport") %>% 
  e_x_axis(week, axisPointer = list(show = TRUE)) %>% e_line(dep_delay)

#### Real Estate #####
#Extraído de https://www.zillow.com/research/data/
real_estate = read.csv("data/imob.csv")
head(real_estate)
real_estate = real_estate %>% filter(stringr::str_detect(RegionName,"San Francisco|New York|Los Angeles")) %>%
  select(-c(RegionID,SizeRank,RegionType,StateName)) %>% 
  pivot_longer(!RegionName, names_to = "date", values_to = "count") %>% 
  mutate(date = as.Date(str_replace(date,"X",""), "%Y.%m.%d"),
         RegionName  = as.factor(RegionName))

real_estate %>% 
  group_by(RegionName) %>% 
  e_charts(x = date) %>% 
  e_datazoom(
    type = "slider", 
    toolbox = FALSE,
    bottom = -5
  ) %>% 
  e_tooltip() %>% 
  e_legend(right = 0) %>%
  e_title("Count of something related to real estate") %>% 
  e_x_axis(date, axisPointer = list(show = TRUE)) %>% e_line(count)


#### GDP  ####
gdp = readxl::read_xls("data/gdp.xls") %>% janitor::clean_names()

gdp = gdp %>% filter(stringr::str_detect(country_name,"Portugal|Brazil"))%>%
  select(-c(2,3,4)) %>% 
  pivot_longer(!country_name,names_to = "year",values_to="gdp") %>% 
  mutate(year = as.Date(str_replace(year,"x",""),"%Y")) %>% select(1,year,gdp)

gdp %>% 
  group_by(country_name) %>% 
  e_charts(x = year) %>% 
  e_datazoom(
    type = "slider", 
    toolbox = FALSE,
    bottom = -5
  ) %>% 
  e_tooltip() %>% 
  e_legend(right = 0) %>%
  e_title("GDP Portugal x Brazil") %>% 
  e_x_axis(year, axisPointer = list(show = TRUE)) %>% e_line(gdp)

#### GDP Per Capita - Brazil x Portugal####
gdppercap = readxl::read_xls("data/gdppercapita.xls") %>% janitor::clean_names()

gdppercap = gdppercap %>% filter(stringr::str_detect(country_name,"Portugal|Brazil"))%>%
  select(-c(2,3,4)) %>% 
  pivot_longer(!country_name,names_to = "year",values_to="gdp") %>% 
  mutate(year = as.Date(str_replace(year,"x",""),"%Y")) %>% select(1,year,gdp)

gdpercapPlot = gdppercap %>% 
  group_by(country_name) %>% 
  arrange(desc(country_name)) %>% 
  mutate(gdp = signif(gdp,3)) %>% 
  e_charts(x = year) %>% 
  #e_datazoom(type = "slider", toolbox = FALSE,bottom = 10) %>% 
  #%>% e_toolbox_feature(feature = "saveAsImage")
  e_tooltip(#trigger = "axis",
            order ='valueDesc'
            #formatter = e_tooltip_item_formatter("currency")
            ) %>% 
  e_title("Comparison of GDP per capita - Portugal x Brazil","Similar growth trends but different scales- Source: World Bank") %>% 
  e_x_axis(year, axisPointer = list(show = TRUE)) %>% 
    e_line(gdp, smooth = TRUE) %>% 
  e_legend(right = 0) %>% 
  e_theme("chalk") %>%
  e_color(c("gold","red")) %>% 
  e_y_axis(
    formatter = e_axis_formatter("currency", currency = "USD")
  )%>%
  e_animation(show = FALSE) 
gdpercapPlot
htmlwidgets::saveWidget(widget = gdpercapPlot, file = "out/gdpercapPlot.html",title = "Comparison of GDP per capita - Portugal x Brazil")
webshot('out/gdpercapPlot.html', file = 'out/teste.png')

#### GDP per capita in some more countries - per decade ####
pacman::p_load(CGPfunctions,janitor)
pacman::p_load(ggthemes,hrbrthemes)
countries = c("Portugal","Brazil","Italy","Argentina","United States","Canada","Switzerland")

morecountries = readxl::read_xls("data/gdppercapita.xls") %>% janitor::clean_names() %>%
  filter(stringr::str_detect(country_name,paste(countries, collapse = "|")))%>%
  select(-c(2,3,4)) %>%
  pivot_longer(!country_name,names_to = "year",values_to="gdp") %>%
  mutate(year = as.Date(str_replace(year,"x",""),"%Y")) %>% select(1,year,gdp)

colors <- c("Argentina" = "gray", "Canada" = "gray",
            "Italy" = "gray", "United States" = "gray",
            "Portugal" = "red", "Switzerland" = "gray",
            "Brazil" = "gold")

morecountries = morecountries %>% mutate(year = format(year,"%Y")) %>% filter(year %in% c(1990,2000,2010,2020)) %>% 
  mutate(gdp = signif(gdp, 2))

install.packages("devtools")
library(devtools)
devtools::install_github('Mikata-Project/ggthemr')
library(ggthemr)
pacman::p_load(Cairo)

newggslopegraph(dataframe = morecountries,
                Times = year,
                Measurement = gdp,
                Grouping = country_name,Title = "Evolution of GDP per capita",
                SubTitle = "4 past decades - Source: World Bank",
                Caption = "By Rafael Belokurows",
                LineThickness = 2,
                LineColor = colors,
                XTextSize = 12,    # Size of the times
                YTextSize = 3.5,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "center",
                SubTitleJustify = "center",
                CaptionJustify = "left",
                DataTextSize = 3.5,
                DataLabelFillColor = "#F0F0F0",
                #ThemeChoice = "ipsum",
                WiderLabels = T) + 
  theme_fivethirtyeight() +
  theme(legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

ggsave("out/evolution of gdp per capita.pdf", width = 30, height = 20, units = "cm",device=cairo_pdf)

rough90s = morecountries %>%  filter(year %in% c(1990,2000))

custom_colors <- rough90s %>% 
  group_by(country_name) %>% 
  mutate(Diff = gdp - lead(gdp)) %>% 
  mutate(trend = case_when(
    Diff > 0 ~ "red3",
    Diff < 0 ~ "#009C3B",
    TRUE ~ "gold"
  )
  ) %>%
  select(country_name, trend) %>% slice(1) %>%
  tibble::deframe()

newggslopegraph(dataframe = rough90s,
                Times = year,
                Measurement = gdp,
                Grouping = country_name,Title = "Change in GDP per capita in the last decade",
                SubTitle = "",
                Caption = "By Rafael Belokurows",
                LineThickness = 2,
                LineColor = custom_colors,
                XTextSize = 12,    # Size of the times
                YTextSize = 3.5,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "center",
                SubTitleJustify = "center",
                CaptionJustify = "left",
                DataTextSize = 3.5,
                DataLabelFillColor = "#F0F0F0",
                #ThemeChoice = "ipsum",
                WiderLabels = T) + 
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
  )
ggsave("out/gdp per capita 1990 - 2000.pdf", width = 30, height = 20, units = "cm",device=cairo_pdf)

lastdecade = morecountries %>%  filter(year %in% c(2010,2020))

custom_colors <- lastdecade %>% 
  group_by(country_name) %>% 
  mutate(Diff = gdp - lead(gdp)) %>% 
  mutate(trend = case_when(
    Diff > 0 ~ "red3",
    Diff < 0 ~ "#009C3B",
    TRUE ~ "gold"
  )
  ) %>%
  select(country_name, trend) %>% slice(1) %>%
  tibble::deframe()

newggslopegraph(dataframe = lastdecade,
                Times = year,
                Measurement = gdp,
                Grouping = country_name,Title = "Change in GDP per capita in the last decade",
                SubTitle = "",
                Caption = "By Rafael Belokurows",
                LineThickness = 2,
                LineColor = custom_colors,
                XTextSize = 12,    # Size of the times
                YTextSize = 3.5,     # Size of the groups
                TitleTextSize = 14,
                SubTitleTextSize = 12,
                CaptionTextSize = 10,
                TitleJustify = "center",
                SubTitleJustify = "center",
                CaptionJustify = "left",
                DataTextSize = 3.5,
                DataLabelFillColor = "#F0F0F0",
                #ThemeChoice = "ipsum",
                WiderLabels = T) + 
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
  )

ggsave("out/gdp per capita 2010-2020.pdf", width = 30, height = 20, units = "cm",device=cairo_pdf)


#### Tidy Tuesday 05/10 - geofacetting ####

tuesdata <- tidytuesdayR::tt_load('2021-10-05')
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

nurses <- tuesdata$nurses

# Or read in the data manually
#https://www.r-graph-gallery.com/web-time-series-and-facetting.html
#https://cran.r-project.org/web/packages/geofacet/vignettes/geofacet.html
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')
pacman::p_load(geofacet,ggh4x,colorspace)
states = state_ranks %>%  distinct(state,name) %>% rename("abb" = "state","state" = "name" )

nurses = nurses %>% select(State,Year,`Location Quotient`) %>% janitor::clean_names() %>% 
  left_join(states) 



ggplot(state_unemp, aes(year, rate)) +
  geom_line() +
  facet_geo(~ state, grid = "us_state_grid2", label = "name") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  labs(title = "Seasonally Adjusted US Unemployment Rate 2000-2016",
       caption = "Data Source: bls.gov",
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme(strip.text.x = element_text(size = 6))

nurses %>% tidyr::drop_na() %>% 
ggplot(aes(year, location_quotient)) +
  geom_line(color = "steelblue") +
  facet_geo(~ abb, grid = "us_state_grid2", label = "name") +
  #scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  labs(title = "Seasonally Adjusted US Unemployment Rate 2000-2016",
       caption = "Data Source: bls.gov",
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme(strip.text.x = element_text(size = 6)) +
  theme_bw()


ggplot(eu_gdp, aes(year, gdp_pc)) +
  geom_line(color = "steelblue") +
  facet_geo(~ name, grid = "eu_grid1", scales = "free_y") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  ylab("GDP Per Capita in Relation to EU Index (100)") +
  theme_bw()
