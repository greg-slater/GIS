
# library(geojsonio)
# library(sp)
# library(methods)
# library(tidyverse)
# library(plotly)
# library(OpenStreetMap)
# library(ggmap)
# library(reshape2)

library(sf)
library(maptools)
library(tmap)
library(tmaptools)
library(ggplot2)
library(viridis)
library(tidyverse)

# read in data and reference tables, there are 227759 OAs in England, Wales & Scotland
data = read_csv('internet_use_esrc.csv')
oa_msoa = read_csv('oa_mapping.csv')
oa_region = read_csv('OA_to_region.csv')
msoa_cities <- read_csv('MSOA_to_cities.csv')
pop_data = read_csv('census_pop_data/census_pop_2011_all.csv')

head(data)

# join data to both reference tables for MSOA, region and cities, as well as population data 
data <- data %>% 
  left_join(oa_msoa, by=c('oa11'='OA11CD')) %>%
  left_join(oa_region, by=c('oa11'='OA11CD')) %>%
  left_join(pop_data, by=c('oa11'='oa_code')) %>%
  left_join(msoa_cities, by=c('MSOA11CD'= 'MSOA11CD'))

# get rid of columns we don't need
data <- select(data, -c(phhreadr, pmobmail2, pu_accmob))

head(data)

# check OA hasn't been duped at all by the left joins
sprintf('number of unique OAs: %s', n_distinct(data$oa11))

# group by MSOA by calculating internet users per OA, summing pop and internet users to MSOA,
# then calculating the % of internet users again. Keep region for further optional filtering
data_msoa <-  data %>% 
    filter(!is.na(RGN11NM)) %>%
    mutate(int_pop = pusenet*people) %>%
    group_by(MSOA11CD) %>%
    summarise(t_pop = sum(people), t_int_pop = sum(int_pop), pusenet_msoa = t_int_pop/t_pop, RGN11NM = max(RGN11NM), TCITY15NM=max(TCITY15NM)) #%>%
    # filter(RGN11NM == 'London')

summary(data_msoa)  
head(data_msoa)
# write_csv(data_msoa, 'internet_use_MSOA_level.csv')


# read in shape files
msoa_shp <- read_shape('shp_MSOA/Middle_Layer_Super_Output_Areas_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp')
reg_shp  <- read_shape('shp_regions/Regions_December_2016_Generalised_Clipped_Boundaries_in_England.shp')
countries_shp <- read_shape('shp_countries/Countries_December_2017_Generalised_Clipped_Boundaries_in_UK.shp')


# inner join msoa to data
msoa_shp_all <- msoa_shp %>% inner_join(data_msoa, by= c('msoa11cd'='MSOA11CD'))
head(msoa_shp)

# colour palette
pal <- viridisLite::magma(5, begin = 0.2, end = 1)

uk_internet <- tm_shape(msoa_shp_all) + tm_fill(col='pusenet_msoa', palette=pal, style='jenks', title='% of Internet Users') +
  tm_borders(col='white', alpha=0.1, lwd=1) +
  tm_style('gray', bg.color='#eeeeee') +
  tm_shape(reg_shp) + tm_borders(col='white', lwd=1, alpha=0.5) +
  tm_compass(position = c('right', 'top'), color.light = 'grey90', color.dark='grey20') +
  tm_scale_bar(position=c('center','bottom'), color.light = 'grey90', color.dark='grey20') +
  tm_layout(title='Digital Inequality in England', frame=FALSE,
            title.position = c("center", "top"), 
            legend.position = c("right", "bottom"),
            inner.margins = c(0.1, 0.1, 0.1, 0.1))

# uk_internet

# tmap_save(uk_internet, 'out_eng_internet_access.png', height=7)





# create list of cities we're interested in then create new table filtered by them
cities_of_interest <- c('London', 'Birmingham', 'Liverpool', 'Leeds', 'Sheffield', 'Bristol', 'Manchester', 'Leicester', 'Coventry')
msoa_cities <- filter(data_msoa, TCITY15NM %in% cities_of_interest)

# summarise internet scores to city level and create new place columns in output table
city_internet <- msoa_cities %>% 
  group_by(TCITY15NM) %>% 
  summarise(pop = sum(t_pop), int_pop = sum(t_int_pop), pusenet=int_pop/pop) %>%
  # mutate(x = 1)
  mutate(place_type = 'City') %>%
  rename(place = TCITY15NM)

head(city_internet)

# summarise to region, keeping output in same format as city summary
region_internet <- data_msoa %>% 
  group_by(RGN11NM) %>% 
  summarise(pop = sum(t_pop), int_pop = sum(t_int_pop), pusenet=int_pop/pop) %>%
  mutate(place_type = 'Region') %>%
  rename(place = RGN11NM)

head(region_internet)

# create new table from both tables above unioned together
place_scores <- bind_rows(city_internet, region_internet)

# theme for use in ggplot below
mytheme <- theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                 axis.line.x = element_blank(), legend.position='none', #axis.text.x = element_blank(), 
                 axis.title = element_blank(), panel.background = element_rect(fill="#eeeeee") )
                  #, plot.margin = unit(c(1, 4, 1, 4), "cm"))

cities_chart <- ggplot(place_scores, aes(place_type, pusenet, color=pusenet, label=place)) +
  geom_point(size=3.5) + ylim(0.6, 0.85) +
  geom_text(aes(label=place), hjust=0, nudge_x=0.03, size=3, color='black') +
  scale_color_viridis_c(option = "A", begin = 0.2, end = 1) +
  mytheme

cities_chart

ggsave('place_scores_chart.eps', plot=cities_chart, height=7, width=4.2, device=cairo_ps)
ggsave('place_scores_chart.png', plot=cities_chart, height=7)

# create a facet map of cities of interest

msoa_cities_shp <- msoa_shp %>% inner_join(msoa_cities, by= c('msoa11cd'='MSOA11CD'))

tm_shape(msoa_cities_shp) +
  tm_fill(col='pusenet_msoa', palette=pal, style='jenks', title='% of Internet Users') +
  tm_facets(by='TCITY15NM') + tm_style('gray')








cities_chart <- ggplot(city_internet) +
  aes(x, pusenet, color=pusenet, label=TCITY15NM) +
  geom_point(size=3) +
  geom_text(aes(label=TCITY15NM), hjust=0, nudge_x=0.1, size=3, color='black') +
  scale_color_viridis_c(option = "A", begin = 0.2, end = 1) +
  xlim(0.9,2) +
  mytheme

cities_chart

uk_internet
print(cities_chart, vp = grid::viewport(x=.3, y=.5, width=.2, height=.5))



print()
?scale_fill_viridis_c()

library(BAMMtools)
classes <- getJenksBreaks(data_msoa$pusenet_msoa, 6)
classes

findColours(classes, pal)

library(spatstat)
cmap <- colourmap(pal, breaks = classes)

cmap

