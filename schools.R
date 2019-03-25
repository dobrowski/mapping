
### Load Libraries ---------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,tidycensus
        ,tigris
        ,sf
        ,here
        ,mapview
        ,ggmap
        ,tmap
        ,ggrepel
        ,plotly
        ,ggthemes
        ,scales
        ,lubridate
        ,RColorBrewer
        ,viridis)

`%notin%` = Negate(`%in%`)

# census_api_key( "5a728953bcea6dd63adb8f4c700d3b46fe401371", install = TRUE )
register_google(key = 'AIzaSyAnhCxcUMakZdwXmSPwvJTqnFyfhPrwONM')

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


### Get the Shapefiles -------------

v15 <- load_variables(2017, "acs5", cache = TRUE)

View(v15)



schools <-read.delim(paste0("pubschls.txt"))

monterey_schools <- schools %>%
        filter(County == "Monterey",
               Latitude != "No Data",
               StatusType == "Active") %>% 
        select(NCESDist,NCESSchool ,CDSCode,District, School, GSserved, Latitude, Longitude) %>%
        mutate(GEOID = as.character(NCESDist),
               CDSCode = as.character(CDSCode),
               ncessch = str_c(NCESDist,NCESSchool),
               isdistrict = if_else(School == "No Data", 1,0)
               ) 



# 
# get_acs(geography = "school district (unified)", 
#         state = "CA",
#         year = 2017,
#         variables  = "B05006_001",
#         geometry = TRUE)



### Get local districts and bind together --------

districts_un <- school_districts(type = "unified" , state = "CA", year = 2018, class = "sf") %>%
        st_transform(4269) %>% rename(LEA = UNSDLEA)

districts_elem <- school_districts(type = "elementary" , state = "CA", year = 2018, class = "sf") %>%
        st_transform(4269) %>% rename(LEA = ELSDLEA)

districts_sec <- school_districts(type = "secondary" , state = "CA", year = 2018, class = "sf") %>%
        st_transform(4269) %>% rename(LEA = SCSDLEA)

districts <- districts_un %>%
        rbind(districts_elem) %>%
        rbind(districts_sec) %>%
        st_transform(4269)




### Get geography for tracts and county of monterey and only use the land areas  ------------

monterey_tracts <- tracts("CA", "Monterey", class = "sf") %>%
        filter(ALAND > 0) %>% 
        st_as_sf(monterey_tracts) %>%
        st_transform(4269) %>% st_union()


monterey_county <- counties("CA", class = "sf") %>%
        filter(NAME == "Monterey")  %>%
        st_transform(4269)



### Establish local school boundaries shapefiles -------

# From National Center for Education Statistics, School Attendance Boundary Survey
# https://nces.ed.gov/programs/edge/SABS
#  Also relevant information for school lat/lon:  https://nces.ed.gov/programs/edge/Geographic/SchoolLocations
primary <- st_read("SABS_1516/SABS_1516_Primary.shp")


middle <- st_read("SABS_1516/SABS_1516_Middle.shp")


primary.ca <- primary %>%
        filter(stAbbrev == "CA") %>%
        st_transform(4269)

middle.ca <- middle %>%
        filter(stAbbrev == "CA") %>%
        st_transform(4269)

monterey_primary <- primary.ca %>%
        st_intersection(monterey_tracts)

monterey_middle <- middle.ca %>%
        st_intersection(monterey_tracts)

monterey_primary_middle <- monterey_primary %>% rbind(monterey_middle)








# convert schools to coordinate points ----------
coords_sf <- schools %>%
        filter(County == "Monterey"
               ,StatusType == "Active"
               ,!str_detect(School, "Uplift")
               ) %>%
        mutate(Longitude = as.numeric(as.character(Longitude)),
               Latitude = as.numeric(as.character( Latitude))) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs= 4269) 


plot(coords_sf)


#  plot points of schools within the districts ----------
gg <- ggplot() +
        geom_sf(data=monterey_tracts, color = "black", fill=NA, size=0.5) +
        geom_sf(data=coords_sf, size=1, aes(color="red")) +
        labs(x=NULL, y=NULL, 
             title="title",
             subtitle=today(),
             caption="Source: ") +
        theme(plot.title=element_text(face="bold", family="Arial", size=13)) +
        theme(plot.caption=element_text(face="bold",
                                        family="Arial",
                                        size=7,
                                        color="gray",
                                        margin=margin(t=10, r=80))) +
        theme(legend.position="none") +
        theme_bw() +
        coord_sf(xlim = c(-120.2, -122.0), ylim = c(35.8, 36.9), datum = NA) 

gg



### Only districts within Monterey County ----------
# This line is all districts that are partially in Monterey County, so lots of districts that are mostly in SLO or Santa Cruz, etc.
# monterey_districts <-  districts %>%  st_intersection(monterey_tracts)  


monterey_districts <- districts[st_contains(monterey_tracts, districts, sparse = FALSE),] %>%
        rbind(districts %>% filter(str_detect(NAME, "South Monter") )  ) %>%
        st_intersection(monterey_tracts) %>%
        filter(FUNCSTAT == "E") %>%  
        mutate(elem = if_else(LOGRADE == "KG",1,0),
               high = if_else(HIGRADE == "12",1,0) ) %>% 
        st_collection_extract("POLYGON")  # To address error cause by two intersections 



###  Get the centers of the districts to label names -------
centroids <- st_centroid( monterey_districts)  

centroids.coords <- centroids %>% st_coordinates

centroids$long <- centroids.coords[,1]
centroids$lat <- centroids.coords[,2]



### Plot the map -----

## Using GGplot
gg <- ggplot() +
        geom_sf(data=monterey_tracts, color = "black", fill=NA, size=0.5) +
        geom_sf(data=monterey_districts, size=1, aes(color="red", alpha = .3 , fill = HIGRADE)) +
        geom_label_repel(data=centroids, size=3, aes(long,lat  ,label = NAME ) ,color="black") +
#      geom_point(monterey_districts, aes(x = lat , y = long, size = 4, shape = 23, fill = "darkred") ) +
        labs(x=NULL, y=NULL, 
             title="title",
             subtitle=today(),
             caption="Source: ") +
        theme(legend.position="none") +
        theme_bw() +
        coord_sf(xlim = c(-120.2, -122.0), ylim = c(35.8, 36.9), datum = NA) 

gg


## USing tmap ------
       
tmap_mode("view")

 tm_shape(monterey_districts %>% filter(elem == 1)) +
        tm_fill("HIGRADE", alpha = .5) +
        tm_borders() +
        tm_text("NAME", auto.placement = TRUE) +
 tm_shape(monterey_districts  %>% filter(high == 1)) +
         tm_fill("HIGRADE", alpha = .5) +
         tm_borders() +
         tm_text("NAME", auto.placement = TRUE) 

tmap_mode("plot")



### merge example ------

feeders <- read_csv("Feeder Districts Salinas UnionALL.csv") %>%
        mutate(CDSCode = as.character(CDSCode))

joint <- monterey_schools %>%
        left_join(feeders)

joint2 <- monterey_districts %>%
        left_join(joint %>% filter(isdistrict == 1)) %>%
        select(NAME,
               Chronic = ChronicAbsenteeismRate,
               ELA,
               Math,
               Suspension = `Suspension Rate (Total)`  ,
               elem,
               high,
               geometry)


tmap_mode("view")

map.ela <- tm_shape(joint2 %>% filter(elem == 1)) +
        tm_fill("ELA", alpha = .5, popup.vars = c("ELA", "Math", "Chronic", "Suspension")) +
        tm_borders() +
        tm_text("NAME", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.65 , lon = -121.6 ,  zoom = 11))

tmap_save(map.ela,  "map-ela.html")


map.math <- tm_shape(joint2 %>% filter(elem == 1)) +
        tm_fill("Math", alpha = .5, popup.vars = c("ELA", "Math", "Chronic", "Suspension")) +
        tm_borders() +
        tm_text("NAME", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.65 , lon = -121.6 ,  zoom = 11))

# Alternative dual map ... I think I prefer the facet version below
facet.maps <- tmap_arrange(map.ela, map.math, sync = TRUE, ncol = 2)

# tmap_save(facet.maps,  "map-facet.html")



map.facet <- tm_shape(joint2 %>% filter(elem == 1)) +
        tm_polygons(c("ELA", "Math")) +
        tm_borders() +
        tm_text("NAME", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.65 , lon = -121.6 ,  zoom = 10)) +
        tm_facets(sync = TRUE, ncol = 2)
# tm_facets(sync = TRUE, as.layers = TRUE)

tmap_save(map.facet,  "map-Salinas-district-facet.html")

#####




pal <- seq_gradient_pal("white", "red")(seq(0, 1, length.out = 30))
# pal <- gradient_n_pal(pal)(seq(0, 1, length.out = 30))

mapview(joiner, zcol = "HIGRADE", legend = TRUE, col.regions = pal)

map2 <- mapview(joiner, zcol = "n", legend = TRUE)

mapshot(map2, url = "calfresh2.html")







#### Old code ---------


# register_google(key = 'AIzaSyAnhCxcUMakZdwXmSPwvJTqnFyfhPrwONM')












###  Geocoding based on address --------------

# We need a single column for addresses,

calfresh_addresses_local2$location <- paste0(calfresh_addresses_local$HOME_STREET_NUM,
                                             calfresh_addresses_local$HOME_STREET_NAME)

calfresh_addresses_local3 <- calfresh_addresses_local %>%
        filter(!is.na(HOME_STREET_NAME)) %>%
        unite(col=location, sep = " ",  remove = FALSE,
              HOME_STREET_NUM,
              HOME_STREET_NAME, 
              HOME_STREET_SUFFIX, 
              HOME_CITY, 
              HOME_STA_CD, 
              HOME_ZIP)


# Got a list without duplicated addressed

sql_addresses <- calfresh_addresses_local3 %>% ungroup() %>% distinct(location)

# Split the list into batches smaller than 2500 to avoid limits, and then geocoded them;  change source to google as desired

sql_geo1 <- geocode(location = sql_addresses$location[1:2400], output="latlon", source="dsk")
sql_geo2 <- geocode(location = sql_addresses$location[2401:4800], output="latlon", source="dsk")
sql_geo3<- geocode(location = sql_addresses$location[4801:7200], output="latlon", source="dsk")
sql_geo4 <- geocode(location = sql_addresses$location[7201:9600], output="latlon", source="dsk")
sql_geo5 <- geocode(location = sql_addresses$location[9601:12000], output="latlon", source="dsk")
sql_geo6 <- geocode(location = sql_addresses$location[12001:14400], output="latlon", source="dsk")
sql_geo7 <- geocode(location = sql_addresses$location[14401:16660], output="latlon", source="dsk")


# Put the batches back together

sql_geo <- rbind(sql_geo1, sql_geo2, sql_geo3, sql_geo4, sql_geo5, sql_geo6, sql_geo7)


# Connected the geocoordinates to the addresses

sql_addresses_full <- cbind(sql_geo, sql_addresses)


# Removed PO Boxes, General Delivery and Homeless  addresses

sql_addresses_clean <- sql_addresses_full %>%
        mutate(lon= replace(lon, str_detect(location, "PO BOX")  ,NA)) %>%
        mutate(lon= replace(lon, str_detect(location, "Homeless")  ,NA)) %>%
        mutate(lon= replace(lon, str_detect(location, "General Delivery")  ,NA)) %>% 
        mutate(lat= replace(lat, str_detect(location, "PO BOX")  ,NA)) %>%
        mutate(lat= replace(lat, str_detect(location, "Homeless")  ,NA)) %>%
        mutate(lat= replace(lat, str_detect(location, "General Delivery")  ,NA))


# Joined addresses and lat lon back to the full set of people


calfresh_sql3 <- left_join(calfresh_sql2, sql_addresses_clean) %>%
        left_join(cwin_local) 





### Graphing the Data ------------


# Eliminating things not in Santa Cruz and then converting to the SF format

# locations_filtered <- filter(locations, Longitude < -120.5)

coords_sf <- calfresh_sql3 %>% filter(!is.na(lon)) %>%
        st_as_sf(coords = c("lon", "lat"), crs= 4269) 


plot(coords_sf)



# Plotting points with ggplot2

gg <- ggplot() +
        geom_sf(data=santacruz, color = "black", fill=NA, size=0.5) +
        geom_sf(data=coords_sf, size=1, aes(color="red")) +
        labs(x=NULL, y=NULL, 
             title="title",
             subtitle=today(),
             caption="Source: ") +
        theme(plot.title=element_text(face="bold", family="Arial", size=13)) +
        theme(plot.caption=element_text(face="bold",
                                        family="Arial",
                                        size=7,
                                        color="gray",
                                        margin=margin(t=10, r=80))) +
        theme(legend.position="none") +
        theme_bw() +
        coord_sf(xlim = c(-121.6, -122.3), ylim = c(36.8, 37.3), datum = NA) 

gg

# ggsave(file="points3.jpg", width=10, height=8)





####  Chloropleth of cases by census tract  --------------

# Calculating points in a polygon


joiner <- st_join(coords_sf, santacruz, join = st_intersects) %>%
        group_by(NAME) %>% count() %>% as.data.frame() %>% select(-geometry) %>%
        right_join(santacruz, by = "NAME") %>% st_as_sf()



# ggplot(joiner, aes(fill = n)) + geom_sf()


# pal <- inferno(20, direction = -1)
# pal <- brewer_pal(type = "seq", palette = "YlOrBr", direction = 1)
pal <- seq_gradient_pal("white", "red")(seq(0, 1, length.out = 30))
# pal <- gradient_n_pal(pal)(seq(0, 1, length.out = 30))

mapview(joiner, zcol = "n", legend = TRUE, col.regions = pal)

map2 <- mapview(joiner, zcol = "n", legend = TRUE)

mapshot(map2, url = "calfresh2.html")

joiner_simple <- joiner %>%  select(NAME, n, geometry)

st_write(joiner, "cf_complete.csv")
st_write(joiner, "cf_complete.shp")
st_write(joiner_simple, "cf_simple.csv")
st_write(joiner_simple, "cf_simple.shp")


name <- "CalFresh Cases by Age"
g <- ggplot(data = calfresh_sql5, aes(x = age, fill = child)) + 
        geom_bar(stat = "count") +
        labs(title = name, subtitle = subt , x = "" , y = "") +
        theme_hc()
ggplotly(g)


calfresh_sql5$ZIP2 <- as.factor(calfresh_sql5$ZIP)

CFgraph <- calfresh_sql5 %>% filter(ZIP > 90000)

g <- ggplot(data = calfresh_sql5, aes(x = age, fill = child)) + 
        geom_bar( ) +
        facet_wrap(~ ZIP2) +
        labs(title = name, subtitle = subt , x = "" , y = "") +
        theme_hc()
g


load_variables()




pop <- get_acs(table = "B01001", geography = "county", state = "CA") 

pop2<- pop %>% filter(GEOID == "06087") %>% 
        left_join( v15, by=c("variable" = "name")) %>%
        select(variable, estimate, moe, label) %>%
        mutate(label = sub('.*!!', '', label)) %>% 
        mutate(label = recode(label, 
                              "15 to 17 years" = "15 to 19 years",
                              "18 and 19 years" = "15 to 19 years",
                              "20 years" = "20 to 24 years",
                              "21 years" = "20 to 24 years",
                              "22 to 24 years"   = "20 to 24 years",
                              "60 and 61 years" = "60 to 64 years",
                              "62 to 64 years" = "60 to 64 years",
                              "65 and 66 years"  = "65 to 69 years",
                              "67 to 69 years"   = "65 to 69 years"
        )) %>%
        group_by(label) %>% 
        summarise(total = sum(estimate)) 



B17017_002

B17020_001

C17002_002  table






pov1 <- get_acs(table = "B17020", geography = "tract", county = "Santa Cruz", state = "CA", cache_table = TRUE)%>% 
        left_join( v15, by=c("variable" = "name")) %>%
        filter(variable == "B17020_002") %>%
        select(-NAME)


Below_Poverty <-    merge(santacruz, pov1, all = TRUE)



map3 <- mapview(Below_Poverty, zcol = "estimate", legend = TRUE)

mapshot(map3, url = "G:/PResentation/Poverty.html")





