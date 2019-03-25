

### Establish local school shapefiles -------

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


### Functions ------

join.map <- function(file){
        
        feeders <- read_csv( file) %>%
                mutate(CDSCode = as.character(CDSCode))
        
        
        joint <- monterey_schools %>%
                left_join(feeders)
        
        joint2 <- monterey_primary_middle  %>%
                left_join(joint) %>%
                filter(isdistrict == 0,
                       !str_detect(SchoolName,"Boronda Elem")) %>%
                select(SchoolName,
                       District,
                       Grades = GSserved,
                       Chronic = ChronicAbsenteeismRate,
                       ELA,
                       Math,
                       Suspension = `Suspension Rate (Total)`  ,
                       geometry) %>%
                mutate(District = fct_drop(District),
                       Grades = fct_drop(Grades)) %>%
                filter(Grades %notin% c("K-5", "K-3", "4-5") )
}

make.maps <- function(file, districtname, groupy ,centerpoint){
        
        joint2<- join.map(file)
        
        
        for( i in c("ELA", "Math", "Chronic", "Suspension")){
                map.ela <- tm_shape(joint2) +
                        tm_fill(i, alpha = .5, popup.vars = c("District", "Grades" ,"ELA", "Math", "Chronic", "Suspension")) +
                        tm_borders() +
                        tm_text("SchoolName", auto.placement = TRUE) +
                        tm_view(set.view = centerpoint)
                
                tmap_save(map.ela, here("maps", paste0("map-",districtname,"-",groupy ,"-" ,i,".html")))
        }
        
        
}



###  Join School data ------

joint2 <- join.map("Feeder Districts Salinas UnionALL.csv") 

### Make some maps -------


tmap_mode("view")


#  Map loop for single var

map.ela <- tm_shape(joint2) +
        tm_fill("ELA", alpha = .5, popup.vars = c("District","Grades","ELA", "Math", "Chronic", "Suspension")) +
        tm_borders() +
        tm_text("SchoolName", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.68 , lon = -121.65 ,  zoom = 13))

tmap_save(map.ela,  "map-Salinas-ela.html")


#  Make four maps for each data set provided

make.maps(here("data", "Feeder Districts SMCJUHSD ALL.csv"), "SMCJUHSD" , "all" ,c(lat = 36 , lon = -121.32 ,  zoom = 9))
make.maps(here("data","Feeder Districts Salinas Union ALL.csv"), "SUHSD" ,"all" ,c(lat = 36.68 , lon = -121.65 ,  zoom = 13))

make.maps(here("data", "Feeder Districts SMCJUHSD EL.csv"), "SMCJUHSD" , "EL" ,c(lat = 36 , lon = -121.32 ,  zoom = 9))
make.maps(here("data","Feeder Districts Salinas Union EL.csv"), "SUHSD" ,"EL" ,c(lat = 36.68 , lon = -121.65 ,  zoom = 13))

make.maps(here("data", "Feeder Districts SMCJUHSD SWD.csv"), "SMCJUHSD" , "SWD" ,c(lat = 36 , lon = -121.32 ,  zoom = 9))
make.maps(here("data","Feeder Districts Salinas Union SWD.csv"), "SUHSD" ,"SWD" ,c(lat = 36.68 , lon = -121.65 ,  zoom = 13))



#  Make maps for ELPI


feeders <- read_csv(here("data" ,"Feeder Districts Salinas Union ELPI.csv")) %>%
        mutate(CDSCode = as.character(CDSCode))


joint <- monterey_schools %>%
        left_join(feeders)

joint2 <- monterey_primary_middle  %>%
        left_join(joint) %>%
        filter(!is.na(SchoolName)) %>%
        mutate(Grades = GSserved) %>%
        filter(Grades %notin% c("K-5", "K-3", "4-5") ) %>%
        select(-1:-2)

map.elpi <- tm_shape(joint2) +
        tm_fill("PL1_Pct", alpha = .5, popup.vars = c("District", "PL1_Pct" ,"PL2_Pct", "PL3_Pct", "PL4_Pct")) +
        tm_borders() +
        tm_text("School", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.68 , lon = -121.65 ,  zoom = 13))

tmap_save(map.elpi, here("maps", paste0("map-SUHSD-ELPI.html")))





#  Facet with two in sync maps 

map.facet <- tm_shape(joint2 ) +
        tm_polygons(c("Chronic", "Suspension")) +
        tm_borders() +
        tm_text("NAME", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.65 , lon = -121.6 ,  zoom = 10)) +
        tm_facets(sync = TRUE, ncol = 2)

#  Map with lots of choosable layers 
map.facet <- tm_shape(joint2) +
        tm_polygons(c("District","Grades","ELA", "Math", "Chronic", "Suspension")) +
        tm_borders() +
        tm_text("SchoolName", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.65 , lon = -121.6 ,  zoom = 10)) +
        tm_facets(sync = TRUE, as.layers = TRUE)

# This doens't work. 
# tmap_save(map.facet,  "map-Salinas-layers.html")


### Experiment with mapview ----
mapview(joint2, xcol = "ELA")

m1 <- mapView(franconia, col.regions = "red")
m2 <- mapView(breweries)
test <- m1 + breweries + poppendorf[[4]]

mapshot(test, "test.html")



### SMCJUHSD Middle mapping ------

joint2<- join.map("Feeder Districts SMCJUHSDALL.csv")

# Map with lots of choosable layers 
map.facet <- tm_shape(joint2) +
        tm_polygons(c("District","Grades","ELA", "Math", "Chronic", "Suspension")) +
        tm_borders() +
        tm_text("SchoolName", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36 , lon = -121.32 ,  zoom = 9)) +
        tm_facets(sync = TRUE, as.layers = TRUE)

map.facet

