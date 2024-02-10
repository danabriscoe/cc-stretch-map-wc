## map_wc_helper_functions.R

# helper functions to process data and plot leaflet map


# # l <- leaflet() %>% addTiles()
# addTrackLines <- function(m, df, cpal=wc_pal, .indiv=TRUE){
#     
#     df_split <- split(df, df$id)
#     
#     names(df_split) %>%
#         purrr::walk( function(x) {
#             m <<- m %>%
#                 addPolylines(data=df_split[[x]],
#                            lng=~lon, lat=~lat,
#                            # label=~as.character(turtle_name),
#                            popup=paste0(
#                                "<strong>", "Name: "  , "</strong>", df_split[[x]]$turtle_name, "<br>",
#                                "<strong>", "ID: ", "</strong>", x),
#                            
#                            color = ~cpal(id),
#                            group = ifelse(.indiv==TRUE, x, 
#                                           str_c("All Tracks (n=", daily_df$id %>% n_distinct(),")"))
#                            # labelOptions = labelOptions(noHide = F,
#                            #                             direction = 'auto')
#                            )
#         })
#     return(m)
# } # end func
# ###-------
# 
# 
# # add all/indiv tracks to map as lines +/- circle markers
# addMapTracks <- function(m, df, cpal, .indiv=TRUE, .addLines=TRUE, .addMarkers=TRUE){
#     # m = base leaflet map
#     # df = dataframe to map
#     # ids = indiv track ids
#     
#     ids <- unique(df$id)    
#     t_names <- unique(df$turtle_name)
#     t_nums <- unique(df$turtle_num)
#     
#     # Add track lines
#     for (id in ids) {
#         
#         df_id <- df[df$id == id, ]
#         
#         if(!.indiv){
#             grp <- str_c("All Tracks (n=", ids %>% n_distinct(),")")
#             grps <- grp
#         } else {
#             grp <- id
#             grps <- ids #c(str_c(t_nums, ' - ', t_names))
#         }
#         
#         if(.addLines & !.addMarkers){
#             m <- m |>
#                 # Add track lines
#                 addPolylines(data = df_id, group = as.character(id),
#                              lng = ~lon,
#                              lat = ~lat,
#                              
#                              popup=paste0(
#                                  "<strong>", "ID: ", "</strong>", df_id$id), 
#                              
#                              color = ~cpal(id)) 
#             
#         } else if (.addLines & .addMarkers) {
#             m <- m |>
#                 # Add track lines
#                 addPolylines(data = df_id, group = as.character(id),
#                              lng = ~lon,
#                              lat = ~lat,
#                              color = ~cpal(id)) 
#             
#         } 
#         if(.addMarkers | !.addLines){
#             m <- m |>    
#                 # Turtle Data Points
#                 addCircleMarkers(data = df_id[df_id$id == id, ], group = as.character(grp),
#                                  lng= ~lon,
#                                  lat= ~lat, # the spatial data, requires "~" here because it's NOT sp or sf object
#                                  
#                                  popup=paste0(
#                                      "<strong>", "Name: "  , "</strong>", df_id$turtle_name, "<br><strong>",
#                                      "ID: ", "</strong>", df_id$id, "<br><br><strong>",
#                                      "Date: ",  "</strong>", format(df_id$date, format="%Y-%b-%d"), "<br><strong>",
#                                      "Lat: ", "</strong>", round(df_id$lat,2), "°N","<br><strong>",
#                                      "Lon: ", "</strong>", round(df_id$lon,2),"°W"),
#                                  
#                                  stroke=TRUE, weight=0.6,radius=5,
#                                  fillOpacity = 0.75, color="black",
#                                  fillColor= ~cpal(id))  # mapping to the color palette
#         }
#     } # end for loop
#     
#     
#     
#     #create layer control
#     ret_map <- m %>%
#         addLayersControl(
#             baseGroups = c("ESRI World Imagery", "ESRI OpenStreetMap"
#                            # , "Esri.WorldPhysical"
#             ),
#             overlayGroups = c(grps),
#             # hideGroup(as.character(c(ids[3:25])))
#             options = layersControlOptions(collapsed = TRUE)) %>%
#         
#         addScaleBar(position = c("bottomleft"),
#                     scaleBarOptions(
#                         # maxWidth = 100,
#                         metric = TRUE,
#                         imperial = TRUE,
#                         updateWhenIdle = TRUE)
#         )
#     
#     return(ret_map)
#     
# } # end function


## Repeat Above but use Names instead of IDs
addTrackLines <- function(m, df, cpal=wc_pal, .indiv=TRUE){
  
  # df_split <- split(df, df$id)
  df_split <- split(df, df$turtle_name)
  
  names(df_split) %>%
    purrr::walk( function(x) {
      m <<- m %>%
        addPolylines(data=df_split[[x]],
                     lng=~lon, lat=~lat,
                     # label=~as.character(turtle_name),
                     popup=paste0(
                       "<strong>", "Name: "  , "</strong>", df_split[[x]]$turtle_name, "<br>",
                       "<strong>", "ID: ", "</strong>", x),
                     
                     color = ~cpal(id),
                     
                     options = pathOptions(pane = "tracks"),
                     
                     group = ifelse(.indiv==TRUE, x, 
                                    str_c("All Tracks (n=", daily_df$id %>% n_distinct(),")"))
                     # labelOptions = labelOptions(noHide = F,
                     #                             direction = 'auto')
        )
    })
  return(m)
} # end func


# add all/indiv tracks to map as lines +/- circle markers
addMapTracks <- function(m, df, cpal, .indiv=TRUE, .addLines=TRUE, .addMarkers=TRUE){
  # m = base leaflet map
  # df = dataframe to map
  # ids = indiv track ids
  
  # ids <- unique(df$id)    
  ids <- unique(df$turtle_name)    
  t_names <- unique(df$turtle_name)
  t_nums <- unique(df$turtle_num)
  
  # Add track lines
  for (id in ids) {
    
    # df_id <- df[df$id == id, ]
    df_id <- df[df$turtle_name == id, ]
    
    if(!.indiv){
      grp <- str_c("All Tracks (n=", ids %>% n_distinct(),")")
      grps <- grp
    } else {
      grp <- id
      grps <- ids #c(str_c(t_nums, ' - ', t_names))
    }
    
    if(.addLines & !.addMarkers){
      m <- m |>
        # Add track lines
        addPolylines(data = df_id, group = as.character(id),
                     lng = ~lon,
                     lat = ~lat,
                     
                     popup=paste0(
                       "<strong>", "ID: ", "</strong>", df_id$id), 
                     
                     color = ~cpal(id)) 
      
    } else if (.addLines & .addMarkers) {
      m <- m |>
        # Add track lines
        addPolylines(data = df_id, group = as.character(id),
                     lng = ~lon,
                     lat = ~lat,
                     color = ~cpal(id)) 
      
    } 
    if(.addMarkers | !.addLines){
      m <- m |>    
        # Turtle Data Points
        addCircleMarkers(
          # data = df_id[df_id$id == id, ], 
          data = df_id[df_id$turtle_name == id, ],            
          group = as.character(grp),
          lng= ~lon,
          lat= ~lat, # the spatial data, requires "~" here because it's NOT sp or sf object
          
          popup=paste0(
            "<strong>", "Name: "  , "</strong>", df_id$turtle_name, "<br><strong>",
            "ID: ", "</strong>", df_id$id, "<br><br><strong>",
            "Date: ",  "</strong>", format(df_id$date, format="%Y-%b-%d"), "<br><strong>",
            "Lat: ", "</strong>", round(df_id$lat,2), "°N","<br><strong>",
            "Lon: ", "</strong>", round(df_id$lon,2),"°W"),
          
          stroke=TRUE, weight=0.6,radius=5,
          fillOpacity = 0.75, color="black",
          fillColor= ~cpal(id))  # mapping to the color palette
    }
  } # end for loop
  
  
  
  #create layer control
  ret_map <- m %>%
    addLayersControl(
      baseGroups = c("ESRI World Imagery", "ESRI OpenStreetMap"
                     # , "Esri.WorldPhysical"
      ),
      overlayGroups = c(grps),
      # hideGroup(as.character(c(ids[3:25])))
      options = layersControlOptions(collapsed = TRUE)) %>%
    
    addScaleBar(position = c("bottomleft"),
                scaleBarOptions(
                  # maxWidth = 100,
                  metric = TRUE,
                  imperial = TRUE,
                  updateWhenIdle = TRUE)
    )
  
  return(ret_map)
  
} # end function



addMapInset <- function(m){
  m %>%  addMiniMap(
    tiles = "Esri.WorldImagery",
    position = 'bottomleft', 
    # width = 200, height = 200,
    toggleDisplay = TRUE) 
}    



## Attach metadata to df
attach_metadata <- function(dat_df, meta_df, by="id"){
  merged_df <- 
    dat_df %>%
    merge(., meta_df, by="id", all.x = TRUE)
  
  return(merged_df)
}


## Calc avg daily location
calc_avg_daily_loc <- function(x, ...) {
  # check date attr
  if(!is.Date(raw_data_cohort_1_w_names$date)){
    x <- x %>%
      mutate(date = as.Date(date))
  } else {
    print('error: fix date attribute')
  }
  
  # vars to group
  vars1 <- syms(...) # must be in quotes
  
  # calc daily avg by groups 
  ret <- x %>%
    group_by(!!!vars1) %>%
    summarise(lat = mean(lat), 
              lon = mean(lon),
              scl_cm = mean(scl_cm), 
              .groups = 'drop')
  return(ret)
}


# calc weekly summary stats
calcWeeklyStats <- function(x, var, by=week_idx){
  var <- enquo(var)
  # dots <- ensyms(...)
  grp_by <- enquo(by)
  # if(.unique_weeks){
  #     grp <- as.name(substitute('end_of_week'))
  # } else {
  #     grp <- as.name(substitute(week_idx))
  # }
  # 
  ret <- x %>%
    group_by(!!grp_by) %>%
    summarize(
      avg = mean(!!var, na.rm=TRUE),
      med = median(!!var, na.rm=TRUE),
      min = min(!!var),
      max = max(!!var),
      high = mean(!!var, na.rm = T) + 0.2 * sd(!!var, na.rm= T),
      low  = mean(!!var, na.rm = T) - 0.2 * sd(!!var, na.rm= T), .groups = "drop")  
  return(ret)
}

# make180
make180 <- function(lon){
  isnot360<-min(lon)<0
  if (!isnot360) {
    ind<-which(lon>180)
    lon[ind]<-lon[ind]-360
  }
  return(lon)
}

# make360
make360 <- function(lon){
  isnot360<-min(lon)<0
  if(isnot360){
    ind<-which(lon<0)
    lon[ind]<-lon[ind]+360
  }
  return(lon)
}


# make spatial lines (for leafgl)
makeSpatialLines <- function(df, lon360=TRUE){
  if(lon360){
    df <- df %>% mutate(lon = make360(lon))
  }
  
  ret <- df %>%
    dplyr::mutate(id = as.factor(id)) %>% 
    st_as_sf(coords=c('lon', 'lat'), crs=4326) %>%
    group_by(id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  return(ret)
}


## Load raw data (from wc downloads)
load_wc_downloads <- function(wc_files){
  ret <- rbindlist(lapply(wc_files, fread)) %>%
    dplyr::select('Platform ID No.', 'Latitude', 'Longitude', 'Loc. quality', 'Loc. date') %>%
    dplyr::rename(id = 1,
                  lat = 2, 
                  lon = 3, 
                  loc_quality = 4, 
                  date = 5
    ) %>%
    mutate(date = as.POSIXct(date, format= "%m/%d/%Y %H:%M:%S", tz="UTC")) %>%
    as_tibble()
  
  return(ret)
}

## Load metadata files
load_metadata_xls <- function(meta_files){
  ret <- readxl::read_excel(path = meta_files) %>%
    dplyr::select(1,3,5,6) %>%
    dplyr::rename("turtle_num"=1, "id"=2, "turtle_name"=3, "scl_cm"=4)
  return(ret)
}    
