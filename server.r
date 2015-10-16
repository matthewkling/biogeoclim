


# install and load required packages
packages <- c("shiny", "rgdal", "raster", "dplyr", "tidyr", "ggplot2", "ggmap", "gridExtra", "grid", "sampSurf", "sp", "FNN")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(packages, update=T)



# load data
d <- readRDS("data/pixels.rds")
valid_types <- readRDS("data/types.rds")

# coordinate systems
pll <- CRS("+proj=longlat +ellps=WGS84")
paea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# state boundaries
boundaries <- map_data("state")


#input <- list(location="Meeker Park, CO", comparison="horizontal", zoom=10, radius=25, prob=.9, basemap="terrain", radius_regional=150, neighbors=.05)

#input <- list(location="telluride", comparison="vertical", radius=25, prob=.9)

shinyServer(function(input, output, session) {
      
      #output$logo <- renderImage({
      #      list(src = "data/logo.png",
      #           contentType = 'image/png',
      #           width = 200,
      #           height = 200*486/421)
      #}, deleteFile = F)
      
      # tab panel text
      output$biogeo_title <- renderText({paste0("What lives near ", input$location, ", and where else does it live? What climates are these habitat types known to tolerate, and how does local climate change stack up?")})
      output$geoclim_title <- renderText({paste0("With climate change, how are climates analogous to ", input$location, "\'s migrating across space?")})
      
      
      circle <- reactive({
            location <- input$location
            if(is.na(as.numeric(substr(location, nchar(location), nchar(location))))){
                  point <- geocode(location, source="google")
            } else{
                  point <- as.numeric(str_split(location, pattern=", "))
            }
            names(point) <- c("x", "y")
            coordinates(point) <- c("x", "y")
            projection(point) <- pll
            point <- spTransform(point, paea)
            point <- as.data.frame(point)
            point <- c(x=point$x, y=point$y)
            radius_m <- input$radius * 1000
            circle <- sampSurf::spCircle(radius_m, pll, point)
            lapply(circle, function(x){
                  projection(x) <- paea
                  spTransform(x, pll)
            })
      })
      
      
      d_local <- reactive({
            s <- as.data.frame(d)
            coordinates(s) <- c("x", "y")
            projection(s) <- pll
            s <- s[!is.na(sp::over(s, circle()$spCircle)),]
            s <- tbl_df(as.data.frame(s))
            types <- rev(sort(table(s$vegtype)))
            types <- as.integer(names(types))
            types <- types[types %in% valid_types]
            types <- types[1:8]
            s[s$vegtype %in% types,]
      })
      
      
      output$localtypes <- renderUI({
            classes <- as.character(na.omit(unique(d_local()$classname)))
            title <- paste("Select from vegetation types within", input$radius, "km of", input$location)
            
            if(input$comparison=="horizontal"){
                  return(checkboxGroupInput("classes_selected", title, classes, selected=classes[1:4]))
            }
            if(input$comparison=="vertical"){
                  radioButtons("classes_selected", title, classes, selected=classes[1])
            }
      })
      #input$classes_selected <- d_local$classname[1:4]
      
      
      
      
      
      ############## BIO-GEO ###############
      
      output$biogeo <- renderPlot({
            
            dc <- fortify(circle()$spCircle)
            dp <- as.data.frame(circle()$location)
            
            style <- theme(axis.text=element_blank(), axis.title=element_blank(), 
                           axis.ticks=element_blank(), panel.grid=element_blank())
            expand <- function(x, margin=.25){
                  r <- x[2] - x[1]
                  b <- r * margin
                  x + c(-b, b)
            }
            
            
            if(input$comparison=="horizontal"){
                  f <- d %>%
                        dplyr::select(x, y, classname) %>%
                        filter(classname %in% input$classes_selected)
            }
            
            if(input$comparison=="vertical"){
                  d_type <- d_local() %>%
                        select(classname, evt_order, evt_sbcls) %>%
                        filter(classname %in% input$classes_selected) %>%
                        distinct() %>%
                        mutate_each(funs(as.character))
                  f <- d %>%
                        dplyr::select(x, y, classname, evt_order, evt_sbcls) %>%
                        filter(evt_order %in% d_type$evt_order) %>%
                        gather(level, name, classname, evt_order, evt_sbcls) %>%
                        filter(name %in% d_type[1,]) %>%
                        mutate(classname = name) %>%
                        mutate(level = as.integer(factor(level, levels=c("classname", "evt_sbcls", "evt_order")))) %>%
                        arrange(x, y, level) %>%
                        group_by(x, y) %>%
                        slice(1) %>%
                        ungroup() %>%
                        mutate(classname = paste0(level, ": ", name))
            }
            
            #colors <- list(horizontal=c("red", "purple", "blue", "forestgreen"),
            #               vertical=c("darkblue", "dodgerblue", "cadetblue1"))
            #colors <- colors[[input$comparison]]
            #colors <- scale_fill_manual(values=colorRampPalette(colors)(length(unique(f$classname))))
            
            regional_map <- ggplot() + 
                  geom_raster(data=f, aes(x, y, fill=classname), size=.6, shape=15) +
                  geom_polygon(data=boundaries, aes(long, lat, group=group), 
                               size=.5, color="gray40", fill=NA) +
                  geom_polygon(data=dc, aes(long, lat), color="black", fill=NA) +
                  annotate(geom="point", x=dp$x, y=dp$y, shape=3, size=3) +
                  coord_fixed(xlim=expand(range(f$x, na.rm=T), margin=.05), 
                              ylim=expand(range(f$y, na.rm=T), margin=.05), 
                              ratio=1.3) +
                  theme_minimal() +
                  style +
                  theme(legend.position="none")
            
            local_map <- ggplot() + 
                  geom_polygon(data=boundaries, aes(long, lat, group=group), size=.5, color="gray40", fill=NA) +
                  geom_raster(data=f, aes(x, y, fill=classname)) +
                  geom_polygon(data=dc, aes(long, lat), color="black", fill=NA) +
                  annotate(geom="point", x=dp$x, y=dp$y, shape=3, size=10) +
                  coord_fixed(xlim=expand(range(dc$long)), 
                              ylim=expand(range(dc$lat)), 
                              ratio=1.3) +
                  labs(x=NULL, y=NULL, fill=NULL) +
                  theme_minimal() +
                  style +
                  theme(legend.position="bottom",
                        legend.direction="vertical")
            
            grid.draw(arrangeGrob(local_map, regional_map,  
                                  nrow=1, widths=c(2, 3)))
      })
      
      
      
      ######################  BIO-CLIM #####################
      
      output$bioclim <- renderPlot({
            if(input$comparison=="horizontal"){
                  f <- d %>%
                        dplyr::select(x, y, classname, bio1_1980, bio12_1980) %>%
                        filter(classname %in% input$classes_selected) %>%
                        mutate(bio12_1980 = log10(bio12_1980))
                  names(f) <- gsub("_1980", "", names(f))
            }
            
            if(input$comparison=="vertical"){
                  d_type <- d_local() %>%
                        select(classname, evt_order, evt_sbcls) %>%
                        filter(classname %in% input$classes_selected) %>%
                        distinct() %>%
                        mutate_each(funs(as.character))
                  f <- d %>%
                        dplyr::select(x, y, classname, evt_order, evt_sbcls, bio1_1980, bio12_1980) %>%
                        filter(evt_order %in% d_type$evt_order) %>%
                        gather(level, name, classname, evt_order, evt_sbcls) %>%
                        filter(name %in% d_type[1,]) %>%
                        mutate(classname = name) %>%
                        mutate(level = as.integer(factor(level, levels=c("classname", "evt_sbcls", "evt_order")))) %>%
                        arrange(x, y, level) %>%
                        group_by(x, y) %>%
                        slice(1) %>% # retain only the most detailed level for each pixel
                        ungroup() %>%
                        mutate(classname = paste0(level, ": ", name),
                               bio12_1980=log10(bio12_1980))
                  names(f) <- sub("_1980", "", names(f))
                  f <- f %>%
                        split(f$classname) %>%
                        lapply(function(x) sample_n(x, min(nrow(x), 10000))) %>%
                        do.call("rbind", .)
            }
            
            # build contours
            getlevel <- function(i){
                  x <- f[f$classname==i,]
                  x <- na.omit(x)
                  x <- try(contourLevel(x$bio1, x$bio12, input$prob))
                  if(class(x)=="try-error") x <- NA
                  return(x)
            }
            levels <- sapply(unique(f$classname), getlevel)
            alph <- .2
            if(input$comparison=="vertical") alph <- .05
            contours_baseline_national <- mapply(function(df, b) stat_density2d(data=df, 
                                                                                aes(x=bio1, y=bio12, color=classname, fill=classname), 
                                                                                breaks=b, geom="polygon", alpha=alph, size=.1, na.rm=T), 
                                                 plyr::dlply(f, plyr::.(classname)), levels)
            
            
            
            
            # point and segment data
            d_point <- d_local() %>%
                  filter(classname %in% input$classes_selected) %>%
                  group_by(classname) %>%
                  summarize_each(funs(mean), bio1_1980:bio12_2050) %>%
                  gather(info, value, bio1_1980:bio12_2050) %>%
                  separate(info, c("variable", "year"), sep="_", convert=T) %>%
                  spread(variable, value) %>%
                  mutate(bio12=log10(bio12))
            if(input$comparison=="vertical") d_point$classname <- paste0("1: ", d_point$classname)
            
            d_segment <- d_point %>%
                  gather(variable, value, bio1, bio12) %>%
                  unite(info, variable, year) %>%
                  spread(info, value)
            
            ggplot() + 
                  contours_baseline_national +
                  geom_segment(data=d_segment, aes(x=bio1_1980, xend=bio1_2012, y=bio12_1980, 
                                            yend=bio12_2012, color=classname), 
                               size=1, linetype=1) +
                  geom_segment(data=d_segment, aes(x=bio1_2012, xend=bio1_2050, y=bio12_2012, 
                                            yend=bio12_2050, color=classname),
                               size=.5, linetype=1, alpha=.5, arrow=grid::arrow(angle=15, type="closed")) +
                  geom_point(data=d_point, aes(bio1, bio12, color=classname, alpha=factor(year)), 
                             size=8, shape=21) +
                  scale_alpha_discrete(range=c(1, .5), guide="none") +
                  scale_x_continuous(expand=c(.1,0)) +
                  scale_y_continuous(expand=c(.1,0)) +
                  labs(x="mean annual temperature", 
                       y=paste("log total annual precipitation")) +
                  theme_minimal() +
                  theme(legend.position="bottom", legend.direction="vertical")
            
      })
      
      ############## GEO-CLIM ###############
      
      output$geoclim <- renderPlot({
            
            #require(ggmap)
            basemap <- ggmap(get_map(input$location, maptype=input$basemap, 
                                     color="bw", zoom=input$zoom))
            
            ext <- c(range(basemap$data$lon), range(basemap$data$lat))
            
            
            f <- d %>% 
                  filter(classname != "Open Water") %>%
                  dplyr::select(x, y, elevation, bio1_1980:bio12_2050) %>%
                  filter(x>=ext[1], x<=ext[2],
                         y>=ext[3], y<=ext[4]) %>%
                  gather(info, value, bio1_1980:bio12_2050) %>%
                  separate(info, c("variable", "year"), sep="_", convert=T) %>%
                  spread(variable, value) %>%
                  mutate(bio12=log10(bio12)) %>%
                  arrange(year, x, y)
            
            pc <- prcomp(f[,c("bio1", "bio12")], center=T, scale=T)
            
            f_local <- d_local() %>%
                  dplyr::select(x, y, bio1_1980:bio12_2050) %>%
                  gather(info, value, bio1_1980:bio12_2050) %>%
                  separate(info, c("variable", "year"), sep="_", convert=T) %>%
                  spread(variable, value) %>%
                  mutate(bio12=log10(bio12)) %>%
                  group_by(year) %>%
                  summarize_each(funs(mean), bio1, bio12)
            pc_local <- predict(pc, f_local[f_local$year==input$reference_year, c("bio1", "bio12")])
            
            
            library(FNN)
            k <- input$neighbors
            k <- round(nrow(f) / 3 * k, 0)
            fy <- split(f, f$year)
            pcy <- split(as.data.frame(pc$x), f$year)
            nny <- lapply(pcy, function(x){
                  get.knnx(data=as.matrix(x), query=matrix(pc_local, ncol=2), k=k)
            })
            
            for(x in names(nny)){
                  fy[[x]]$nearest <- 0
                  fy[[x]]$nearest[nny[[x]]$nn.index] <- 1
            }
            f <- do.call("rbind", fy)
            fn <- filter(f, nearest==1)
            
            
            
            style <- theme(axis.text=element_blank(), axis.title=element_blank(), 
                           axis.ticks=element_blank(), panel.grid=element_blank())
            dc <- fortify(circle()$spCircle)
            dp <- as.data.frame(circle()$location)
            
            
            
            latlong <- basemap +
                  geom_raster(data=fn[fn$year==1980,], 
                              aes(x, y), fill="blue", alpha=.4) +
                  geom_raster(data=fn[fn$year==2012,], 
                              aes(x, y), fill="green", alpha=.4) +
                  geom_raster(data=fn[fn$year==2050,], 
                              aes(x, y), fill="red", alpha=.4) +
                  geom_polygon(data=dc, aes(long, lat), color="black", fill=NA) +
                  annotate(geom="point", x=dp$x, y=dp$y, shape=3, size=3) +
                  coord_cartesian() +
                  coord_fixed(ratio=1.3) +
                  theme_minimal() +
                  style
             
            elev <- ggplot(fn, aes(elevation, fill=factor(year), color=factor(year))) +
                  geom_density(alpha=.3, adjust=.8) +
                  scale_fill_manual(values=c("blue", "green", "red")) +
                  scale_color_manual(values=c("blue", "green", "red")) +
                  coord_flip() +
                  theme_minimal() +
                  theme(legend.position="top") +
                  labs(y="relative land area", fill="time period", color="time period")
            
            grid.draw(arrangeGrob(latlong, elev, nrow=1, widths=c(2, 1)))
            
            
      })
      
      
})

