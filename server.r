

# install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "rgdal", "raster", "dplyr", "tidyr", 
               "ggplot2", "ggmap", "gridExtra", "grid", 
               "sampSurf", "sp", "FNN", 
               update=T)



# load data
d <- readRDS("data/pixels.rds")
valid_types <- readRDS("data/types.rds")
m <- stack(list.files(recursive=T, pattern=".grd"))

txtf <- list.files("data/txt")
txt <- lapply(paste0("data/txt/", txtf), readLines)
names(txt) <- sub(".txt", "", txtf)

# coordinate systems
pll <- CRS("+proj=longlat +ellps=WGS84")
paea <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# state boundaries
boundaries <- map_data("state")


#input <- list(location="Meeker Park, CO", comparison="horizontal", zoom=10, radius=25, prob=.9, basemap="terrain", radius_regional=150, neighbors=.05)
#input <- list(location="telluride", comparison="vertical", radius=25, prob=.9)

shinyServer(function(input, output, session) {
      
      # tab panel text
      location <- reactive({
            if(input$location == "Enter a location (Googleable name in lower 48)"){return("Telluride")
            }else{return(input$location)}
      })
      output$biogeo_title <- renderText({paste0("What lives near ", location(), ", and where else does it live?")})
      output$bioclim_title <- renderText({paste0("What climates are these habitat types known to tolerate, and how does local climate change compare?")})
      output$analogs_title <- renderText({paste0("How is ", location(), "'s prototypical climate migrating across space?")})
      output$seasons_title <- renderText({paste0("How is the seasonality of ", location(), "'s climate changing?")})
      
      
      circle <- reactive({
            location <- location()
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
            title <- paste("Select from vegetation types within", input$radius, "km of", location())
            
            if(input$comparison=="horizontal"){
                  return(checkboxGroupInput("classes_selected", title, classes, selected=classes[1:4]))
            }
            if(input$comparison=="vertical"){
                  radioButtons("classes_selected", title, classes, selected=classes[1])
            }
      })
      #input$classes_selected <- d_local$classname[1:4]
      
      
      
      
      
      ############## biogeo ###############
      
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
                  #geom_polygon(data=boundaries, aes(long, lat, group=group), size=.5, color="gray40", fill=NA) +
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
                        lapply(function(x) sample_n(x, min(nrow(x), 1000))) %>%
                        do.call("rbind", .)
            }
            
            
            # build contours
            contourLevel <- function(x, y, prob = 0.95){
                  kk <- MASS::kde2d(x, y)
                  dx <- diff(kk$x[1:2])
                  dy <- diff(kk$y[1:2])
                  sz <- sort(kk$z)
                  c1 <- cumsum(sz) * dx * dy
                  approx(c1, sz, xout = 1 - prob)$y
            }
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
                  mutate(year=sub("1980", "1948.1980", year),
                         year=sub("2012", "1981.2012", year),
                         year=sub("2050", "2041.2070", year)) %>%
                  spread(variable, value) %>%
                  mutate(bio12=log10(bio12))
            if(input$comparison=="vertical") d_point$classname <- paste0("1: ", d_point$classname)
            
            d_segment <- d_point %>%
                  gather(variable, value, bio1, bio12) %>%
                  unite(info, variable, year) %>%
                  spread(info, value)
            
            ggplot() + 
                  contours_baseline_national +
                  geom_segment(data=d_segment, aes(x=bio1_1948.1980, xend=bio1_1981.2012, y=bio12_1948.1980, 
                                                   yend=bio12_1981.2012, color=classname), 
                               size=1, linetype=1) +
                  geom_segment(data=d_segment, aes(x=bio1_1981.2012, xend=bio1_2041.2070, y=bio12_1981.2012, 
                                                   yend=bio12_2041.2070, color=classname),
                               size=.5, linetype=1, alpha=.5, arrow=grid::arrow(angle=15, type="closed")) +
                  geom_point(data=d_point, aes(bio1, bio12, color=classname, alpha=factor(year)), 
                             size=8, shape=21) +
                  scale_alpha_discrete(range=c(1, .5), guide="none") +
                  scale_x_continuous(expand=c(.1,0)) +
                  scale_y_continuous(expand=c(.1,0)) +
                  labs(x="mean annual temperature", 
                       y=paste("log total annual precipitation")) +
                  theme_minimal() +
                  theme(legend.position="none")
            
      })
      
      ############## analogs ###############
      
      output$analogs <- renderPlot({
                  
                  #require(ggmap)
                  basemap <- ggmap(get_map(location(), maptype=input$basemap, 
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
                        arrange(year, x, y) %>%
                        mutate(year=sub("1980", "1948.1980", year),
                               year=sub("2012", "1981.2012", year),
                               year=sub("2050", "2041.2070", year))
                  
                  
                  pc <- prcomp(f[,c("bio1", "bio12")], center=T, scale=T)
                  
                  f_local <- d_local() %>%
                        dplyr::select(x, y, bio1_1980:bio12_2050) %>%
                        gather(info, value, bio1_1980:bio12_2050) %>%
                        separate(info, c("variable", "year"), sep="_", convert=T) %>%
                        spread(variable, value) %>%
                        mutate(bio12=log10(bio12),
                               year=sub("1980", "1948.1980", year),
                               year=sub("2012", "1981.2012", year),
                               year=sub("2050", "2041.2070", year)) %>%
                        group_by(year) %>%
                        summarize_each(funs(mean), bio1, bio12)
                  pc_local <- predict(pc, 
                                      f_local[f_local$year==sub("-", ".", input$reference_period), 
                                              c("bio1", "bio12")])
                  
                  
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
                        geom_raster(data=fn[fn$year=="1948.1980",], 
                                    aes(x, y), fill="blue", alpha=.4) +
                        geom_raster(data=fn[fn$year=="1981.2012",], 
                                    aes(x, y), fill="green", alpha=.4) +
                        geom_raster(data=fn[fn$year=="2041.2070",], 
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
                        theme(legend.position="top", legend.direction="vertical",
                              axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
                        labs(y="relative land area", fill="time period", color="time period")
                  
                  grid.draw(arrangeGrob(latlong, elev, nrow=1, widths=c(2, 1)))
            })
      
      
      output$analogs_description <- renderText(txt$analogs)
      output$seasons_description <- renderText(txt$seasons)
      
      
      
      
      ############ seasons #############
      
      output$seasonal <- renderPlot({
            
            exclude <- sub("-", ".", input$period_excluded)
            
            md <- data.frame(info=names(m)) %>%
                  separate(info, c("variable", "year", "month"), sep="_") %>%
                  mutate(year=sub("1980", "1948.1980", year),
                         year=sub("2012", "1981.2012", year),
                         year=sub("2050", "2041.2070", year))
            md$value <- as.vector(raster::extract(m, circle()$location))
            
            md <- md %>%
                  filter(year != exclude) %>%
                  spread(variable, value) %>%
                  mutate(year=factor(year, levels=c("1948.1980", "1981.2012", "2041.2070")))
            
            if(input$detrend==T){
                  deltas <- md %>%
                        gather(variable, value, ppt, tmean) %>%
                        group_by(year, variable) %>%
                        summarize(value=mean(value)) %>%
                        spread(year, value)
                  deltas[,4] <- as.vector(deltas[,3] - deltas[,2])
                  names(deltas)[4] <- "delta"
                  
                  md$ppt[md$year==names(deltas)[3]] <- md$ppt[md$year==names(deltas)[3]] - deltas$delta[deltas$variable=="ppt"]
                  md$tmean[md$year==names(deltas)[3]] <- md$tmean[md$year==names(deltas)[3]] - deltas$delta[deltas$variable=="tmean"]
                  
            }
            
            
            k <- md %>%
                  gather(variable, value, ppt, tmean) %>%
                  unite(info, variable, year) %>%
                  spread(info, value)
            names(k) <- c("month", "ppt1", "ppt2", "tmean1", "tmean2")
            
            w <- md %>%
                  #filter(year=="1981.2012") %>%
                  group_by(month) %>%
                  summarize(tmean=mean(tmean), 
                            ppt=mean(ppt))
            
            
            
            ggplot(md) + 
                  geom_segment(data=k, aes(x=tmean1, xend=tmean2, 
                                           y=ppt1, yend=ppt2),
                               color="gray", arrow=grid::arrow(angle=15, type="closed", length=unit(.2, "in"))) +
                  geom_polygon(aes(x=tmean, y=ppt, color=year, group=year, order=month), 
                               fill=NA) +
                  geom_point(aes(x=tmean, y=ppt, color=year, group=year, order=month),
                             size=3) +
                  geom_text(data=w, aes(x=tmean, y=ppt, label=month),
                            color="black") +
                  scale_color_manual(values=c("blue", "limegreen", "red"), drop=F) +
                  theme_minimal() +
                  theme(legend.position="right") +
                  scale_y_log10(breaks=c(1, 1.5,3,5, 10,15,30,50,100,150,300,500,1000,1500,3000)) +
                  labs(x="mean monthly temperature (deg c)", 
                       y="total monthly precipitation (mm, log scale)")
      })
      
})

