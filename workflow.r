# install.packages("pacman")
pacman::p_load(geogrid,
               sf,
               tmap,
               rayshader,
               ggplot2,
               viridis,
               magick,
               gifski,
               devtools,
               janitor,
               tidyverse)

#functions
transition_values <- function(from,
                              to,
                              steps = 10,
                              one_way = FALSE,
                              type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range) / 2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <-
      cos(seq(0, 2 * pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1,-1, length.out = steps)
    } else {
      xout <- c(seq(1,-1, length.out = floor(steps / 2)),
                seq(-1, 1, length.out = ceiling(steps / 2)))
    }
    scaling <- approx(x = c(-1, 1),
                      y = c(-1, 1),
                      xout = xout)$y
  }
  
  middle - half_width * scaling
}

#raw data

# shapefile url
# https://opendata.arcgis.com/datasets/5ce27b980ffb43c39b012c2ebeab92c0_2.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D

input_file <-
  sf::read_sf(
    "shapefile/Westminster_Parliamentary_Constituencies_December_2017_Generalised_Clipped_Boundaries_in_the_UK.shp"
  )

download.file(
  "https://data.london.gov.uk/download/general-election-results-2017/26ee40ae-becf-4839-bb0c-509024e61bfd/2017%2520General%2520Election%2520Results.xls",
  mode = "wb",
  destfile = "2017_election_results.xls"
)


#combine shape file with election results
results<-alextools::read_excel_allsheets("2017_election_results.xls")

colnames(results$`Administrative data`)<-results$`Administrative data`[2,]

df<-results$`Administrative data`[-c(1:2),]%>%
  rename("pcon17cd"=`ONS Code`)%>%
  left_join(input_file)

df$Electorate<-as.numeric(df$Electorate)

response<-"Electorate"


#plot map

rawplot <- tm_shape(df) +
  tm_polygons(response, palette = "viridis")


gg_election<-ggplot(df) +
  geom_sf(
    aes(
      geometry = geometry,
      fill = Electorate,
      colour = Electorate
    ),
    lwd = 0
  )+
scale_fill_viridis(response)+
  scale_colour_viridis(response) +
  ggtitle(response) +
  theme_bw()+
  theme(legend.title = element_blank())

#use rayshading to create 3D map

plot_gg(
  gg_election,
  multicore = TRUE,
  width = 6 ,
  height = 10,
  fov = 70,
  scale = 200
)


#Define parameters for animation


zoom <- c(transition_values(1, 0.5, 48, TRUE),
          rep(0.5, 408),
          transition_values(0.5, 1, 48, TRUE))


phi <- c(
  rep(90, 72),
  transition_values(90, 45, 72, TRUE),
  rep(45, 24 * 12),
  transition_values(45, 90, 24, TRUE),
  rep(90, 48)
)

theta <- c(rep(0, 144),
           transition_values(0, 360, (24 * 12) + 24, TRUE),
           rep(0, 48))

n_frames <- length(phi)
duration <- n_frames / 30


file <- "Electorate_slower.gif"


# generate temp .png images
temp_dir <- tempdir()
img_frames <-
  file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))

message(paste("Generating", n_frames, "temporary .png images..."))
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  render_camera(
    theta = theta[i],
    phi = phi[i],
    zoom = zoom[i],
    fov = 45
  )
  render_snapshot(img_frames[i])
}

#render gif
message("Generating .gif...")
magick::image_write_gif(magick::image_read(img_frames),
                        path = file,
                        delay = duration / n_frames)


