# load data from txt
scottish_lakes <- read.table("scottish_lakes.txt", sep = " ",
                             col.names = c("name", "volume", "area",
                                           "length", "max_depth",
                                           "mean_depth"))

# find the lake with the highest/lowest volume/area
largest_volume <- scottish_lakes$name[which.max(scottish_lakes$volume)]
smallest_volume <- scottish_lakes$name[which.min(scottish_lakes$volume)]
largest_area <- scottish_lakes$name[which.max(scottish_lakes$area)]
smallest_area <- scottish_lakes$name[which.min(scottish_lakes$area)]

# order the frame by area
scottish_lakes <- scottish_lakes[order(scottish_lakes$area,
                                       decreasing = TRUE), ]

# total area
total_area <- sum(scottish_lakes$area)
print(paste0("Area of Scotland covered by water: ", total_area, " km2"))
