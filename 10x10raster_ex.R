library("devtools")
load_all()

init_provenance_graph(namespace = "https://www.provr.com/10x10raster_ex#")

in_raster <- array(val <- sample(0:100), dim = c(10, 10))
# build input entity
in_raster_entity <- Entity("in_raster", "Input Raster")

out_raster <- array(NA, dim = c(10, 10))
# build output entity
out_raster_entity <- Entity("out_raster", "Mask Raster")

for (i in seq_along(in_raster)) {
    if (in_raster[i] >= 70) {
        out_raster[i] <- 1
    } else {
        out_raster[i] <- 0
    }
}
# build mask activity
mask_activity <- Activity("mask", "Mask", "Generate mask by setting
    every value in the input raster that is 70 or greater
    to 1 and each other value to 0.")

# set inputs for mask activity
mask_activity$used(in_raster_entity)

# set the activity that generated the output entity
out_raster_entity$wasGeneratedBy(mask_activity)

# write graph to file
serialize_provenance_graph(name = "10x10raster_ex.ttl")