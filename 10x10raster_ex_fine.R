library("devtools")
library("uuid")
load_all()

init_provenance_graph(namespace = "https://www.provr.com/10x10raster_ex#")

in_raster <- array(val <- sample(0:100), dim = c(3, 3))
in_raster_entity <- Entity("in_raster", "Input Raster")

out_raster <- array(NA, dim = c(3, 3))
out_raster_entity <- Entity("out_raster", "Mask Raster")

enter_loop_activity <- Activity("enter_loop", "Enter Loop")
enter_loop_activity$used(in_raster_entity)


leave_loop_activity <- Activity("leave_loop", "Leave Loop")

for (i in seq_along(in_raster)) {
    # build input entity
    id <- paste("in_px", toString(i), sep = "_")
    label <- toString(in_raster[i])
    in_px_entity <- Entity(id, label)
    in_px_entity$wasGeneratedBy(enter_loop_activity)

    out_px_entity <- NA
    if (in_raster[i] >= 70) {
        out_raster[i] <- 1

        # build set to 1 activity
        id <- paste("set_to_one", UUIDgenerate(), sep = "_")
        set_to_one_activity <- Activity(id, "Set Pixel to 1")
        set_to_one_activity$used(in_px_entity)

        # build output entity
        id <- paste("out_px", toString(i), sep = "_")
        out_px_entity <- Entity(id, "1")
        out_px_entity$wasGeneratedBy(set_to_one_activity)
    } else {
        out_raster[i] <- 0

        # build set to 0 activity
        id <- paste("set_to_zero", UUIDgenerate(), sep = "_")
        set_to_zero_activity <- Activity(id, "Set Pixel to 0")
        set_to_zero_activity$used(in_px_entity)

        # build output entity
        id <- paste("out_px", toString(i), sep = "_")
        out_px_entity <- Entity(id, "0")
        out_px_entity$wasGeneratedBy(set_to_zero_activity)
    }
    leave_loop_activity$used(out_px_entity)
}
out_raster_entity$wasGeneratedBy(leave_loop_activity)


# write graph to file
serialize_provenance_graph(name = "10x10raster_ex_fine.ttl")