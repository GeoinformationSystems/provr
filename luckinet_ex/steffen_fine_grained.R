
library("devtools")
load_all()
library("stringr")

init_provenance_graph(namespace = "https://www.idiv.de/de/luckinet.html/geokur/#")

landcover_classes <- list("forest", "grasland", "mountains")
landuse_classes <- list(list("forestry", "protected zone"), list("arable land", "pastures", "protected zone"), list("quary"))
ahIds <- list(111, 112, 113, 234, 453, 231)

# landcover_classes = list('forest')
# landuse_classes = list(list('forestry'))
# ahIds = list(111)


mp_cover_entity <- Entity("mp_cover", "land cover map")

# save for later
lc_limits_entities <- list()
lu_min_entities <- list()
lu_max_entities <- list()
for (i in 1:length(landcover_classes)) {
    landcover_class <- landcover_classes[i]
    for (landuse_class in unlist(landuse_classes[i])) {
        lc_limit <- paste(landcover_class, landuse_class, sep = "_")
        limit_entity <- Entity(
            id = paste("limits_", str_replace(lc_limit, " ", "-"), sep = ""),
            label = paste("limits:", str_replace(lc_limit, "_", " | "), sep = " ")
        )
        lc_limits_entities <- append(lc_limits_entities, limit_entity)
        reclassify_activity <- Activity(
            id = paste("reclassify_", str_replace(lc_limit, " ", "-"), sep = ""),
            label = ("reclassify")
        )
        lu_min_entity <- Entity(
            paste("min_", str_replace(lc_limit, " ", "-"), sep = ""),
            label = paste("minimum: ", str_replace(lc_limit, "_", " | "), sep = " ")
        )
        lu_min_entities <- append(lu_min_entities, lu_min_entity)
        lu_min_entity$wasGeneratedBy(reclassify_activity)
        lu_max_entity <- Entity(
            paste("max_", str_replace(lc_limit, " ", "-"), sep = ""),
            label = paste("maximum: ", str_replace(lc_limit, "_", " | "), sep = " ")
        )
        lu_max_entities <- append(lu_max_entities, lu_max_entity)
        lu_max_entity$wasGeneratedBy(reclassify_activity)

        reclassify_activity$used(limit_entity)
        reclassify_activity$used(mp_cover_entity)
    }
}





for (ahId in ahIds) {
    ahId_entity <- Entity(
        id = paste("ahId_", ahId, sep = ""),
        label = paste("ahID", ahId, sep = " ")
    )
    # census has same should be available at every ahID area, am i right?
    census_id_entity <- Entity(
        id = paste("censusId_", ahId, sep = ""),
        label = paste("census", ahId, sep = " ")
    )
    # i is for iterating through lu_min and lu_max entities
    i <- 1
    for (lc_limit_entity in lc_limits_entities) {
        build_local_suit_limits_activity <- Activity(
            id = paste("build_local_suit_map", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("build local suitability map")
        )
        build_local_suit_limits_activity$used(ahId_entity)
        build_local_suit_limits_activity$used(lc_limit_entity)
        suit_map_entity <- Entity(
            id = paste("suit_map", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("suitability map", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        suit_map_entity$wasGeneratedBy(build_local_suit_limits_activity)
        derive_suit_min_activity <- Activity(
            id = paste("derive_suit_min", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "derive siutability minimum"
        )
        derive_suit_min_activity$used(suit_map_entity)
        suit_min_entity <- Entity(
            id = paste("suit_map_minimum", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("suitability map minimum", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        suit_min_entity$wasGeneratedBy(derive_suit_min_activity)
        derive_suit_max_activity <- Activity(
            id = paste("derive_suit_max", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "derive suitability maximum"
        )
        derive_suit_max_activity$used(suit_map_entity)
        suit_max_entity <- Entity(
            id = paste("suit_map_maximum", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("suitability map maximum", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        suit_max_entity$wasGeneratedBy(derive_suit_max_activity)

        calc_map_sws_activity <- Activity(
            id = paste("calc_map_sws", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "calculate suitablity weighted supply",
            description = "calculate mp_sws (suitability weighted supply) by min-max-normalising mp_suit between LU_max and LU_min."
        )
        calc_map_sws_activity$used(suit_max_entity)
        calc_map_sws_activity$used(suit_min_entity)


        calc_map_sws_activity$used(lu_min_entities[[i]])
        calc_map_sws_activity$used(lu_max_entities[[i]])

        map_sws_entity <- Entity(
            id = paste("map_sws", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("map of suitablility weighted supply", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        map_sws_entity$wasGeneratedBy(calc_map_sws_activity)

        calc_supply_demand_ratio_activity <- Activity(
            id = paste("calc_supply_demand_ratio", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "calculate supply/demand ratio",
            description = "calculate the ratio between pure suitability weighted supply and the actual demand (census in that area)."
        )
        calc_supply_demand_ratio_activity$used(map_sws_entity)
        calc_supply_demand_ratio_activity$used(census_id_entity)


        ratio_entity <- Entity(
            id = paste("ratio_sws_census", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("ratio supply demand", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        ratio_entity$wasGeneratedBy(calc_supply_demand_ratio_activity)

        keep_lu_limit_activity <- Activity(
            id = paste("keep_lu_limit", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "keep LU limit",
        )
        recalculate_lu_limit_activity <- Activity(
            id = paste("recalc_lu_limit", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "recalculate LU limit",
        )

        # new lu mins and max for each combination of ah id and according lc limits?
        lu_minc_entity <- Entity(
            id = paste("lu_minc", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("LU minc", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        lu_maxc_entity <- Entity(
            id = paste("lu_maxc", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("LU maxc", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )

        # simulate calculations
        val <- rnorm(1, mean = 1)

        if (val < 1) {
            keep_lu_limit_activity$used(lu_min_entities[[i]])
            keep_lu_limit_activity$used(ratio_entity)
            lu_minc_entity$wasGeneratedBy(keep_lu_limit_activity)

            recalculate_lu_limit_activity$used(lu_max_entities[[i]])
            recalculate_lu_limit_activity$used(ratio_entity)

            lu_maxc_entity$wasGeneratedBy(recalculate_lu_limit_activity)
        }
        else {
            keep_lu_limit_activity$used(lu_max_entities[[i]])
            keep_lu_limit_activity$used(ratio_entity)
            lu_maxc_entity$wasGeneratedBy(keep_lu_limit_activity)

            recalculate_lu_limit_activity$used(lu_min_entities[[i]])
            recalculate_lu_limit_activity$used(ratio_entity)
            lu_minc_entity$wasGeneratedBy(recalculate_lu_limit_activity)
        }

        calc_map_swsc_activity <- Activity(
            id = paste("calc_corrected_map_sws", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = "calculate corrected suitablity weighted supply",
            description = "LU_minc and LU_maxc are used to also calculate corrected mp_sws (mp_swsc). What happens here is that if there was more demand than supply in a given territory (and the lower limit was increased), mp_swsc will have higher values than mp_sws in that territory, and vice versa, it will have lower values. Finally, when summarising all values of a territory, they shall sum up to what had been specified in the actual demand. (Basically, I combine suitability with the census stats to get a map where the spatial pattern of the allocated amounts is determined by the relative differences in suitability (within the confines of the used landcover map) and where all pixels of a particular region sum up to the amount of demand for that area.)"
        )
        calc_map_swsc_activity$used(suit_max_entity)
        calc_map_swsc_activity$used(suit_min_entity)
        calc_map_swsc_activity$used(lu_minc_entity)
        calc_map_swsc_activity$used(lu_maxc_entity)

        map_swsc_entity <- Entity(
            id = paste("corrected_map_sws", paste(ahId, lc_limit_entity$get_id(), sep = "_"), sep = "_"),
            label = paste("corrected map of suitablility weighted supply", paste(ahId, str_replace(lc_limit_entity$get_label(), "limits:", " | "), sep = " "), sep = " "),
        )
        map_swsc_entity$wasGeneratedBy(calc_map_swsc_activity)
        i <- i + 1
    }
}

serialize_provenance_graph(name = "steffen_fine_grained.ttl")