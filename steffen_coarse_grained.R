
library("devtools")
load_all()
library("stringr")

init_provenance_graph(namespace = "https://www.idiv.de/de/luckinet.html/geokur/#")

lc_limits_entity <- Entity(id = "lc_limits", label = "land cover limits")
mp_cover_entity <- Entity("mp_cover", "land cover map")

det_lu_lim_activity <- Activity("determine_landuse_limits", "determine land use limits")
det_lu_lim_activity$used(lc_limits_entity)
det_lu_lim_activity$used(mp_cover_entity)

lu_min_entity <- Entity("lu_min", "land use minimum")
lu_min_entity$wasGeneratedBy(det_lu_lim_activity)

lu_max_entity <- Entity("lu_max", "land use maximum")
lu_max_entity$wasGeneratedBy(det_lu_lim_activity)

mp_ahIds_entity <- Entity("mp_ahIDs", "administrive areas")

mp_suit_entity <- Entity("mp_suit", "suitability map")

det_suit_lim_activity <- Activity("determine_suitability_limits", "determine suitability limits")
det_suit_lim_activity$used(mp_ahIds_entity)
det_suit_lim_activity$used(mp_suit_entity)
det_suit_lim_activity$used(lc_limits_entity)

suit_min_entity <- Entity("suit_min", "suitability minimum")
suit_min_entity$wasGeneratedBy(det_suit_lim_activity)

suit_max_entity <- Entity("suit_max", "suitability maximum")
suit_max_entity$wasGeneratedBy(det_suit_lim_activity)

pre_alloc_activity <- Activity("pre_alloc", "pre allocate", "min-max normalisation")
pre_alloc_activity$used(lu_min_entity)
pre_alloc_activity$used(lu_max_entity)
pre_alloc_activity$used(suit_min_entity)
pre_alloc_activity$used(suit_max_entity)

mp_sws_entity <- Entity("suitability_weighted_supply_map", "suiatability weighted supply map")
mp_sws_entity$wasGeneratedBy(pre_alloc_activity)

census_areas_entity <- Entity("census_areas", "census area")

det_corr_activity <- Activity("determine_correction_factors", "determine correction factors", "correct limits based on supply/demand ratio")
det_corr_activity$used(mp_ahIds_entity)
det_corr_activity$used(mp_sws_entity)
det_corr_activity$used(lc_limits_entity)
det_corr_activity$used(census_areas_entity)

lu_min_c_entity <- Entity("lu_min_c", "land use minimum corrected")
lu_min_c_entity$wasGeneratedBy(det_corr_activity)

lu_max_c_entity <- Entity("lu_max_c", "land use maximum corrected")
lu_max_c_entity$wasGeneratedBy(det_corr_activity)

allocate_activity <- Activity("allocate", "allocate")
allocate_activity$used(mp_suit_entity)
allocate_activity$used(suit_min_entity)
allocate_activity$used(suit_max_entity)
allocate_activity$used(lu_min_c_entity)
allocate_activity$used(lu_max_c_entity)

allocated_data_entity <- Entity("allocated_data", "allocated data")
allocated_data_entity$wasGeneratedBy(allocate_activity)

serialize_provenance_graph(name = "steffen_coarse_grained.ttl")