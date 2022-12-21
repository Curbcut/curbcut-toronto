variables <- qs::qread("data/variables.qs")

id <- "tree"
var_left <- c("tree_count", "tree_ppt", "tree_sqkm")
var_right <- c(variables$var_code[variables$source == "Canadian census"], "canale", "canbics")
time <- 2019