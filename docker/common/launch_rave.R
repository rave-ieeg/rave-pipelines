# local({
#     ncpus <- as.integer(Sys.getenv("NCPUS", dipsaus::detectCores()))
#     if( is.na(ncpus) || ncpus < 1 ) {
#         ncpus <- 1
#     }
#     raveio::raveio_setopt("max_worker", ncpus)
# })

local({
    sys_env <- capture.output({print(Sys.getenv())})
    writeLines(sys_env, "/apps/shiny/envs.txt")
})

rave::start_rave2(launch.browser = FALSE)
