build_time <- Sys.time()
build_version <- 1.0

s <- readLines("./src/js/class-shidashi.js")
idx <- which(startsWith(trimws(s), "// Insert build version here")) + 1
s[idx] <- sprintf("    this.build = { version: '%.1f', date: '%s' };", build_version, strftime(build_time, usetz = TRUE))
writeLines(s, "./src/js/class-shidashi.js")
system("npx webpack")

shidashi_path <- "./www/shidashi/js"
old_build <- list.files(shidashi_path, pattern = "^shidashi-.*\\.js$", ignore.case = TRUE, include.dirs = FALSE)
if(length(old_build)) {
  for(f in old_build) {
    unlink(file.path(shidashi_path, f))
  }
}

new_filename <- sprintf("shidashi-v%.1f.%s.js", build_version, strftime(build_time, "%y%m%d.%H%M%S"))

file.rename(
  from = file.path(shidashi_path, "shidashi.js"),
  to = file.path(shidashi_path, new_filename)
)

# viewer/header.html
header_src <- readLines("./views/header.html")
idx <- which(startsWith(trimws(header_src), '<script src="shidashi/js'))[[1]]
header_src[[idx]] <- sprintf('<script src="shidashi/js/%s"></script>', new_filename)
writeLines(header_src, "./views/header.html")
