library(downloader)

library(rinat)

urm <- get_inat_obs_user(username = "urban-rural-moths", maxresults = 200)
k <- get_inat_obs_user(username = "kaushik2")

total_obs <- rbind(urm, k)


iurl <- as.character(total_obs$image_url)
names <- total_obs$id

for (i in 67:nrow(total_obs)) {
  download(url = iurl[i], destfile=paste0('downloaded_photos/', names[i],'.jpg'), mode = 'wb')
}
