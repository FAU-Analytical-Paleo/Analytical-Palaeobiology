#example for rattlesnake

rattler <- dismo::gbif('crotalus','horridus')


# get rid of occurences without location information
rattler=subset(rattler, !is.na(lon) & !is.na(lat))

# find and eliminate duplicate locations
rattlerdups=duplicated(rattler[, c("lon", "lat")])
rattler <-rattler[!rattlerdups, ]

rattlerocc <-  cbind.data.frame(rattler$lon,rattler$lat)

fold <- kfold(rattlerocc, k=5)
rattlertest <- rattlerocc[fold == 1, ]
rattlertrain <- rattlerocc[fold != 1, ]