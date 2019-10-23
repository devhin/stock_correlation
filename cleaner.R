# recuperer tous les .zip
zip_files <- list.files("2014_2016", pattern = "\\.ZIP$")
names <- gsub(".{4}$", "", zip_files)
# finess <- substr(names, start = 1, stop = 9)
# print(paste0(i, " _ ", finess[i]))

# pour chaque zip : extraire le html
for (zip in zip_files) {
  path_zip <- paste0("2014_2016/", zip)
  path_unzipped <- "2014_2016/unzipped"
  unzip(path_zip, exdir = path_unzipped, unzip = "internal")
  }

unzipped_files <- list.files("2014_2016/unzipped")

isin_cote_continue <- subset(read.delim(paste0("2014_2016/unzipped/", unzipped_files[1]), header = FALSE, sep = "\t", dec = "."), V7>5000)$V1
for (i in 1:length(unzipped_files)) {
  temp <- subset(read.delim(paste0("2014_2016/unzipped/", unzipped_files[i]), header = FALSE, sep = "\t", dec = "."), V7>5000)$V1
  isin_cote_continue <- intersect(isin_cote_continue,temp)
}
length(isin_cote_continue)

data_in <- data.frame(matrix(0, length(unzipped_files), length(isin_cote_continue)), stringsAsFactors=F)
colnames(data_in) <- isin_cote_continue

for (i in 1:length(unzipped_files)) {
# for (i in 1:10) {
  temp_jour <- read.delim(paste0("2014_2016/unzipped/", unzipped_files[i]), header = FALSE, sep = "\t", dec = ".")
  for (isin in isin_cote_continue) {
    temp_ligne <- subset(temp_jour, V1==isin)
    temp_val <- temp_ligne[1,5] + (temp_ligne[1,4] - temp_ligne[1,5])/2
    data_in[[i, isin]] <- temp_val
  }
}

write.xlsx(data_in, "test.xlsx")
