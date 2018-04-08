# this will take all the .txt files and make them a nice data frame
# set working directory to where all .txt files are
# make sure that you don't have any other file in that directory
files <- list.files()
kanye <- data.frame(NA)
for (i in files) {
  row <- which(files == i)
  name <- gsub(".txt", "", i)
  album <- strsplit(name, "_")[[1]][[1]]
  song <- strsplit(name, "_")[[1]][[2]]
  kanye[row, 1] <- album
  kanye[row, 2] <- song
  kanye[row, 3] <- paste(readLines(i), collapse=" ")
}
colnames(kanye) <- c("album", "song", "lyrics")
kanye$album <- as.factor(kanye$album)
