# TODO
# Removed because did not play nice with gsub:
# 2014-09-10
# 2014-10-04
# 2016-11-09
# 2016-11-16
# 2016-12-14
# 2017-01-25
# 2017-06-28
# 2017-07-26
# 2017-09-06
# 2017-10-11
# 2017-11-21
# 2018-02-28
# 2018-04-18
# 2018-05-23
# 2018-05-25
# 2018-06-27
# 2018-07-27
# 2019-03-13


files <- list.files("data/rollcall/", pattern = "txt")
files0 <- tolower(files)
files0 <- gsub("-", "", files)

# filename patter: "0123459_other_stuff.txt
pattern <- '^(\\d{8})\\w+.txt'
d <- strcapture(pattern, files0, data.frame(date = character()))
d$date <- as.Date(d$date, format = "%Y%m%d")

# filename pattern: "other_stuff_0123456.txt"
pattern <- '^\\w+(\\d{8}).+'
e <- strcapture(pattern, files0, data.frame(date = character()))
e$date <- as.Date(e$date, format = "%Y%m%d")

# filename pattern: as above but MDY instead of YMD
pattern <- '^\\D+([0-9-]+)\\D+'
f <- strcapture(pattern, files0, data.frame(date = character()))
f$date <- as.Date(f$date, format = "%m%d%Y")

# # filename pattern: "some_stuff_0.1.2.3.txt"
# pattern <- '^\\D+(\\d+\\.\\d+\\.\\d+])\\D+'
# g <- strcapture(pattern, files0, data.frame(date = character()))
# g$date <- as.Date(g$date, format = "%m%d%Y")


d$ID <- seq.int(nrow(d))
e$ID <- seq.int(nrow(e))
f$ID <- seq.int(nrow(f))

m <- merge(d, e, by = "ID", all = FALSE, sort = FALSE, no.dups = FALSE)
m <- merge(m, f, by = "ID", all = FALSE, sort = FALSE, no.dups = FALSE)
m$datedate <- paste0(m$date.x, m$date.y, m$date)
m <- m[,"datedate"]
m <- gsub("NA", "", m)

rollcall <- data.frame(name = character(),
                       date = character(),
                       stringsAsFactors = FALSE)

files <- files[!m == ""]
m <- m[!m == ""]
for(f in files){
    if(length(files) != length(m)) stop("Number of files and of dates not the same.")
    txtfile <- file(paste0("data/rollcall/",f), "r")
    t <- readLines(txtfile)
    close(txtfile)
    t <- paste0(unlist(t, recursive = FALSE), collapse = " ")
    #h <- gsub(".*Present -- ([\\w,]+)[[[:punct:]][[:space:]]]*Absent -- ([\\w,]+) --.*",
     #    "Present: \\1\nAbsent: \\2",t)
    present <- gsub(".*(?<=Present --)(.+)(?=-- [0-9]+\\. Absent).*",
                    "\\1", t, perl = TRUE)
    present <- gsub("Alderman", "", present)
    present <- gsub("Aldermen", "", present)
    present <- gsub("and", "", present)
    present <- strsplit(present, ",")
    present <- unlist(present)
    present <- trimws(present)
    date <- m[which(grepl(f, files))]
    date = rep(date, length(present))
    print(f)
    print(paste("date:", date))
    print(present)
    new <- data.frame(name = present,
                      date = date,
                      stringsAsFactors = FALSE)
    rollcall <- rbind(rollcall, new)
}

all_alds <- unique(rollcall$name)

attendance <- data.frame(name =rep(all_alds, length(m)),
                         date = rep(m, each = length(all_alds)),
                         stringsAsFactors = FALSE)
missing <- dplyr::setdiff(attendance, rollcall)
missing$attended <- FALSE
nonmissing <- dplyr::intersect(attendance, rollcall)
nonmissing$attended <- TRUE

attendance_v2 <- rbind(missing, nonmissing)
attendance_v2$date <- as.Date(attendance_v2$date)
attendance_v2 <- attendance_v2[order(attendance_v2$date),]
attendance_v2$date <- as.character(attendance_v2$date)

infile <- file("alderpeople_dictionary", "r")
current_alds <- readLines(infile)
close(infile)
current_alds <- current_alds[51:length(current_alds)]

attendance_v2$current <- FALSE
attendance_v2$current[!attendance_v2$name %in% current_alds] <- TRUE
attendance_v2 <- attendance_v2[order(attendance_v2$current),]

p <- ggplot(attendance_v2, aes(y = name, x = date)) +
    geom_raster(aes(fill = attended), alpha = .7) +
    labs(title = "Attendance at City Council meetings",
         x = "Date",
         y = "Alderman",
         fill = "Has attended") +
    scale_fill_discrete(labels = c("no", "yes")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = "attendance.png", plot = p,
       width = 35, height = 20, units = "cm", dpi = 300)

