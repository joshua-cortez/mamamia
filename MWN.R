MWN = read.csv("Test MWN 1.csv", stringsAsFactors = FALSE)
MWN$DATE = as.Date(MWN$DATE, "%m/%d/%Y")
test = MWN %>% group_by(PAGEPATH) %>% summarize(publishDate = min(DATE))
MWN = merge(test, MWN, key = PAGEPATH)


library(RGoogleAnalytics)
library(dplyr)
library(data.table)
library(bigrquery)

########## PART I Downloading data from GA

token = Auth("649827186751-5jm6uau4l3vp46br5u6m93mpp188ocj5.apps.googleusercontent.com", "Tux6NPT0Si8vYbuWiipqerEq")
save(token,file="./token_file")
load("./token_file")
ValidateToken(token)

# the table.id of MWN is 105252640 while that of urbanwalkabout is 5156968 (these are the two views that are used)
#Note on the MWN view: the data in GA begins at 2015-07-15, so quering before that date will result in an error
dimensions = c("nthWeek", "pagePath", "medium", "hostname")
metrics = c("sessions", "bounces", "entrances", "uniquePageviews", "timeOnPage", "exits")

query.list <- Init(start.date = "2015-07-15",
                   end.date = "2015-09-30",
                   dimensions = "ga:nthWeek, ga:medium, ga:pagePath, ga:hostname",
                   metrics = "ga:sessions,ga:bounces,ga:entrances,ga:uniquePageviews,ga:timeOnPage,ga:exits",
                   max.results = 10000,
                   table.id = "ga:105252640")

ga.query <- QueryBuilder(query.list)

ga.data <- GetReportData(ga.query, token, paginate_query = TRUE)

###### PART II

# segmenting based on social, organic, and paid traffic
# not sure if cpc and CPC should also be included in paid
ga.data$medium = ifelse(ga.data$medium %in% c("referral", "social", "Social"), "social", ifelse(ga.data$medium == "organic", "organic", ifelse(ga.data$medium %in% c("paid", "Paid"), "paid", "other"))) 
ga.data = group_by(ga.data, date, pagePath, hostname, medium) %>% summarise_each_(funs(sum), vars = metrics)

# adding the new social and organic columns
Socialsessions = ifelse(ga.data$medium == "social", ga.data$sessions, 0)
SocialuniquePageviews = ifelse(ga.data$medium == "social", ga.data$uniquePageviews, 0)
Organicsessions = ifelse(ga.data$medium == "organic", ga.data$sessions, 0)
OrganicuniquePageviews = ifelse(ga.data$medium == "organic", ga.data$uniquePageviews, 0)
Paidsessions = ifelse(ga.data$medium == "paid", ga.data$sessions, 0)
PaiduniquePageviews = ifelse(ga.data$medium == "paid", ga.data$uniquePageviews, 0)

new_metrics = c(metrics, "Socialsessions", "SocialuniquePageviews", "Organicsessions", "OrganicuniquePageviews", "Paidsessions", "PaiduniquePageviews")
ga.data = cbind(ga.data, Socialsessions, SocialuniquePageviews, Organicsessions, OrganicuniquePageviews, Paidsessions, PaiduniquePageviews)
ga.data = group_by(ga.data, date, pagePath, hostname) %>% summarise_each_(funs(sum), vars = new_metrics)


###### PART III constructing the sections column and summarizing based on the new section dimension (USE ONLY when using sections in MWN)


# function defined to extract the sections from the page paths
extract_section = function(x){
  strsplit(x,split = "/")[[1]][2]
}

# adds a new column with the (raw) sections
ga.data = dplyr::mutate(ga.data, section = sapply(pagePath, extract_section))

# list of sections in MWN that are deemed relevant
# other sections not in this list will be classified as "other" later on
sections = c("advertise", "advertising", "author", "beauty", 
             "beauty-and-style", "brand", "career", "competitions", "competitions-and-giveaways", 
             "entertainment", "faqs", "finance", "fitness", "glow-team", "hair-and-nails", "health", 
             "inspiration", "jobs", "lifestyle", "news", "news-and-opinion", "parenting", "privacy-policy",
             "relationship-2", "relationships", "rogue", "social", 
             "style", "submissions", "wellbeing", "tag")

# collapses (row-wise) the ga.data table 
ga.data$section = ifelse(ga.data$section %in% sections, ga.data$section, "other")
#ga.data = filter(ga.data, section!= "other") 
ga.data = group_by(ga.data, date, pagePath, hostname, section) %>% summarise_each_(funs(sum), vars = colnames(ga.data)[-c(1:length(dimensions),ncol(ga.data))])

# computing the average time on page
ga.data = dplyr::mutate(ga.data, avgtimeonPage = ifelse(uniquePageviews == 0, 0, timeOnPage/uniquePageviews))
