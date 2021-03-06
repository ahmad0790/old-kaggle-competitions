setwd("/Users/ahkhan/Documents/KDD")
library(data.table)
library(dplyr)

object <- read.csv("object.csv")
enrollment_train <- read.csv("enrollment_train.csv")
truth_train <- read.csv("truth_train.csv",header=FALSE)
colnames(truth_train) <- c("enrollment_id", "dropout")
submission <- read.csv("sampleSubmission.csv", header=FALSE)
enrollment_test <- read.csv("enrollment_test.csv")

log_test <- fread("log_test.csv")
log_train <- fread("log_train.csv")

###DATA###
train <- merge(log_train, enrollment_train, by= c("enrollment_id"),all.x=TRUE)
train <- merge(train, truth_train, by="enrollment_id",all.x=TRUE)
train$train_ind <- 1
train <- as.data.frame(train)
test <- merge(log_test, enrollment_test, by= c("enrollment_id"),all.x=TRUE)
test$dropout <- NA
test$train_ind <- 0
merged <- rbind(train, test)

events <- with(merged, data.frame(model.matrix(~event+0)))
merged <- cbind(merged, events)
merged <- as.data.frame(merged)

#merged$object <- as.character(merged$object)
#merged$course_id <- as.character(merged$course_id)
#object$module_id <- as.character(object$module_id)
#object$course_id <- as.character(object$course_id)

#merged <- left_join(merged, object
#,by = c("course_id" = "course_id", "object"="module_id"))

#write.csv(merged, "merged_joined.csv")

##feature engineering##
library(DataCombine)
merged <- slide(merged,Var= "time",GroupVar="enrollment_id",slideBy=1,NewVar = "end_time")

library(lubridate)
merged$time <- ymd_hms(gsub("T", " ", merged$time))
merged$end_time <- ymd_hms(gsub("T", " ", merged$end_time))   
merged$Date <- as.Date(merged$time)
merged$end_time <- as.POSIXct(merged$end_time)
merged$hour <- hour(merged$time)
merged$month <- month(merged$time)
merged$weekday <- wday(merged$time)

##event duration in seconds
merged$event_duration <- as.numeric(merged$end_time - merged$time)
merged$event_duration <- ifelse(is.na(merged$event_duration),0,merged$event_duration)

merged$video_time = as.numeric(ifelse(merged$event == 'video',as.numeric(merged$event_duration),0))
merged$problem_time = as.numeric(ifelse(merged$event == 'problem',as.numeric(merged$event_duration),0))
merged$discussion_time = as.numeric(ifelse(merged$event == 'discussion',as.numeric(merged$event_duration),0))

video_limit <- matrix(quantile(merged$video_time,0.993,na.rm=TRUE))[1,1]
discussion_limit <- matrix(quantile(merged$discussion_time,0.993,na.rm=TRUE))[1,1]
problem_limit <- matrix(quantile(merged$problem_time,0.993,na.rm=TRUE))[1,1]

merged$video_time <- as.numeric(ifelse(merged$video_time > 3937,3937,as.numeric(merged$video_time)))
merged$discussion_time <- as.numeric(ifelse(merged$discussion_time > discussion_limit,discussion_limit,as.numeric(merged$discussion_time)))
merged$problem_time <- as.numeric(ifelse(merged$problem_time > problem_limit,problem_limit,as.numeric(merged$problem_time)))

merged$sessions_3hrs <- ifelse(merged$event_duration >= 10800,1,0)
merged$sessions_1hr <- ifelse(merged$event_duration >= 3600,1,0)

##course vars
merged <-
  merged %>%
  group_by(course_id) %>%
  mutate(num_modules = length(unique(object))
         ,course_start = min(Date)
         ,course_end = max(Date))

#enroll vars
merged <-
  merged %>%
  group_by(enrollment_id) %>%
  mutate(enrollment_start_date = min(Date)
         ,enrollment_last_date = max(Date)
         ,enrollment_last_week_cutoff = enrollment_last_date -7
         
         ,num_videos = sum(eventvideo)
         ,num_navigate = sum(eventnavigate)
         ,num_access = sum(eventaccess)
         ,num_problem = sum(eventproblem)
         ,num_page_close = sum(eventpage_close)
         ,num_discussion = sum(eventdiscussion)
         ,num_wiki = sum(eventwiki)
         ,num_events = length(eventvideo)
         
         ,enrollment_length = as.numeric(max(Date) - min(Date))
         ,unqiue_days_accessed = length(unique(Date))
         
         ,video_time_enroll = sum(video_time)
         ,problem_time_enroll =  sum(problem_time)
         ,discussion_time_enroll =  sum(discussion_time)
         
         ,modules_accessed = length(unique(object))
         
         ,access_since_open = as.numeric(min(Date) - course_start)
         ,days_to_end = as.numeric(course_end - max(Date))
         ,avg_hour = mean(hour)
         ,sum_session_3hrs = sum(sessions_3hrs)
         ,sum_session_1hr = sum(sessions_1hr)
         
         ,events_last_week = sum(ifelse(Date >= enrollment_last_week_cutoff,1,0))
         ,modules_last_week = length(unique((ifelse(Date >= enrollment_last_week_cutoff,object,0))))
  )

merged$enrollment_length <- ifelse(merged$enrollment_length == 0,1,as.numeric(merged$enrollment_length))        
merged$access_since_open <- ifelse(merged$access_since_open == 0,1,as.numeric(merged$access_since_open))        
merged$days_enrolled <- as.numeric((merged$Date - merged$enrollment_start_date))                              

merged$modules_p <- merged$modules_accessed/merged$num_modules

##user vars
merged <-
  merged %>%
  group_by(username) %>%
  mutate(total_courses = length(unique(course_id))
         ,total_modules = length(unique(object))
         ,avg_videos_user = sum(eventvideo) /total_courses
         ,avg_navigate_user = sum(eventnavigate)/total_courses
         ,avg_access_user = sum(eventaccess)/total_courses
         ,avg_problem_user = sum(eventproblem)/total_courses
         ,avg_page_close_user = sum(eventpage_close)/total_courses
         ,avg_discussion_user = sum(eventdiscussion)/total_courses
         ,avg_wiki_user = sum(eventwiki)/total_courses
         ,user_total_events = length(event)/total_courses
         
         ,user_activity_age = max(Date) - min(Date)
         ,video_time_user = sum(video_time)  
         ,problem_time_user =  sum(problem_time)
         ,discussion_time_user =  sum(discussion_time)
  )

merged$user_activity_age <- as.numeric(merged$user_activity_age)

##course vars
merged <-
  merged %>%
  group_by(course_id) %>%
  mutate(total_enroll_course = length(unique(enrollment_id))
         ,avg_videos_course = sum(eventvideo)/total_enroll_course
         ,avg_navigate_course = sum(eventnavigate)/total_enroll_course
         ,avg_access_course = sum(eventaccess)/total_enroll_course
         ,avg_problem_course = sum(eventproblem)/total_enroll_course
         ,avg_page_close_course = sum(eventpage_close)/total_enroll_course
         ,avg_discussion_course = sum(eventdiscussion)/total_enroll_course
         ,avg_wiki_course = sum(eventwiki)/total_enroll_course
         ,avg_enrollment_length = mean(enrollment_length)
  )

merged <- as.data.frame(merged)

library(sqldf)
##this selection of vars get 0.8936
enrollment_agg <- sqldf("select
                        enrollment_id
                        
                        ,avg(num_videos) as num_videos
                        ,avg(num_navigate) as num_navigate
                        ,avg(num_access) as num_access
                        ,avg(num_problem) as num_problem
                        ,avg(num_page_close) as num_page_close
                        ,avg(num_discussion) as num_discussion
                        ,avg(num_wiki) as num_wiki
                        ,avg(num_events) as num_events
                        
                        ,avg(enrollment_length) as enrollment_length
                        ,avg(unqiue_days_accessed) as unqiue_days_accessed
                        
                        ,avg(video_time_enroll) as video_time_enroll
                        ,avg(problem_time_enroll) as problem_time_enroll
                        ,avg(discussion_time_enroll) as discussion_time_enroll
                        
                        ,avg(access_since_open) as access_since_open
                        ,avg(modules_p) as modules_p
                        ,avg(days_enrolled) as days_enrolled
                        ,avg(days_to_end) as days_to_end
                        
                        ,avg(total_courses) as total_courses
                        ,avg(total_modules) as total_modules
                        ,avg(avg_videos_user) as avg_videos_user
                        ,avg(avg_navigate_user) as avg_navigate_user
                        ,avg(avg_access_user) as avg_access_user
                        ,avg(avg_problem_user) as avg_problem_user
                        ,avg(avg_page_close_user) as avg_page_close_user
                        ,avg(avg_discussion_user) as avg_discussion_user
                        ,avg(avg_wiki_user) as avg_wiki_user
                        ,avg(user_activity_age) as user_activity_age
                        ,avg(user_total_events) as user_total_events
                        ,avg(video_time_user) as video_time_user
                        ,avg(problem_time_user) as problem_time_user
                        ,avg(discussion_time_user) as discussion_time_user
                        
                        ,avg(total_enroll_course) as total_enroll_course
                        ,avg(avg_videos_course) as avg_videos_course
                        ,avg(avg_navigate_course) as avg_navigate_course
                        ,avg(avg_access_course) as avg_access_course
                        ,avg(avg_problem_course) as avg_problem_course
                        ,avg(avg_page_close_course) as avg_page_close_course
                        ,avg(avg_discussion_course) as avg_discussion_course
                        ,avg(avg_wiki_course) as avg_wiki_course
                        
                        ,avg(avg_enrollment_length) as avg_enroll_course
                        
                        ,avg(avg_hour) as avg_hour
                        ,avg(month) as avg_month
                        ,avg(weekday) as avg_weekday
                        ,avg(event_duration) as avg_event_duration
                        ,avg(sessions_3hrs) as sessions_3hrs
                        ,avg(sessions_3hrs) as sessions_1hr
                        
                        ,avg(events_last_week) as events_last_week
                        ,avg(modules_last_week) as modules_last_week
                        
                        from merged
                        group by 1
                        
                        ")

enrollment_backup <- enrollment_agg

enrollment_agg <- enrollment_backup
library(caret)
trans <- preProcess(enrollment_agg[,2:ncol(enrollment_agg)], method = c("BoxCox","center", "scale","pca"))
enrolltrans <-  predict(trans, enrollment_agg[,2:ncol(enrollment_agg)])
enrollment_agg <- cbind(enrollment_id = enrollment_agg$enrollment_id
                        ,enrolltrans)


enrollment_train_agg <- merge(enrollment_train, truth_train, by="enrollment_id", all.x=TRUE)
enrollment_train_agg <- merge(enrollment_train_agg, enrollment_agg, by="enrollment_id", all.x=TRUE)
enrollment_test_agg <- merge(enrollment_test, enrollment_agg, by="enrollment_id", all.x=TRUE)
enrollment_train_agg$dropout <- as.factor(enrollment_train_agg$dropout)

###object vars
object$module_start <- ymd_hms(gsub("T", " ", object$start))
library(plyr)
module_courses <- ddply(object,
                        ~course_id
                        ,summarise
                        ,module_start = mean(module_start,na.rm=T)
                        ,children = length(unique(children))
                        ,num_modules = length(unique(module_id))
)
module_courses$module_hour <- hour(module_courses$module_start)
module_courses$module_weekday <- wday(module_courses$module_start)

enrollment_train_agg <- left_join(enrollment_train_agg, module_courses, by="course_id")
enrollment_test_agg <- left_join(enrollment_test_agg, module_courses, by="course_id")

enrollment_train_agg$module_start <- NULL
enrollment_test_agg$module_start <- NULL

detach(package: plyr)
