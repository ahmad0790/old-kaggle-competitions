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

merged$object <- as.character(merged$object)
merged$course_id <- as.character(merged$course_id)
object$module_id <- as.character(object$module_id)
object$course_id <- as.character(object$course_id)

merged <- left_join(merged, object
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

merged <-
  merged %>%
  group_by(course_id) %>%
  mutate(num_modules = length(unique(object))
         #,num_children = length(unique(children))
         ,course_start = min(Date)
         ,course_end = max(Date))

merged <-
  merged %>%
  group_by(enrollment_id,Date) %>%
  mutate(num_videos_day = sum(eventvideo)
         ,num_navigate_day = sum(eventnavigate)
         ,num_access_day = sum(eventaccess)
         ,num_problem_day = sum(eventproblem)
         ,num_page_close_day = sum(eventpage_close)
         ,num_discussion_day = sum(eventdiscussion)
         ,num_wiki_day = sum(eventwiki)
         ,num_events_day = length(eventvideo)
         ,video_time_enroll_day = sum(video_time)
         ,problem_time_enroll_day =  sum(problem_time)
         ,discussion_time_enroll_day =  sum(discussion_time)
         ,modules_accessed_day = length(unique(object))
  )

merged <-
  merged %>%
  group_by(enrollment_id) %>%
  mutate(sum_session_3hrs = sum(sessions_3hrs)
         ,sum_session_1hr = sum(sessions_1hr)
  )
#enroll vars
merged <-
  merged %>%
  group_by(enrollment_id) %>%
  mutate(enrollment_start_date = min(Date)
        ,enrollment_last_date = max(Date)
        
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
        ,video_time_enroll_avg = mean(video_time)
        ,problem_time_enroll =  sum(problem_time)
        ,problem_time_enroll_avg =  mean(problem_time)
        ,discussion_time_enroll =  sum(discussion_time)
        ,discuss_time_enroll_avg = mean(discussion_time)
        
        ,modules_accessed = length(unique(object))
        
        #,children_accessed = length(unique(children))
        ,access_since_open = as.numeric(min(Date) - course_start)
        ,days_to_end = as.numeric(course_end - max(Date))
        ,avg_hour = mean(hour)
        ,sum_session_3hrs = sum(sessions_3hrs)
        ,sum_session_1hr = sum(sessions_1hr)
        )
  
merged$enrollment_length <- ifelse(merged$enrollment_length == 0,1,as.numeric(merged$enrollment_length))        
merged$access_since_open <- ifelse(merged$access_since_open == 0,1,as.numeric(merged$access_since_open))        
merged$days_enrolled <- as.numeric((merged$Date - merged$enrollment_start_date))                              
#merged$weeks_enrolled <- floor(merged$days_enrolled/7)

merged$modules_p <- merged$modules_accessed/merged$num_modules
#merged$children_p <- merged$children_accessed/merged$num_children

##user vars
merged <-
  merged %>%
  group_by(username) %>%
  mutate(total_courses = length(unique(course_id))
         ,total_modules = length(unique(object))
         ,avg_videos_user = sum(eventvideo) #/total_courses
         ,avg_navigate_user = sum(eventnavigate)#/total_courses
         ,avg_access_user = sum(eventaccess)#/total_courses
         ,avg_problem_user = sum(eventproblem)#/total_courses
         ,avg_page_close_user = sum(eventpage_close)#/total_courses
         ,avg_discussion_user = sum(eventdiscussion)#/total_courses
         ,avg_wiki_user = sum(eventwiki)#/total_courses
         ,user_total_events = length(event)
         
         ,user_activity_age = max(Date) - min(Date)
         ,video_time_user = sum(video_time)
         
         ,problem_time_user =  sum(problem_time)
         ,discussion_time_user =  sum(discussion_time)
         #,course_complete_p = (total_courses - sum(dropout))/total_courses
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
#write.table(merged, "merged_dropout.csv",sep=",")

merged$module_date <- as.Date(merged$start)

merged <-
  merged %>%
  group_by(enrollment_id,object) %>%
  mutate(access_since_module_st = as.numeric(min(Date) - min(module_date))
         ,module_completion_days = as.numeric(max(Date) - min(Date))
  )

merged <- as.data.frame(merged)


###model
library(randomForest)
rf_dropout <- randomForest(dropout~.,data=enrollment_train[,4:13], importance=TRUE
                           ,ntree=500, do.Trace=T)

predict_rf_dropout <- predict(rf_dropout, enrollment_test, type="prob")
predict_rf_dropout <- as.numeric(predict_rf_dropout[,2])
subm <- data.frame(cbind(enrollment_test$enrollment_id,predict_rf_dropout))
write.csv(subm, "submission.csv",col.names = FALSE, row.names=FALSE)
