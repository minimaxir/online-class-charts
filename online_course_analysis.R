source("Rstart.R")

data <- tbl_df(read.csv("HMXPC13_DI_v2_5-14-14.csv", header=T))

data <- data %>% mutate(
  age = 2013 - YoB,
  highest_level = ifelse(certified==1,"Certified",ifelse(explored==1,"Explored",ifelse(viewed==1,"Viewed","Registered"))),
  course_id = as.character(lapply(as.character(course_id), function(x) { 
    return (strsplit(x,"/")[[1]][2])
  }))
)

user_data <- data %>%
  group_by(userid_DI, LoE_DI, age, gender) %>%
  summarize(num_courses_taken = n(),
            num_courses_completed = sum(certified)
  )

### How many courses do students take?

course_count <- user_data %>%
  filter(num_courses_taken <= 13) %>%
  group_by(num_courses_taken) %>%
  summarize(count=n()) %>%
  mutate(perc = count/sum(count))

perc_labels <- paste(format(course_count$perc*100,digits=0,scientific = FALSE),"%",sep='')

ggplot(aes(x=num_courses_taken, y=count), data=course_count) +
  geom_bar(stat="identity") +
  theme_custom() +
  #coord_flip() +
  geom_text(label = perc_labels, color = "#1a1a1a", vjust=-0.5, family=fontFamily, size=2) + 
  scale_x_discrete(limits=seq(1,13,1)) + 
  scale_y_continuous(labels=comma) +
  labs(y = "# of Students", title=paste("Distribution of # Courses Taken by",format(sum(course_count$count), big.mark=","),"Students"), x="# of Courses Taken")
  
ggsave("student-courses.png", dpi=300, width=4, height=3)

### Analysis of Course Data

course_names <- data.frame(course_id=c("CB22x", "CS50x", "ER22x", "PH207x","PH278x","14.73x","2.01x","3.091x","6.002x","6.00x","7.00x","8.02x","8.MReV"),
                           school=c(rep("Harvard",5), rep("MIT",8)),
                           name=c("The Ancient Greek Hero", "Intro to Computer Science", "Justice", "Health in Numbers", "Human Health & Environment", "Challenges of Global Poverty", "Elements of Structures", "Intro to Solid State Chemistry","Circuits and Electronics","Intro to CS/Programming","Intro to Biology","Electricity and Magnetism","Mechanics Review"))

by_course <- data %>%
  group_by(course_id) %>%
  summarize(count = n(),
            avg_age = mean(age,na.rm=T),
            courses_completed = sum(certified),
            courses_uncompleted = count - courses_completed,
            avg_fulfillment = courses_completed / count
  ) %>%
  arrange(desc(count))

by_course <- tbl_df(merge(by_course, course_names))
by_course <- tbl_df(by_course) %>% arrange(desc(count))
harvard_color = "#A51C30"
mit_color = "black"


# How many students are in each class?

temp_df <- data %>% group_by(course_id, highest_level) %>% summarize(count=n())
temp_df <- tbl_df(merge(temp_df, course_names))
temp_df$name <- factor(temp_df$name, levels = rev(by_course$name), ordered=T)
temp_df$highest_level <- factor(temp_df$highest_level, levels=c("Registered","Viewed","Explored","Certified"), ordered=T)

label_color <- rev(ifelse(by_course$school=="Harvard",harvard_color,mit_color))

ggplot(aes(x = name, y = count, fill = highest_level, order=highest_level), data=temp_df) + 
  geom_bar(stat="identity") +
  theme_custom() +
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_text(color=label_color)) + 
  labs(y = "# of Students", title="Attendance of Harvard/MIT's Online Classes") + 
  scale_fill_manual("Class Status",
                      values=c(colors[2:5])
  )

ggsave("class-attendance.png", dpi=300, width=4, height=3)

# What % of classes are finished once started?

by_course <- tbl_df(by_course) %>% arrange(desc(avg_fulfillment))
by_course$name <- reorder(by_course$name,by_course$avg_fulfillment)
perc_labels <- paste(format(by_course$avg_fulfillment*100,digits=1),"%",sep='')

label_color <- rev(ifelse(by_course$school=="Harvard",harvard_color,mit_color))

ggplot(aes(x=name, y= as.numeric(avg_fulfillment)), data=by_course) +
  geom_bar(fill = label_color, stat="identity") +
  theme_custom() +
  coord_flip() +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=2) + 
  scale_y_continuous(labels = percent, breaks=seq(0,0.08,by=0.01)) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_text(color=label_color)) + 
  labs(y = "Completion Rate (# Completed / # Taking Class)", title="Completion Rate of Harvard/MIT's Online Classes")

ggsave("class-perc-finished.png", dpi=300, width=4, height=3)

# What is the average grade for those who have completed the classes?

by_course_completed <- data %>%
  filter(certified == 1) %>%
  group_by(course_id) %>%
  summarize(count = n(),
            avg_grade = mean(grade,na.rm=T)
  )

by_course_completed <- tbl_df(merge(by_course_completed, course_names))
by_course_completed <- tbl_df(by_course_completed) %>% arrange(desc(avg_grade))
by_course_completed$name <- reorder(by_course_completed$name,by_course_completed$avg_grade)

perc_labels <- paste(format(by_course_completed$avg_grade*100,digits=1),"%",sep='')
label_color <- rev(ifelse(by_course_completed$school=="Harvard",harvard_color,mit_color))

ggplot(aes(x=name, y= as.numeric(avg_grade)), data=by_course_completed) +
  geom_bar(fill = label_color, stat="identity") +
  theme_custom() +
  coord_flip() +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=2) + 
  scale_y_continuous(labels = percent) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_text(color=label_color)) + 
  labs(y = "Avg Student Grade", title="Grades of Completed Harvard/MIT Online Classes")

ggsave("class-perc-grade.png", dpi=300, width=4, height=3)


### Does Age impact attendance/success?

second_color = colors[3]

by_age <- user_data %>%
  filter(!is.na(age) & age > 12) %>%
  group_by(age) %>%
  summarize(count = n(),
            avg_courses_taken = mean(num_courses_taken),
            perc_more_than_one_class = sum(num_courses_taken > 1) / count,
            avg_courses_completed = mean(num_courses_completed),
            avg_fulfillment = avg_courses_completed / avg_courses_taken,
            perc_m = sum(gender=='m') / (sum(gender=='m') + sum(gender=='f'))
  ) %>%
  arrange(age)

ggplot(aes(x=age, y=count), data=by_age) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  scale_x_continuous(breaks=seq(0,80,by=10)) + 
  scale_y_continuous(labels=comma) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("Ages of",format(sum(by_age$count), big.mark=","),"Students of MIT/Harvard Online Classes"), x="Age", y="# of Students")

ggsave("student-age.png", dpi=300, width=4, height=3)

ggplot(aes(x=age, y=perc_more_than_one_class), data=by_age) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  scale_x_continuous(breaks=seq(0,80,by=10)) + 
  scale_y_continuous(labels = percent) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("% of Students of MIT/Harvard Online Classes Taking >1 Class"), x="Age of Student", y="% of Students in each Age Group Taking >1 Class")

ggsave("student-num-classes.png", dpi=300, width=4, height=3)

ggplot(aes(x=age, y=avg_fulfillment), data=by_age) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  scale_x_continuous(breaks=seq(0,80,by=10)) + 
  scale_y_continuous(labels = percent, breaks=seq(0,.07,by=.01)) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title="% of MIT/Harvard Online Classes Completed, by Age Group", x="Age of Student", y="% of Classes Completed in each Age Group")

ggsave("student-fulfillment.png", dpi=300, width=4, height=3)



### Education and Participation

second_color = colors[4]

by_education <- user_data %>%
  filter(!is.na(LoE_DI) & LoE_DI != '') %>%
  group_by(LoE_DI) %>%
  summarize(count = n(),
            avg_age = mean(age,na.rm=T),
            avg_courses_taken = mean(num_courses_taken),
            avg_courses_completed = mean(num_courses_completed),
            avg_fulfillment = avg_courses_completed / avg_courses_taken,
            perc_f = sum(gender=='f') / (sum(gender=='m') + sum(gender=='f'))
  ) %>%
  arrange(desc(count))

by_education$LoE_DI <- gsub("Less than Secondary", "< Secondary", by_education$LoE_DI)
by_education$LoE_DI <- factor(by_education$LoE_DI, levels=rev(c("< Secondary", "Secondary", "Bachelor's", "Master's", "Doctorate")), ordered=T)

ggplot(aes(x=LoE_DI, y=count), data=by_education) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("Education Levels of",format(sum(by_education$count), big.mark=","),"Students in Online Classes"), x="Level of Education", y="# of Students")

ggsave("education-count.png", dpi=300, width=4, height=3)

ggplot(aes(x=LoE_DI, y=avg_age), data=by_education) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  geom_text(label = format(by_education$avg_age,digits=3), color = "white", hjust=1.5, family=fontFamily, size=4) + 
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title="Average Ages of Students in MIT/Harvard Online Classes", x="Level of Education", y="Average Age of Students per LoE")

ggsave("education-age.png", dpi=300, width=4, height=3)

perc_labels <- paste(format(by_education$avg_fulfillment*100,digits=3),"%",sep='')

ggplot(aes(x=LoE_DI, y=avg_fulfillment), data=by_education) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=3) + 
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title="Completion Rate by Education in MIT/Harvard Online Classes", x="Level of Education", y="% of Classes Completed per LoE")

ggsave("education-fulfillment.png", dpi=300, width=4, height=3)

by_education_completed <- data %>%
  filter(!is.na(LoE_DI) & LoE_DI != '' & certified==1 & course_id!="CS50x") %>%
  group_by(LoE_DI) %>%
  summarize(count = n(),
            avg_grade = mean(grade,na.rm=T)
  ) %>%
  arrange(desc(count))

by_education_completed$LoE_DI <- gsub("Less than Secondary", "< Secondary", by_education_completed$LoE_DI)
by_education_completed$LoE_DI <- factor(by_education_completed$LoE_DI, levels=rev(c("< Secondary", "Secondary", "Bachelor's", "Master's", "Doctorate")), ordered=T)

perc_labels <- paste(format(by_education_completed$avg_grade*100,digits=3),"%",sep='')

ggplot(aes(x=LoE_DI, y=avg_grade), data=by_education_completed) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=4) + 
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title="Avg Grade by Education in MIT/Harvard Online Classes", x="Level of Education", y="Avg % Final Grade for Classes per LoE")

ggsave("education-grade.png", dpi=300, width=4, height=3)

### Gender Comparisons

second_color = colors[7]

by_gender <- user_data %>%
  filter(gender=='m' | gender=='f') %>%
  group_by(gender) %>%
  summarize(count = n(),
            avg_age = mean(age,na.rm=T),
            avg_courses_taken = mean(num_courses_taken),
            avg_courses_completed = mean(num_courses_completed),
            avg_fulfillment = avg_courses_completed / avg_courses_taken
  ) %>%
  arrange(desc(count))

by_gender$gender <- c("Male", "Female")

ggplot(aes(x=gender,y=count), data=by_gender) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("Gender Distribution of",format(sum(by_gender$count), big.mark=","),"Students in Online Classes"), x="Gender", y="# of Students")

ggsave("gender-count.png", dpi=300, width=4, height=3)

ggplot(aes(x=gender, y=avg_age), data=by_gender) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = comma, breaks=seq(0,35,by=5)) +
  geom_text(label = format(by_gender$avg_age,digits=3), color = "white", hjust=1.5, family=fontFamily, size=5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("Avg Ages of Sdnts. in MIT/Harvard Online Classes"), x="Gender", y="Avg Age of Students per Gender")

ggsave("gender-age.png", dpi=300, width=4, height=3)

perc_labels <- paste(format(by_gender$avg_fulfillment*100,digits=2),"%",sep='')

ggplot(aes(x=gender, y=avg_fulfillment), data=by_gender) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent, breaks=seq(0,0.1,by=0.01)) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("Avg Completion Rates of MIT/Harvard Online Classes"), x="Gender", y="Avg % Completion Rate of Classes by Gender")

ggsave("gender-completion.png", dpi=300, width=4, height=3)

by_gender_completed <- data %>%
  filter((gender=='m' | gender=='f') & certified==1 & course_id!="CS50x") %>%
  group_by(gender) %>%
  summarize(count = n(),
            avg_grade = mean(grade,na.rm=T)
  )

by_gender_completed$gender <- c("Female", "Male")
by_gender_completed$gender <- factor(by_gender_completed$gender,levels=rev(c("Male", "Female")), ordered=T)

perc_labels <- paste(format(by_gender_completed$avg_grade*100,digits=3),"%",sep='')

ggplot(aes(x=gender, y=avg_grade), data=by_gender_completed) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color)) +
  labs(title=paste("Avg % Grade of MIT/Harvard Online Classes by Gender"), x="Gender", y="Avg % Final Grade for each Gender")

ggsave("gender-grade.png", dpi=300, width=4, height=3)

### Which countries produce the best students?

second_color = colors[6]

user_data <- data %>%
  filter(final_cc_cname_DI != "Unknown/Other") %>%
  group_by(userid_DI, LoE_DI, final_cc_cname_DI, age, gender) %>%
  summarize(num_courses_taken = n(),
            num_courses_completed = sum(certified)
  )

by_country <- user_data %>%
  filter(!is.na(final_cc_cname_DI) & final_cc_cname_DI != '') %>%
  group_by(final_cc_cname_DI) %>%
  summarize(count = n(),
            avg_age = mean(age,na.rm=T),
            perc_more_than_one_class = sum(num_courses_taken > 1) / count,
            avg_courses_taken = mean(num_courses_taken),
            avg_courses_completed = mean(num_courses_completed),
            avg_fulfillment = avg_courses_completed / avg_courses_taken,
            perc_m = sum(gender=='m') / (sum(gender=='m') + sum(gender=='f'))
  ) %>%
  arrange(desc(count))

by_country <- by_country[which(!grepl("Other",by_country$final_cc_cname_DI)),]
by_country <- by_country[which(!grepl("Russian Federation",by_country$final_cc_cname_DI)),]

by_country$final_cc_cname_DI <- reorder(by_country$final_cc_cname_DI,by_country$count)

perc_labels <- paste(by_country$count %/% 1000,"k",sep='')
perc_labels[1] <- ''


ggplot(aes(x=final_cc_cname_DI, y=count), data=by_country) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  geom_text(label = perc_labels, color = second_color, hjust=-.5, family=fontFamily, size=1.5) +
  scale_y_continuous(labels = comma, breaks=seq(0,150000,by=20000)) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color), axis.text.y=element_text(size=5)) +
  labs(title=paste("Countries of",format(sum(by_country$count), big.mark=","),"Students in MIT/Harvard Online Classes"), x="Country", y="# of Students")

ggsave("country-count.png", dpi=300, width=4, height=3)

by_country$final_cc_cname_DI <- reorder(by_country$final_cc_cname_DI,by_country$avg_age)

ggplot(aes(x=final_cc_cname_DI, y=avg_age), data=by_country) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = comma, breaks=seq(0,35,by=5)) +
  geom_text(label = format(by_country$avg_age,digits=3), color = "white", hjust=1.5, family=fontFamily, size=1.5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color), axis.text.y=element_text(size=5)) +
  labs(title=paste("Avg Ages of Sdnts. in MIT/Harvard Online Classes"), x="Country", y="Avg Age of Students per Country")

ggsave("country-age.png", dpi=300, width=4, height=3)

by_country$final_cc_cname_DI <- reorder(by_country$final_cc_cname_DI,by_country$perc_more_than_one_class)

perc_labels <- paste(format(by_country$perc_more_than_one_class*100,digits=3),"%",sep='')

ggplot(aes(x=final_cc_cname_DI, y=perc_more_than_one_class), data=by_country) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent, breaks=seq(0,0.3,by=0.05)) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=1.5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color), axis.text.y=element_text(size=5)) +
  labs(title=paste("% Students Taking >1 Online Classes by Country"), x="Country", y="% Students w/ >1 Classes per Country")

ggsave("country-more-than-one.png", dpi=300, width=4, height=3)

by_country$final_cc_cname_DI <- reorder(by_country$final_cc_cname_DI,by_country$avg_fulfillment)

perc_labels <- paste(format(by_country$avg_fulfillment*100,digits=1),"%",sep='')

ggplot(aes(x=final_cc_cname_DI, y=avg_fulfillment), data=by_country) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent, breaks=seq(0,0.1,by=0.01)) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=1.5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color), axis.text.y=element_text(size=5)) +
  labs(title=paste("Avg Completion Rates of MIT/Harvard Online Classes"), x="Country", y="Avg % Completion Rate of Classes by Country")

ggsave("country-completion.png", dpi=300, width=4, height=3)

by_country_completed <- data %>%
  filter(!is.na(final_cc_cname_DI) & (final_cc_cname_DI != '') & certified==1 & course_id!="CS50x") %>%
  group_by(final_cc_cname_DI) %>%
  summarize(count = n(),
            avg_grade = mean(grade,na.rm=T)
  ) %>%
  arrange(desc(count))

by_country_completed <- by_country_completed[which(!grepl("Other",by_country_completed$final_cc_cname_DI)),]
by_country_completed <- by_country_completed[which(!grepl("Russian Federation",by_country_completed$final_cc_cname_DI)),]

by_country_completed$final_cc_cname_DI <- reorder(by_country_completed$final_cc_cname_DI,by_country_completed$avg_grade)

perc_labels <- paste(format(by_country_completed$avg_grade*100,digits=3),"%",sep='')

ggplot(aes(x=final_cc_cname_DI, y=avg_grade), data=by_country_completed) +
  geom_bar(stat="identity", fill = second_color) +
  theme_custom() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  geom_text(label = perc_labels, color = "white", hjust=1.5, family=fontFamily, size=1.5) +
  theme(axis.title.y = element_text(color=second_color), axis.title.x = element_text(color=second_color), axis.text.y=element_text(size=5)) +
  labs(title=paste("Avg Final Grades of MIT/Harvard Online Classes"), x="Country", y="Avg % Final Grades of Classes per Country")

ggsave("country-grade.png", dpi=300, width=4, height=3)
