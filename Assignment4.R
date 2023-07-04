student_name <- c("Mike A", "Joy B", "Kate C", "Jose D")
student_ID <- c("00123", "00124", "00125", "00126")
GPA <- c(3.1, 2.9, 3.6, 3.3)
student_name
student_ID
GPA
gender <- factor(c("MALE", "MALE", "FEMALE", "MALE"))
gender
students <- data.frame(student_name, student_ID, GPA, gender)
students
stu_4 <- list(student_name = "Maria E", student_ID="00130", GPA=3.9, gender = "FEMALE")
stu_4
students <- rbind(students, stu_4)
students
dim(students)
students[1:2,]
students$gender 
