library(tinytest)

# 3.5.2025
curr_value <- data.frame(
	calender_week = c("2025-17", "2025-17"), 
	typ = c("activepos", "cash"), 
	value = c(57, 0)
)
trans <- data.frame(
	date = as.Date("2025-04-10"),
	last_day_of_month = as.Date("2025-04-30"),
    amount = 55.36
)
ans <- val_irr(con=NULL, portfolio="elvis", trans=trans, current_value=curr_value) 
expect_equal(ans, 0.6665462, tolerance=0.00001)

# 11.5.2025
curr_value <- data.frame(
	calender_week = c("2025-19", "2025-19"), 
	typ = c("activepos", "cash"), 
	value = c(26310.971235219, 13459.77)
)
trans <- data.frame(
	date = as.Date(c("2024-09-19", "2024-05-06", "2024-04-04", "2024-04-03")),
    last_day_of_month = as.Date(c("2024-09-30","2024-05-31","2024-04-30", "2024-04-30")), 
    amount = c(10000, 12500, 15000, 1)
)
ans <- val_irr(con=NULL, portfolio="nert", trans=trans, current_value=curr_value) 
expect_equal(ans, 0.0637406, tolerance=0.00001)

