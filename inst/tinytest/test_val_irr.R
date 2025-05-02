library(tinytest)

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
