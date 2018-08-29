library(nycflights13)
dim(flights)
flights
str(flights)

filter(flights, month == 1, day == 1)
flights[flights$month == 1 & flights$day == 1, ]

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, -year, -day)
select(flights, tail_num = tailnum)
rename(flights, tail_num = tailnum)

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
       )
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
       )
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
          )

summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
          )

sample_n(flights, 10)
sample_frac(flights, 0.01)



by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) +
    geom_point(aes(size = count), alpha = 1/2) +
    geom_smooth() +
    scale_size_area()

destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
          )

(daily <- group_by(flights, year, month, day))
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))


select(flights, year)
select(flights, 1)
