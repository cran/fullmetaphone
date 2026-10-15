## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(fullmetaphone)

## -----------------------------------------------------------------------------
names <- c("Meyer", "Meier", "Mayer", "Catherine", "Katherine",
           "Smith", "Schmidt")
metaphone(names)
double_metaphone(names)

## -----------------------------------------------------------------------------
long_name <- "Christopher Alexander Montgomery Fitzgerald"
double_metaphone(long_name)
double_metaphone(long_name, max_length = 4)
double_metaphone(long_name, max_length = 10)
double_metaphone(long_name, max_length = Inf)

## -----------------------------------------------------------------------------
double_metaphone(long_name, by_word = TRUE, max_length = 4)

## -----------------------------------------------------------------------------
rules <- c("Philip", "Knight", "Wright", "Gnome", "Thomas", "Laugh",
           "Dumb", "Edge", "Xavier")
data.frame(word = rules, metaphone = metaphone(rules),
           double_metaphone(rules))

## -----------------------------------------------------------------------------
foreign <- c("Schmidt", "Michael", "Jose", "Tagliaro", "Czerny",
             "Wasserman", "Zhao", "Breaux")
data.frame(word = foreign, double_metaphone(foreign))

## -----------------------------------------------------------------------------
sounds_like("Smith", c("Schmidt", "Smyth", "Smythe", "Jones"))
sounds_like("Wasserman", "Vasserman")

## -----------------------------------------------------------------------------
sounds_like("Smith", "Schmidt", method = "metaphone")

## -----------------------------------------------------------------------------
head(us_surnames)
codes_full <- double_metaphone(us_surnames$surname)$primary
codes_four <- double_metaphone(us_surnames$surname, max_length = 4)$primary
length(unique(codes_full))
length(unique(codes_four))

## -----------------------------------------------------------------------------
group <- codes_four == "ALTR"
data.frame(surname = us_surnames$surname[group],
           four = codes_four[group],
           full = codes_full[group])

## -----------------------------------------------------------------------------
first_names <- c("James", "Mary", "Robert", "Patricia", "John",
                 "Jennifer", "Michael", "Linda", "David", "Elizabeth",
                 "Christopher", "Sarah", "Maria", "Jose", "Juan",
                 "Mohammed", "Ahmed", "Wei", "Anna", "Sophie", "Lukas",
                 "Pierre", "Giovanni", "Olga", "Katarzyna", "Priya")
set.seed(2026)
people <- unique(paste(sample(first_names, 5000, replace = TRUE),
                       sample(us_surnames$surname, 5000, replace = TRUE)))
length(people)
length(unique(double_metaphone(people)$primary))
length(unique(double_metaphone(people, max_length = 4)$primary))

## -----------------------------------------------------------------------------
cr <- c("Christopher Anderson", "Christina Andrews", "Christopher Andersen")
data.frame(name = cr,
           four = double_metaphone(cr, max_length = 4)$primary,
           full = double_metaphone(cr)$primary)

## -----------------------------------------------------------------------------
contacts <- data.frame(
  id    = 1:12,
  first = c("Catherine", "Katherine", "Stephen", "Steven", "Mohammed",
            "Muhammad", "Jeffrey", "Geoffrey", "Anne", "Ann", "Sean",
            "Shaun"),
  last  = c("Smith", "Smyth", "Phillips", "Philips", "Ali", "Aly",
            "Wasserman", "Vasserman", "Thompson", "Thomson", "O'Neill",
            "O'Neil"),
  city  = c("Leeds", "Leeds", "Boston", "Boston", "Toronto", "Toronto",
            "Chicago", "Chicago", "Sydney", "Sydney", "Dublin", "Dublin")
)

## -----------------------------------------------------------------------------
contacts$key <- paste(double_metaphone(contacts$last)$primary,
                      contacts$city)
pairs <- merge(contacts, contacts, by = "key", suffixes = c("_a", "_b"))
pairs <- pairs[pairs$id_a < pairs$id_b, ]
pairs$first_match <- sounds_like(pairs$first_a, pairs$first_b)
pairs[, c("first_a", "last_a", "first_b", "last_b", "first_match")]

## -----------------------------------------------------------------------------
pairs <- merge(contacts, contacts, by = "city", suffixes = c("_a", "_b"))
pairs <- pairs[pairs$id_a < pairs$id_b, ]
same_person <- sounds_like(pairs$last_a, pairs$last_b) &
  sounds_like(pairs$first_a, pairs$first_b)
pairs[same_person, c("id_a", "first_a", "last_a", "id_b", "first_b", "last_b")]

## -----------------------------------------------------------------------------
members <- data.frame(
  member_id = c("M1", "M2", "M3", "M4", "M5"),
  name = c("Jon Schmidt", "Maria Gonzalez", "Peter Kowalski",
           "Claire Dubois", "Hans Meyer")
)
donations <- data.frame(
  receipt = c("R1", "R2", "R3", "R4", "R5", "R6"),
  donor = c("John Smith", "Mariah Gonzales", "Piotr Kowalsky",
            "Clare Du Bois", "Hans Maier", "Helen Mayer"),
  amount = c(50, 120, 75, 200, 30, 90)
)

## -----------------------------------------------------------------------------
split_name <- function(x) {
  parts <- strsplit(x, " ", fixed = TRUE)
  data.frame(first = vapply(parts, `[`, "", 1),
             last  = vapply(parts, function(p) paste(p[-1], collapse = " "), ""))
}
members   <- cbind(members, split_name(members$name))
donations <- cbind(donations, split_name(donations$donor))

members$block   <- double_metaphone(members$last)$primary
donations$block <- double_metaphone(donations$last)$primary
candidates <- merge(members, donations, by = "block")
candidates[, c("name", "donor", "block")]

## -----------------------------------------------------------------------------
all_pairs <- merge(members, donations, by = NULL)
all_pairs <- all_pairs[sounds_like(all_pairs$last.x, all_pairs$last.y) &
                       sounds_like(all_pairs$first.x, all_pairs$first.y), ]
all_pairs[, c("member_id", "name", "receipt", "donor", "amount")]

## -----------------------------------------------------------------------------
all_pairs$distance <- diag(adist(all_pairs$name, all_pairs$donor))
all_pairs[order(all_pairs$distance),
          c("name", "donor", "distance")]

## -----------------------------------------------------------------------------
hits <- us_surnames[sounds_like("Meyer", us_surnames$surname), ]
head(hits[order(-hits$count), ], 10)

## -----------------------------------------------------------------------------
hits$distance <- adist("Meyer", hits$surname)[1, ]
head(hits[order(hits$distance, -hits$count), ], 6)

## -----------------------------------------------------------------------------
double_metaphone("Maria Gonzalez Lopez")
double_metaphone("Maria Gonzalez Lopez", by_word = TRUE)

## -----------------------------------------------------------------------------
sounds_like("Pierre Dubois", "Pierre Du Bois")
codes <- metaphone(c("Wei Zhang", "Zhang Wei"), by_word = TRUE)
sorted <- vapply(strsplit(codes, " "),
                 function(w) paste(sort(w), collapse = " "), "")
sorted

## -----------------------------------------------------------------------------
staff <- data.frame(name = c("Stephen Phillips", "Steven Philips"))
cbind(staff, double_metaphone(staff$name))

## ----eval = requireNamespace("dplyr", quietly = TRUE)-------------------------
library(dplyr, warn.conflicts = FALSE)
mutate(staff,
       double_metaphone(name),
       metaphone = metaphone(name))

