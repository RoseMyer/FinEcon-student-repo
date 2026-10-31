# ============================================================================
# bond_project_rose.R -- Bond Project, adapted from the instructor's
# bond_project_student.R for my five bonds (defense + energy theme).
#
# WHAT THIS SCRIPT DOES
#   Q1  Reads daily bond PRICES for five bonds, converts one to dollars,
#       and draws a line graph.
#   Q2  Same for the YIELD TO MATURITY (YTM) of each bond.
#   Q3  Helper output: price-vs-yield scatter, correlations, and the change
#       in each bond's price and yield over the year.
#   Q4  Re-prices one bond from its YTM with the bond pricing equation on a
#       date that is an exact number of semi-annual periods from maturity,
#       and compares the result with the FactSet price.
#   Q5  Summary table sorted by YTM on the last date, a ranked dot plot, and
#       the two bonds with the closest maturities.
#
# WHAT CHANGED FROM THE INSTRUCTOR'S VERSION
#   - Section 1: my bonds, colors, and focus bond (RTX).
#   - Extra code checks (dates sorted, date window, prices vs yields move
#     in opposite directions, Q4 price within tolerance of FactSet).
#   - New sections for Q3 helpers, Q4, and Q5(c).
#   - The Q5 chart title is computed from the data instead of typed in.
#
# INPUT (in bond-assignment/data/), downloaded from FactSet Price History in
# "Multiple" mode with all five CUSIPs, 29 Aug 2025 to 31 Aug 2026, Daily:
#   MyBondPrices.xlsx  Date + one column per bond, price as PERCENT OF FACE
#   MyBondYields.xlsx  Date + one column per bond, yield to maturity in PERCENT
#   Column names look like
#       RTX Corporation 2.25% 01-JUL-2030 (75513EAD3)
#   so the coupon, maturity and CUSIP are read from the column name.
#
# OUTPUT (written to bond-assignment/output/rose/)
# ============================================================================


# ---- 0. Packages -----------------------------------------------------------
# install.packages(c("tidyverse", "readxl", "scales", "assertthat", "here", "knitr"))
library(tidyverse)
library(readxl)
library(scales)
library(assertthat)
library(here)
library(knitr)


# ---- 1. Settings -----------------------------------------------------------
here::i_am("bond-assignment/scripts/bond_project_rose.R")
data_dir   <- here("bond-assignment", "data")
output_dir <- here("bond-assignment", "output", "rose")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

PRICE_FILE <- "MyBondPrices.xlsx"
YIELD_FILE <- "MyBondYields.xlsx"

FACE_VALUE <- 1000   # confirmed on each bond's FactSet Security Overview page
FREQ       <- 2      # all five bonds pay semi-annual coupons (FactSet)
FOCUS      <- "RTX"  # bond used for Q1(c) and Q4

# Assignment window: end of Aug 2025 to end of Aug 2026
# (semester started Sept 2026). Aug 31, 2025 was a Sunday.
WINDOW_START <- as.Date("2025-08-29")
WINDOW_END   <- as.Date("2026-08-31")

my_bonds <- tribble(
  ~cusip,      ~ticker, ~company,           ~sector,
  "75513EAD3", "RTX",   "RTX",              "Defense",
  "30231GBN1", "XOM",   "ExxonMobil",       "Energy",
  "369550BP2", "GD",    "General Dynamics", "Defense",
  "166756BE5", "CVX",   "Chevron",          "Energy",
  "539830BW8", "LMT",   "Lockheed Martin",  "Defense"
)

# S&P ratings as shown on each bond's FactSet Security Overview page
# (screenshots in the report). Fill in GD, CVX and LMT from FactSet.
ratings <- tribble(
  ~ticker, ~sp_rating,
  "RTX",   "BBB+",
  "XOM",   "AA-",
  "GD",    NA,
  "CVX",   NA,
  "LMT",   NA
)

# Okabe-Ito colorblind-friendly palette, one color per company.
firm_palette <- c("RTX"              = "#0072B2",
                  "ExxonMobil"       = "#D55E00",
                  "General Dynamics" = "#009E73",
                  "Chevron"          = "#E69F00",
                  "Lockheed Martin"  = "#CC79A7")

theme_bond <- theme_minimal(base_size = 12) +
  theme(panel.grid      = element_blank(),
        axis.line       = element_line(colour = "grey30"),
        legend.title    = element_blank(),
        legend.position = "bottom",
        plot.title      = element_text(face = "bold"),
        plot.caption    = element_text(colour = "grey35", hjust = 0),
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA))


# ---- 2. Read the data ------------------------------------------------------
# FactSet files have a title in row 1 and a blank row 2, so skip 2 rows.
read_factset <- function(file, value_name) {
  path <- file.path(data_dir, file)
  assert_that(file.exists(path), msg = paste("File not found:", path))
  read_excel(path, skip = 2) |>
    mutate(date = as.Date(Date)) |>
    select(-Date) |>
    pivot_longer(-date, names_to = "header", values_to = value_name)
}

prices_long <- read_factset(PRICE_FILE, "price")
yields_long <- read_factset(YIELD_FILE, "ytm")

bond_panel <- inner_join(prices_long, yields_long, by = c("date", "header")) |>
  arrange(date) |>
  mutate(coupon_rate = as.numeric(str_match(header, "([0-9.]+)%")[, 2]) / 100,
         maturity    = as.Date(str_match(header, "([0-9]{2}-[A-Z]{3}-[0-9]{4})")[, 2],
                               format = "%d-%b-%Y"),
         cusip       = str_match(header, "\\(([A-Z0-9]{9})\\)")[, 2],
         coupon_semi = coupon_rate / FREQ * FACE_VALUE)

# FactSet "Multiple" mode can add extra columns (stock tickers, a Composite
# index, securities viewed earlier). Keep only my five bond CUSIPs.
dropped <- setdiff(unique(bond_panel$header[!bond_panel$cusip %in% my_bonds$cusip]), NA)
if (length(dropped) > 0) {
  message("Ignoring columns that are not one of my five bonds:\n  ",
          paste(dropped, collapse = "\n  "))
}

bond_panel <- bond_panel |>
  filter(cusip %in% my_bonds$cusip) |>
  left_join(my_bonds, by = "cusip") |>
  mutate(company = factor(company, levels = my_bonds$company)) |>
  drop_na(price, ytm)                            # drop days with no quote

bonds <- bond_panel |>
  distinct(ticker, company, sector, cusip, coupon_rate, maturity, coupon_semi)


# ---- 3. Code checks --------------------------------------------------------
assert_that(!any(is.na(bonds$ticker)),
            msg = "A CUSIP in the FactSet file is missing from my_bonds")
assert_that(!any(is.na(bonds$coupon_rate)), !any(is.na(bonds$maturity)),
            msg = "Could not read a coupon rate or maturity from a column name")
assert_that(nrow(bonds) == 5, msg = "Expected exactly five bonds")
assert_that(setequal(bonds$cusip, my_bonds$cusip),
            msg = "The five CUSIPs in the file do not match my_bonds")

# Coupons and maturities should match what I saw on FactSet.
expected <- tribble(
  ~cusip,      ~coupon_check, ~maturity_check,
  "75513EAD3", 0.02250,       as.Date("2030-07-01"),
  "30231GBN1", 0.02610,       as.Date("2030-10-15"),
  "369550BP2", 0.02250,       as.Date("2031-06-01"),
  "166756BE5", 0.04819,       as.Date("2032-04-15"),
  "539830BW8", 0.05250,       as.Date("2033-01-15")
)
chk <- inner_join(bonds, expected, by = "cusip")
assert_that(all(abs(chk$coupon_rate - chk$coupon_check) < 1e-6),
            msg = "A coupon rate does not match FactSet Security Overview")
assert_that(all(chk$maturity == chk$maturity_check),
            msg = "A maturity date does not match FactSet Security Overview")

n_dates <- n_distinct(bond_panel$date)
assert_that(all(table(bond_panel$ticker) == n_dates),
            msg = "Some bond is missing dates that others have")
assert_that(!is.unsorted(unique(bond_panel$date)), msg = "Dates are not sorted")
assert_that(all(between(bond_panel$price, 50, 150)),
            msg = "A price is outside 50-150: check percent-of-face units")
assert_that(all(between(bond_panel$ytm, 0, 15)),
            msg = "A yield is outside 0-15%: check percent units")

first_date <- min(bond_panel$date)
last_date  <- max(bond_panel$date)
assert_that(first_date <= WINDOW_START + 3, last_date >= WINDOW_END - 3,
            msg = "Data do not cover end-Aug 2025 to end-Aug 2026")
assert_that(n_dates > 240, msg = "Fewer than ~1 year of trading days")

# Price and yield must move in opposite directions (Q3a): the correlation of
# daily changes should be strongly negative for every bond.
move_cor <- bond_panel |>
  group_by(company) |>
  arrange(date, .by_group = TRUE) |>
  summarise(cor_daily_changes = cor(diff(price), diff(ytm)), .groups = "drop")
assert_that(all(move_cor$cor_daily_changes < -0.9),
            msg = "Price and yield changes are not strongly negatively related")

# Flag days where FactSet repeated the previous price (no new quote).
stale <- bond_panel |>
  group_by(company) |>
  arrange(date, .by_group = TRUE) |>
  filter(price == lag(price)) |>
  ungroup() |>
  select(date, company, price)

message(sprintf("Loaded %d bonds x %d dates, %s to %s",
                nrow(bonds), n_dates, first_date, last_date))
if (nrow(stale) > 0) {
  message("Days with an unchanged price (possibly no new quote):")
  print(stale, n = Inf)
}


# ---- 4. Question 1: prices -------------------------------------------------
prices_wide <- bond_panel |>
  select(date, company, price) |>
  pivot_wider(names_from = company, values_from = price) |>
  arrange(date)
write_csv(prices_wide, file.path(output_dir, "tab_q1_prices_wide.csv"))

## 1(c) Percent of face to dollars, focus bond on the first date.
focus     <- filter(bonds, ticker == FOCUS)
focus_row <- filter(bond_panel, ticker == FOCUS, date == first_date)
focus_dollar <- focus_row$price / 100 * FACE_VALUE

cat(sprintf(
  "\nQ1(c) %s %.3f%% %s (%s) on %s: quoted %.3f%% of face\n      => %.3f/100 x $%s = $%.2f per bond\n",
  focus$company, focus$coupon_rate * 100, format(focus$maturity, "%d %b %Y"),
  focus$cusip, format(first_date, "%d %b %Y"),
  focus_row$price, focus_row$price, format(FACE_VALUE, big.mark = ","), focus_dollar))

bond_panel <- mutate(bond_panel, price_dollar = price / 100 * FACE_VALUE)
assert_that(all(abs(bond_panel$price_dollar - bond_panel$price * 10) < 1e-9),
            msg = "Dollar price should be 10 x the percent quote at $1,000 face")
write_csv(select(bond_panel, date, ticker, company, price, price_dollar),
          file.path(output_dir, "bond_prices_dollars.csv"))

## 1(d) Line graph of prices.
source_caption <- paste0("Source: FactSet, ", format(first_date, "%d %b %Y"),
                         " to ", format(last_date, "%d %b %Y"), ".")
legend_labels <- bonds |>
  arrange(company) |>
  mutate(lab = sprintf("%s %.3g%% %s", company, coupon_rate * 100,
                       format(maturity, "%b %Y")))
legend_labels <- setNames(legend_labels$lab, as.character(legend_labels$company))

p_prices <- ggplot(bond_panel, aes(date, price, colour = company)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = firm_palette, labels = legend_labels) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "Daily bond price, percent of face value; one bond per issuer",
       x = NULL, y = "Price (% of face value)",
       caption = source_caption) +
  theme_bond + guides(colour = guide_legend(nrow = 2))
ggsave(file.path(output_dir, "fig_q1_bond_prices.png"), p_prices, bg = "white",
       width = 9, height = 5.5, dpi = 300)


# ---- 5. Question 2: yields -------------------------------------------------
yields_wide <- bond_panel |>
  select(date, company, ytm) |>
  pivot_wider(names_from = company, values_from = ytm) |>
  arrange(date)
write_csv(yields_wide, file.path(output_dir, "tab_q2_yields_wide.csv"))

p_yields <- ggplot(bond_panel, aes(date, ytm, colour = company)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = firm_palette, labels = legend_labels) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(title = "Daily yield to maturity, percentage points",
       x = NULL, y = "Yield to maturity (%)",
       caption = source_caption) +
  theme_bond + guides(colour = guide_legend(nrow = 2))
ggsave(file.path(output_dir, "fig_q2_bond_yields.png"), p_yields, bg = "white",
       width = 9, height = 5.5, dpi = 300)


# ---- 6. Question 3 helpers -------------------------------------------------
## 3(a) Price against yield: each bond traces a downward-sloping curve.
p_py <- ggplot(bond_panel, aes(ytm, price, colour = company)) +
  geom_point(size = 0.8, alpha = 0.6) +
  facet_wrap(~ company, scales = "free", nrow = 1) +
  scale_colour_manual(values = firm_palette, guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(title = "Higher yield, lower price: every bond slopes down",
       subtitle = "Each dot is one trading day",
       x = "Yield to maturity (%)", y = "Price (% of face)",
       caption = source_caption) +
  theme_bond
ggsave(file.path(output_dir, "fig_q3_price_vs_yield.png"), p_py, bg = "white",
       width = 11, height = 3.8, dpi = 300)

cat("\nQ3(a) correlation of daily price changes with daily yield changes\n")
print(move_cor)

## 3(b) How much each bond moved over the year, and its high/low dates.
q3_moves <- bond_panel |>
  group_by(company) |>
  summarise(price_start = first(price), price_end = last(price),
            ytm_start   = first(ytm),   ytm_end   = last(ytm),
            ytm_high = max(ytm), ytm_high_date = date[which.max(ytm)],
            ytm_low  = min(ytm), ytm_low_date  = date[which.min(ytm)],
            .groups = "drop") |>
  mutate(price_change   = price_end - price_start,
         ytm_change_bps = (ytm_end - ytm_start) * 100)
cat("\nQ3(b) start-to-end moves, ", format(first_date, "%d %b %Y"), " to ",
    format(last_date, "%d %b %Y"), "\n", sep = "")
print(q3_moves |> mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
write_csv(q3_moves, file.path(output_dir, "tab_q3_moves.csv"))

# Biggest one-day yield moves across all bonds (for finding news events).
big_days <- bond_panel |>
  group_by(company) |>
  arrange(date, .by_group = TRUE) |>
  mutate(d_ytm_bps = (ytm - lag(ytm)) * 100) |>
  ungroup() |>
  group_by(date) |>
  summarise(avg_move_bps = mean(d_ytm_bps), .groups = "drop") |>
  drop_na() |>
  arrange(desc(abs(avg_move_bps))) |>
  slice_head(n = 8)
cat("\nQ3(b) days with the largest average yield move across the five bonds\n")
print(big_days)


# ---- 7. Question 4: price from YTM -----------------------------------------
# Bond pricing equation, semi-annual coupons:
#   P = sum_{t=1}^{n} C / (1 + y/2)^t  +  F / (1 + y/2)^n
#   C = coupon_rate / 2 * F, y = YTM (decimal), n = half-years to maturity.
price_from_ytm <- function(ytm_pct, coupon_rate, n, face = FACE_VALUE, freq = FREQ) {
  y <- ytm_pct / 100 / freq
  C <- coupon_rate / freq * face
  sum(C / (1 + y)^(1:n)) + face / (1 + y)^n
}

# Check the function on the assignment's own example: 6% coupon, 7% YTM,
# 1 year (2 periods) should give about $990.50.
assert_that(abs(price_from_ytm(7, 0.06, 2) - 990.50) < 0.01,
            msg = "Pricing function fails the assignment's worked example")

# Dates in my data that are an exact number of half-years before maturity.
half_year_dates <- function(maturity) {
  seq(maturity, by = "-6 months", length.out = 40)
}
q4_candidates <- bonds |>
  rowwise() |>
  mutate(date = list(half_year_dates(maturity))) |>
  unnest(date) |>
  semi_join(bond_panel, by = c("ticker", "date")) |>
  select(company, ticker, maturity, date)
cat("\nQ4 candidate dates (exact half-years before maturity, with a FactSet quote)\n")
print(q4_candidates, n = Inf)

q4_date <- q4_candidates |> filter(ticker == FOCUS) |> pull(date) |> max()
assert_that(length(q4_date) == 1, !is.infinite(q4_date),
            msg = "No exact half-year date in the data for the focus bond")

q4_row <- filter(bond_panel, ticker == FOCUS, date == q4_date)
q4_n   <- round(lubridate::interval(q4_date, focus$maturity) / months(6))
assert_that(q4_date %m+% months(6 * q4_n) == focus$maturity,
            msg = "Q4 date is not an exact number of half-years from maturity")

q4_price_model   <- price_from_ytm(q4_row$ytm, focus$coupon_rate, q4_n)
q4_price_factset <- q4_row$price_dollar
q4_gap           <- q4_price_model - q4_price_factset

# Reverse check: solve for the YTM that reproduces the FactSet price.
q4_ytm_implied <- uniroot(function(y) price_from_ytm(y, focus$coupon_rate, q4_n) -
                            q4_price_factset, c(0.01, 20))$root

C_semi <- focus$coupon_rate / FREQ * FACE_VALUE
cat(sprintf(paste0(
  "\nQ4 %s %.3f%% due %s, on %s (exactly %d half-years before maturity)\n",
  "   coupon per period C = %.3f%%/2 x $1,000 = $%.2f;  y/2 = %.5f%%/2 = %.6f\n",
  "   P = sum_{t=1}^{%d} %.2f/(1+%.6f)^t + 1000/(1+%.6f)^%d\n",
  "   Model price:   $%.2f\n   FactSet price: $%.2f  (%.3f%% of face)\n",
  "   Difference:    $%.2f\n",
  "   YTM implied by FactSet price: %.5f%% vs FactSet YTM %.5f%%\n"),
  focus$company, focus$coupon_rate * 100, format(focus$maturity, "%d %b %Y"),
  format(q4_date, "%d %b %Y"), q4_n,
  focus$coupon_rate * 100, C_semi, q4_row$ytm, q4_row$ytm / 100 / FREQ,
  q4_n, C_semi, q4_row$ytm / 100 / FREQ, q4_row$ytm / 100 / FREQ, q4_n,
  q4_price_model, q4_price_factset, q4_row$price, q4_gap,
  q4_ytm_implied, q4_row$ytm))

# Small differences are expected (FactSet prices for settlement a day or two
# later, rounding). More than $2 per $1,000 means something is off.
assert_that(abs(q4_gap) < 2,
            msg = "Q4 model price is more than $2 away from FactSet: check inputs")

# Cash-flow table for the write-up.
q4_cashflows <- tibble(period = 1:q4_n,
                       pay_date = q4_date %m+% months(6 * period),
                       cash_flow = C_semi + if_else(period == q4_n, FACE_VALUE, 0),
                       discount_factor = 1 / (1 + q4_row$ytm / 100 / FREQ)^period,
                       present_value = cash_flow * discount_factor)
assert_that(abs(sum(q4_cashflows$present_value) - q4_price_model) < 1e-8)
print(q4_cashflows |> mutate(across(c(discount_factor, present_value), ~ round(.x, 4))))
write_csv(q4_cashflows, file.path(output_dir, "tab_q4_cashflows.csv"))


# ---- 8. Question 5: summary table, chart, closest maturities ---------------
summary_tbl <- bond_panel |>
  filter(date == last_date) |>
  mutate(years_to_maturity = as.numeric(maturity - last_date) / 365.25) |>
  left_join(ratings, by = "ticker") |>
  select(company, sector, cusip, sp_rating, coupon_rate, maturity,
         years_to_maturity, price, price_dollar, ytm) |>
  arrange(ytm)
assert_that(nrow(summary_tbl) == 5, !is.unsorted(summary_tbl$ytm))

cat("\nQ5(a) summary at ", format(last_date, "%d %b %Y"), "\n", sep = "")
print(summary_tbl |>
        mutate(coupon_rate = coupon_rate * 100,
               across(c(years_to_maturity, price, price_dollar, ytm),
                      ~ round(.x, 3))), n = Inf, width = Inf)

## 5(b) Dot plot ordered from lowest to highest YTM, labelled.
spread_bps <- round(diff(range(summary_tbl$ytm)) * 100)
ytm_range  <- range(summary_tbl$ytm)
p_rank <- ggplot(summary_tbl,
                 aes(x = fct_reorder(company, ytm), y = ytm, colour = company)) +
  geom_point(size = 5) +
  geom_text(aes(label = sprintf("%.2f%%", ytm)), vjust = -1.4, size = 3.8,
            colour = "grey20") +
  scale_colour_manual(values = firm_palette, guide = "none") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%"),
                     limits = c(ytm_range[1] - 0.1, ytm_range[2] + 0.2)) +
  labs(title = sprintf("%d basis points separate the highest yield from the lowest",
                       spread_bps),
       subtitle = paste("Yield to maturity, one bond per issuer, as of",
                        format(last_date, "%d %B %Y"), "- lowest to highest"),
       x = "Company", y = "Yield to maturity (%)",
       caption = "Source: FactSet. Vertical axis does not start at zero.") +
  theme_bond
ggsave(file.path(output_dir, "fig_q5_ytm_ranked.png"), p_rank, bg = "white",
       width = 9, height = 5, dpi = 300)

## 5(c) The two bonds whose maturities are closest.
pairs <- combn(nrow(summary_tbl), 2)
closest <- tibble(i = pairs[1, ], j = pairs[2, ]) |>
  mutate(gap_days = abs(as.numeric(summary_tbl$maturity[i] - summary_tbl$maturity[j]))) |>
  slice_min(gap_days, n = 1, with_ties = FALSE)
q5c <- summary_tbl[c(closest$i, closest$j), ] |>
  select(company, sector, sp_rating, coupon_rate, maturity, years_to_maturity, ytm)
cat(sprintf("\nQ5(c) closest maturities: %d days apart; YTM gap %.0f bps\n",
            closest$gap_days, abs(diff(q5c$ytm)) * 100))
print(q5c |> mutate(coupon_rate = coupon_rate * 100,
                    across(c(years_to_maturity, ytm), ~ round(.x, 3))), width = Inf)


# ---- 9. Save tables ---------------------------------------------------------
q5_table <- summary_tbl |>
  transmute(Company             = company,
            `S&P`               = sp_rating,
            `Coupon (%)`        = round(coupon_rate * 100, 3),
            Maturity            = format(maturity, "%d %b %Y"),
            `Years to maturity` = round(years_to_maturity, 2),
            `Price ($)`         = round(price_dollar, 2),
            `YTM (%)`           = round(ytm, 4))
write_csv(q5_table, file.path(output_dir, "tab_q5_summary.csv"))
writeLines(kable(q5_table, format = "pipe"), file.path(output_dir, "tab_q5_summary.md"))
writeLines(kable(q5_table, format = "latex", booktabs = TRUE, linesep = ""),
           file.path(output_dir, "tab_q5_summary.tex"))

message("Done. Figures and tables are in ", output_dir)
