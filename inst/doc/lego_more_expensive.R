## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = '100%',
  fig.width = 6,
  fig.height = 4,
  comment = "#>"
)

## ----setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
library(brickset)
library(ggplot2)
library(dplyr)
data(legosets)
theme_set(theme_minimal())

## ----n-sets-by-year, echo=FALSE, fig.cap='Number of Lego sets by year.'-------
ggplot(legosets, aes(x = year, fill = is.na(US_retailPrice))) + 
	geom_bar() +
	scale_fill_brewer('Retail price available', type = 'qual', palette = 7) +
	ggtitle('Number of Lego sets by year') +
	xlab('Year') + ylab('Number of Lego Sets') +
	theme(legend.position = 'bottom')

## ----lego-summary, echo=TRUE--------------------------------------------------
lego_summary <- legosets |>
	dplyr::filter(
		year >= 2000 &
		pieces > 0
		) |>
	dplyr::mutate(
		price_per_piece = US_retailPrice / pieces,
		valid_set = !is.na(US_retailPrice) & !is.na(pieces)
	) |>
	# There are some electronic products we want to exclude
	dplyr::filter(is.na(price_per_piece) | price_per_piece < 1) |> 
	dplyr::group_by(year) |>
	dplyr::summarise(
		n = dplyr::n(),
		n_valid = sum(valid_set),
		mean_pieces = mean(pieces, na.rm = TRUE),
		sd_pieces = sd(pieces, na.rm = TRUE),
		mean_price = mean(US_retailPrice, na.rm = TRUE),
		sd_price = sd(US_retailPrice, na.rm = TRUE),
		mean_price_per_piece = mean(price_per_piece, na.rm = TRUE),
		sd_price_per_piece = sd(price_per_piece, na.rm = TRUE)
	)
lego_summary |> 
	as.data.frame() |> 
	mutate(
		mean_pieces = paste0(round(mean_pieces, digits = 2), ' (', round(sd_pieces, digits = 2), ')'),
		mean_price = paste0(round(mean_price, digits = 2), ' (', round(sd_price, digits = 2), ')'),
		mean_price_per_piece = paste0(round(mean_price_per_piece, digits = 2), ' (', round(sd_price_per_piece, digits = 2), ')')
	) |>
	dplyr::select(!dplyr::starts_with('sd_')) |>
	dplyr::rename(Year = year, 
				  `Number of sets` = n,
				  `Sets with price` = n_valid, 
				  `Pieces per set` = mean_pieces,
				  `Set price` = mean_price,
				  `Price per piece` = mean_price_per_piece) |>
	knitr::kable(caption = 'Summary of Lego cost by year.', digits = 2)

## ----mean-price-per-piece, fig.cap='Average price (USD) per piece by year.'----
ggplot(lego_summary, aes(x = year, y = mean_price_per_piece)) +
	geom_path() +
	geom_point(aes(size = n_valid)) +
	scale_size('n Sets') +
	scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
	expand_limits(y = 0) +
	ylab('Average price per piece') + xlab('Year') +
	ggtitle('Average price (USD) per piece by year')

## ----mean-price-per-set, fig.cap='Average set price (USD) by year.'-----------
ggplot(lego_summary, aes(x = year, y = mean_price)) +
	geom_path() +
	geom_point(aes(size = n_valid)) +
	scale_size('n Sets') +
	scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
	expand_limits(y = 0) +
	ylab('Average set price') + xlab('Year') + 
	ggtitle('Average set price (USD) by year')

## ----mean-pieces-per-set, fig.cap='Average set price (USD) by year.'----------
ggplot(lego_summary, aes(x = year, y = mean_pieces)) +
	geom_path() +
	geom_point(aes(size = n_valid)) +
	scale_size('n Sets') +
	scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
	expand_limits(y = 0) +
	ylab('Average pieces per set') + xlab('Year') + 
	ggtitle('Average number of pieces per set by year')

