# Safely quote string for use in SQL "LIKE" query
escape_like_query <- function(string, delimiter, con){
  lhs <- paste0("%", delimiter)
  rhs <- paste(rev(unlist(strsplit(lhs, ""))), collapse = "")
  sapply(paste0(c(lhs, "", lhs, ""), string, c(rhs, lhs, "", "")), dbQuoteString, conn = con)
}

# Combine arbitrary number of vectors elementwise, without recycling short ones
combine_vectors <- function(...){
   dots <- list(...)
   lens <- lengths(dots)
   dots <- lapply(dots, paste0, rep("", max(lens) - min(lens)))
   paste(Reduce(paste0, dots), collapse = "")
}

time_plot <- function(.data, agg_column, group = TRUE){
  agg_column <- enquo(agg_column)
  groups = list(quote(Date))
  if(group) groups <- c(groups, quote(Type))
  .data %>%
  mutate(Date = lubridate::floor_date(Date, unit = "quarter")) %>%
  group_by(!!!groups, .drop = FALSE) %>%
  summarize(!! agg_column  := n()) %>%
  ungroup() %>%
  ggplot(aes(x = Date, y = !!agg_column , color = if(group) Type)) +
  geom_line(show.legend = group) +
  if(group) labs(color = "Type")
}

#Get text embedded in p tags of HTML
extract_text <- function(text){
  text <- gsub("\n", "", text)
  #Remove all non-p tags with their contents
  # See https://www.rexegg.com/regex-quantifiers.html
  text <- gsub("<((?:[^\\\\p]|[a-z]{2,}))[^>]*>(?:[^<]|<(?!\\/\\1))*<\\/\\1>", " ", text, perl = TRUE)
  text <- gsub("<\\/?p>", "", text)
  text
}

validate_df <- function(df, username, message){ 
     validate(need(nrow(df) >=1, sprintf("%s %s", message, username)))
}