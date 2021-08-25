# Safely quote string for use in SQL "LIKE" query
escape_like_query <- function(string, delimiter, con){
  lhs <- paste0("%", delimiter)
  rhs <- paste(rev(unlist(strsplit(lhs, ""))), collapse = "")
  sapply(paste0(c(lhs, "", lhs, ""), string, c(rhs, rhs, "", "")), dbQuoteString,
         conn = con, USE.NAMES = FALSE)
}

# Combine arbitrary number of vectors elementwise, without recycling short ones
combine_vectors <- function(...){
   dots <- list(...)
   lens <- lengths(dots)
   dots <- lapply(dots, paste0, rep("", max(lens) - min(lens)))
   paste(Reduce(paste0, dots), collapse = "")
}

time_plot <- function(.data, agg_column, agg_expr = n(), group = TRUE){
  agg_column <- enquo(agg_column)
  agg_expr <- enquo(agg_expr)
  groups = list(quote(Date))
  
  if(group) groups <- c(groups, quote(Type))
  #Complicated business to fill in implicit NA (i.e., dates with no
  # activity)
  .data %>%
  mutate(Date = lubridate::floor_date(Date, unit = "quarter")) %>%
  group_by(!!!groups, .drop = FALSE) %>%
  summarize(!!agg_column  := !!agg_expr, .groups = "drop") %>%
  right_join(tibble(Date = seq(min(.$Date), max(.$Date), "quarter"))) %>% 
  complete(!!!groups, fill = setNames(list(0), as_label(agg_column))) %>%   
  ggplot(aes(x = Date, y = !!agg_column , color = if(group) Type)) +
  geom_line(show.legend = group) +
    labs(y = as_label(agg_column))+
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

tags_table <- function(tags_vector, input, exclude = "" ){
  raw_tags <- tags_vector[complete.cases(tags_vector)]
  validate(need(raw_tags, paste("No tags associated with", input)))
  all_tags <-
    unlist(lapply(raw_tags, function(x) {
      unlist(strsplit(x, "\\|"))
    }))
  tab <- table(all_tags[all_tags != exclude])
  tab <-
    sort(tab, decreasing = TRUE)[1:(min(length(tab), 5))]
  as_tibble(as.list(tab))
  
}

