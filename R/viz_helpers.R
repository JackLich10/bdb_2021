### Visualization helpers

# Function to make names pretty with html
first_last <- function(name) {
  first <- stringr::word(name, 1)
  last <- stringr::str_trim(stringr::str_extract(name, " .*"))
  glue::glue("<div style='line-height:11px'><span style ='font-weight:bold;color:grey;font-size:10px'>{first}</span></div>\n    <div style='line-height:9px'><span style='font-weight:bold;font-variant:small-caps;font-size:13px'>{last}</div>")
}

# Function to make player attributes pretty with html
combine_attrs <- function(number, position, team) {
  glue::glue("<div style='line-height:11px'><span style ='font-weight:bold;color:grey;font-size:13px'>{number}</span></div>\n    <div style='line-height:9px'><span style='font-weight:bold;font-variant:small-caps;font-size:9px'>{position}&nbsp;&nbsp;{team}</span></div>")
}

# Function to make a color palette
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

# Good color palette
good_color <- make_color_pal(c("#35b0ab", "#93d3ab", "#c9ecb4", "#f2fbd2", 
                               "#ffffff"), bias = 2)

# Reverse color palette
reverse_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", 
                                  "#35b0ab"), bias = 2)

# Function to make a reactable table for receivers
reactable_pr_table <- function(pr_summarized, min_atts = 10) {
  
  # reactable table
  pr_summarized <- pr_summarized %>%
    dplyr::filter(return_attempts >= min_atts)
  
  # find color bounds
  min_net <- min(pr_summarized$diff_net)
  max_net <- max(pr_summarized$diff_net)
  
  min_epa <- min(pr_summarized$diff_epa)
  max_epa <- max(pr_summarized$diff_epa)
  
  # reactable table
  pr_summarized %>%
    dplyr::arrange(dplyr::desc(diff_epa)) %>% 
    dplyr::mutate(name = first_last(name = player_name),
                  attrs = combine_attrs(number = jersey, position = position, team = team),
                  name = purrr::map(name, gt::html),
                  attrs = purrr::map(attrs, gt::html)) %>% 
    dplyr::select(headshot_url, name, attrs, return_attempts, touchdowns, 
                  # fumbles,
                  returns, pr_yards, net, act_epa,
                  exp_returns, exp_pr_yards, exp_net, exp_epa,
                  diff_net, diff_epa) %>% 
    reactable::reactable(pagination = TRUE,
                         compact = TRUE,
                         borderless = FALSE,
                         striped = FALSE,
                         fullWidth = TRUE,
                         pageSizeOptions = c(10, 50, 100),
                         showPageSizeOptions = TRUE,
                         defaultPageSize = 10,
                         theme = reactable::reactableTheme(
                           headerStyle = list(
                             "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                             "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                             borderColor = "#555")
                         ),
                         defaultColDef = reactable::colDef(align = "center",
                                                           minWidth = 65,
                                                           headerStyle = list(`font-size` = "12px"),
                                                           footerStyle = list(fontWeight = "bold"),
                                                           na = "–",
                                                           sortNALast = T,
                         ),
                         columns = list(headshot_url = reactable::colDef(name = "",
                                                                         sortable = FALSE,
                                                                         width = 57.5,
                                                                         cell = function(value) {
                                                                           image <- shiny::img(src = value, height = "35px", width = "35px",
                                                                                               # width = "50px",
                                                                                               alt = value, align = "center")
                                                                         }),
                                        name = reactable::colDef(name = "",
                                                                 sortable = FALSE,
                                                                 align = "left",
                                                                 width = 73,
                                                                 html = TRUE),
                                        attrs = reactable::colDef(name = "",
                                                                  sortable = FALSE,
                                                                  align = "center",
                                                                  width = 52.5,
                                                                  html = TRUE),
                                        return_attempts = reactable::colDef(name = "Attempts"),
                                        touchdowns = reactable::colDef(name = "TDs",
                                                                       width = 55,
                                                                       format = reactable::colFormat(digits = 0)),
                                        # fumbles = reactable::colDef(name = "Fumbles",
                                        #                             width = 60,
                                        #                             format = reactable::colFormat(digits = 0)),
                                        returns = reactable::colDef(name = "Returns"),
                                        pr_yards = reactable::colDef(name = "Yards",
                                                                     format = reactable::colFormat(digits = 1)),
                                        net = reactable::colDef(name = "FP",
                                                                format = reactable::colFormat(digits = 1)),
                                        act_epa = reactable::colDef(name = "EPA",
                                                                    format = reactable::colFormat(digits = 2)),
                                        exp_returns = reactable::colDef(name = "Returns",
                                                                        format = reactable::colFormat(digits = 1)),
                                        exp_pr_yards = reactable::colDef(name = "Yards",
                                                                         format = reactable::colFormat(digits = 1)),
                                        exp_net = reactable::colDef(name = "FP",
                                                                    format = reactable::colFormat(digits = 1)),
                                        exp_epa = reactable::colDef(name = "EPA",
                                                                    format = reactable::colFormat(digits = 2)),
                                        diff_net = reactable::colDef(name = "FP",
                                                                     format = reactable::colFormat(digits = 1),
                                                                     style = function(value) {
                                                                       if (is.na(value)) {
                                                                         color <- "#ffffff"
                                                                       } else {
                                                                         normalized <- (value - min_net) / (max_net - min_net)
                                                                         color <- reverse_color(normalized)
                                                                       }
                                                                       list(background = color)
                                                                     },
                                                                     # add a border to the left of this column
                                                                     class = "border-left"),
                                        diff_epa = reactable::colDef(name = "EPA",
                                                                     format = reactable::colFormat(digits = 2),
                                                                     style = function(value) {
                                                                       if (is.na(value)) {
                                                                         color <- "#ffffff"
                                                                       } else {
                                                                         normalized <- (value - min_epa) / (max_epa - min_epa)
                                                                         color <- reverse_color(normalized)
                                                                       }
                                                                       list(background = color)
                                                                     })
                         ),
                         columnGroups = list(
                           reactable::colGroup(name = "Actual", 
                                               columns = c("returns", "pr_yards", "net", "act_epa")),
                           reactable::colGroup(name = "Expected", 
                                               columns = c("exp_returns", "exp_pr_yards", "exp_net", "exp_epa")),
                           reactable::colGroup(name = "Over Expected", 
                                               columns = c("diff_net", "diff_epa"))
                         )
    )
}

# pr_summarized %>% 
#   ggplot(aes(returns - exp_returns, diff_net)) +
#   geom_smooth()
#   geom_point()

# pr_summarized %>% 
#   filter(return_attempts >= 10) %>% 
#   mutate(touchback_pct = touchbacks/let_go) %>% View



