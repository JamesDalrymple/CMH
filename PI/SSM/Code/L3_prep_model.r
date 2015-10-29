#### preparing data for modeling ####



# combining categories: correlation

ssm_scall_diff <- ssm_scale[, .SD,
  .SDcols = grep(x=ssm_scale[, grep(x = names(ssm_scale),
  pattern = "^d_", value = TRUE)],
  pattern="child_education$|substance$|childcare$",
  invert = TRUE, value = TRUE)]
setnames(ssm_scall_diff, names(ssm_scall_diff),
         gsub(x=names(ssm_scall_diff), pattern="^d_", replace=""))

# cor_dt <- data.table(melt(cor(ssm_scall_diff,
#          use = "complete.obs", method = "spearman")))
cor_matrix <- cor(ssm_scall_diff, use = "complete.obs", method = "spearman")

p_cor <- corrplot.mixed(
  mar=c(0,0,1.3,0),
  tl.cex = 1,
  tl.pos = "lt", tl.col = "darkred",
  corr = cor_matrix,
  # method = "shade",
  # type = "lower",
  title = expression(paste(
    "Spearman ", rho, " Plot of Differences (Post - Pre)")),
  addgrid.col = "grey30",
  addCoef.col = "black",
  addCoefasPercent = TRUE,
  order = "FPC", #  "FPC", "AOE", "hclust", "alphabet"
  bg = "black",
  addshade=NULL,
  lower = "ellipse",
  upper = "shade",
  col = col2(100),
  addrect = 2
)


# col1 <-
#   colorRampPalette(
#     c(
#       "#7F0000", "red", "#FF7F00", "yellow", "white", "cyan",
#       "#007FFF", "blue", "#00007F"
#     )
#   )
# col2 <-
#   colorRampPalette(
#     c(
#       "#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
#       "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"
#     )
#   )
# col3 <- colorRampPalette(c("red", "white", "blue"))
# col4 <-
#   colorRampPalette(
#     c(
#       "#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
#       "cyan", "#007FFF", "blue", "#00007F"
#     )
#   )
# wb <- c("white", "black")
# col1(100)




?corrplot
