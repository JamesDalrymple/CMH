#### L2: reshaping the data ####

attach(sql$sql_output) # details is a new copy at this point
attr(details, 'base_cols') <-
  gsub(
    x = grep(
      x = names(details),
      pattern = "1",
      value = TRUE
    ),
    pattern = "1",
    replace = ""
  )
# base_cols <- gsub(x=grep(x=colnames(ssm_details), pattern="1", value=TRUE), pattern="1", replace="")

attr(details, 'initial_cols') <- paste0(attr(details, 'base_cols'), "1")
attr(details, 'end_cols') <- paste0(attr(details, 'base_cols'), "2")
attr(details, 'diff_columns') <- paste0("d_", attr(details, 'base_cols'))

demo <- demo[, .SD, .SDcols =
               c("case_no",
                 "gender",
                 "race1",
                 "race2",
                 "race3",
                 "hispanic",
                 "age",
                 "zipcode_res")]

ssm_scale <- mmerge(details,
                    cls,
                    demo,
                    all.x = TRUE, by = "case_no")
setkey(ssm_scale, NULL)
detach(sql$sql_output)


# ordinal approach - look for any change
ssm_scale[, attr(details, 'diff_columns') :=
            list(
              income2 - income1,
              employment2 - employment1,
              shelter2 - shelter1,
              food2 - food1,
              education2 - education1,
              legal2 - legal1,
              health_care2 - health_care1,
              life_skills2 - life_skills1,
              mental_health2 - mental_health1,
              substance2 - substance1,
              family_friends2 - family_friends1,
              mobility2 - mobility1,
              community2 - community1,
              childcare2 - childcare1,
              child_education2 - child_education1,
              healthcare2 - healthcare1
            )]
rm(demo, details)



# create total_improvement column: 0 = no change, negative value = decrease, positive value = improvement

# #### categorical approach - look for a category change ####
# ssm_cat <- copy(ssm_details)
# # apply my_safe to all data columns
# for(j in c(initial_cols, end_cols)) set(ssm_cat, j=j, value=my_safe(ssm_cat[[j]]))
# rm(j)
#
# # add new columns
# for (i in seq_along(base_cols)) {
# setkeyv(ssm_cat, c(initial_cols[i], end_cols[i]))
# new_cols <- c(paste(base_cols[i], "cat_long", sep="_"), paste(base_cols[i], "cat_short", sep="_"))
# ssm_cat[J("safe", "safe"), c(new_cols) := list("safe with no improvement", 0)]
# ssm_cat[J("safe", "not_safe"), c(new_cols) := list("decrease to not_safe", -1)]
# ssm_cat[J("not_safe", "safe"), c(new_cols) := list("improved to safe", 1)]
# ssm_cat[J("not_safe", "not_safe"), c(new_cols) := list("not_safe with no improvement", 0)]
# }
# ssm_cat


