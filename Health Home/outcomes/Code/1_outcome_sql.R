# consumer pool requirements:
# 1. be 18+ years old on vital date/wellness_note_date
# 2. be 18+ years for at least a year so that we have the potential to have
# a before and after for (a) BMI, (b) wellness overall health, (c) blood press.
# 3. be on  Access, MI, DD, ACT team for at least 1 year so that we have
# potential for a before and after (see #2 for why)
# 4. current closed consumers count if they were with us for at least a year
# and fit requirements 1-3.
# 5. team is last given CMH  team. perhaps if this is difficult, use admission
# records with priority data.table.
sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list()
)

# CMH admissions ---
# team timeline: WSH - Pilot Disease Management <= WSH - SAMHSA PBHCI <= WSH - Health Home
# assigned_staff = 'SAMHSA' for all queries to find if nurse assigned
sql$query$cmh_adm <-
"select distinct
  case_no, team2 as team, cmh_effdt, cmh_expdt, team_effdt, team_expdt, dob,
  staff_eff, staff_exp, assigned_staff, staff_type, supervisor as current_sup
from encompass.dbo.tblE2_CMH_Adm_Consumers_w_OBRA
where county = 'Washtenaw'"

# vitals: perhaps add a filter to get WCCMH consumers during a date range? ---
sql$query$vitals <-
"select distinct
case_no, vt_date, diastolic, systolic, weight, height_feet, height_inches,
bmi, pulse, respirationRate as respiration_rate, staff, staff_type,
smokingStatus as smoke_status
from encompass.dbo.tblE2_Vitals"

# services ---
# /* use productivity services: case_no, doc_date, doc_type, doc_author, doc_staff_type, f2f */

# wellness note: overall health, pain ---
sql$query$wn <-
"select distinct
  case_no, wellnessnote_date as wn_date,
  overall_health_rating as ovr_health, pain
from encompass.dbo.tblE2_WellnessNote_Header
where county = 'Washtenaw'"

# lab values ---
sql$query$labs <-
"select case_no, lab_name, lab_value, lab_result_date as lab_date
from tblE2_Consumers_Lab_Result_Both
where county = 'Washtenaw'"

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(query = get(x, with(sql, query)),
               channel = sql$channel, stringsAsFactors = FALSE)
    output <- data.table(output)
    return(output)
  },
  USE.NAMES = TRUE
)