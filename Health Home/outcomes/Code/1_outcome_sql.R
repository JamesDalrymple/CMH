# funding bucket -- must download manually -- ranged point in  time data
sql <- new.env(parent=.GlobalEnv)
sql$channel <- odbcConnect("WSHSQLGP")

# current state hospital consumers
sql$query$outcome <-
"select distinct
  HH.case_no, hh.dob, hh.team, hh.cmh_effdt, hh.hh_team, hh.hh_adm_date,
  hh.samhsa_staff, hh.stafftype,
  V.VT_DATE as vt_date, V.BMI as bmi, V.diastolic,
  V.systolic, V.weight, V.height_feet, V.height_inches
from encompass.dbo.E2_Fn_CMH_Open_w_HH_Team_or_not() HH
left join encompass.dbo.tblE2_Vitals V on HH.Case_No = V.Case_No
  and ( V.VT_DATE >= HH_adm_date or  (HH_team is null and
  V.VT_DATE >= CMH_effdt and V.VT_DATE >= '7/1/14'))"

sql$query$wellness <-
"select distinct HH.*, W.WellnessNote_date, W.Overall_Health_Rating
from E2_Fn_CMH_Open_w_HH_Team_or_not() HH
left join tblE2_WellnessNote_Header W on HH.Case_No = W.Case_No
  and ( W.WellnessNote_date >= HH_adm_date or
  (HH_team is null and  W.WellnessNote_date>= CMH_effdt
  and  W.WellnessNote_date >= '7/1/14'))
  and W.Overall_Health_Rating is not null"

sql$output <-
      sqlQuery(query = get("outcome", with(sql, query)),
        channel = sql$channel, stringsAsFactors = FALSE)
sql$output <- data.table(sql$output)

sql$wellness <-
  sqlQuery(query = get("wellness", with(sql, query)),
           channel = sql$channel, stringsAsFactors = FALSE)
sql$wellness <- data.table(sql$wellness)