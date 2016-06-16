# cohorts ---------------------------------------------------------------------
# pre/post HH is a primary grouping
# pre/post HH without nurse vs HH with nurse SHOULD be considered

hh_tier <- copy(modify$tiers)
hh_services <- copy(modify$hh_services)
hh_detail <- copy(modify$hh_detail)
cc360_main <- copy(modify$cc360_main)
cc360_med <- copy(modify$cc360_med)
hh_bucket <- copy(modify$hh_bucket)


sqldf("select
  case_no, min(team_effdt) as first_hh_dt
      from hh_detail
      group by case_no", drv = "SQLite")


data.table(
sqldf("select distinct
  first.case_no, first.first_hh_dt,
  strftime('%m-%Y', first.first_hh_dt) AS month,
  tier.util_level, tier.tier, tier.num_ER_last180,
  tier.num_PH_IPs_last180, tier.num_ER_or_PH_IP_last180,
  hh_adm.gender, hh_adm.MI, hh_adm.DD,
  case when nurse.hh_nurse is null then 'N' else 'Y' end as hh_nurse_ever,
  bkt.hh_bucket
  from (select
  first_hh.case_no, min(first_hh.team_effdt) as first_hh_dt
  from hh_detail as first_hh
  group by first_hh.case_no) as first
  left join hh_tier as tier on
    first.case_no = tier.case_no and
    strftime('%m-%Y', first.first_hh_dt) = strftime('%m-%Y', tier.end_date)
  left join hh_detail as hh_adm on
    hh_adm.case_no = first.case_no and
    hh_adm.team_effdt = first.first_hh_dt
  left join hh_detail as nurse on
    nurse.case_no = first.case_no and nurse.hh_nurse = 'Y' and
    hh_adm.team_effdt = first.first_hh_dt
  left join hh_bucket as bkt on
    first.case_no = bkt.case_no
    ", drv = "SQLite")
)


modify$hh_detail[, list(first_hh_date = min(team_effdt)), by = case_no]
