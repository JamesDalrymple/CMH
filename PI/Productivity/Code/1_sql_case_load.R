sql <- list(
  channel = odbcConnect("WSHSQLGP"),
  query = list())

sql$query$prescribers <- "select distinct
	ltrim(rtrim( Person.PE_LNAME)) + ', ' + ltrim(rtrim( Person.PE_FNAME)) as staff_name
from encompass.dbo.PCCStaff as Staff
join encompass.dbo.PCFPerson as Person on Person.PE_RCDID = Staff.STF_PERID
join encompass.dbo.PCCProvider aff on aff.pr_rcdid = STF_PRVIDA
join encompass.dbo.PCCStaffCredentials Cred on Cred.STF_STFID = Staff.ST_RCDID
join encompass.dbo.PCFCode Cred_type on Cred_type.CO_RCDID = Cred.STF_CRDTYP
left join encompass.dbo.PCFCode Cred_Sub_type on Cred_Sub_type.CO_RCDID = Cred.STF_SUBCRD
join encompass.dbo.PCFCode StaffType on Staff.STF_STFTYP = StaffType.CO_RCDID
where STF_PRVIDA is not null  -- CMH staff
and staff.ST_OKTOUSE = 'Y'  -- active staff
and ( getdate() between Cred.ST_FROMDT and Cred.ST_THRUDT
or ( getdate() >= Cred.ST_FROMDT and Cred.ST_THRUDT is null))
and Aff.PR_name like '%Washtenaw%' and StaffType.CO_name='Psychiatrist / PA / NP'"

# note 1: not all CSTS staff have a @ewashtenaw.org in their E.II profile
# note 2: cannot restict to active staff or active user
sql$query$staff_hr <- "select distinct
case when cast(Aff.PR_name as varchar(60)) like 'Lenawee%' then 'Lenawee'
when cast(Aff.PR_name as varchar(60)) like 'Monroe%' then 'Monroe'
when cast(Aff.PR_name as varchar(60)) like '%Livingston%' then 'Livingston'
when cast(Aff.PR_name as varchar(60)) like 'Washtenaw%' then  'Washtenaw'
when cast(Aff.PR_name as varchar(60)) =
'Community Mental Health Partnership of Southeast Michigan-SU'
then 'CMHPSM-SUD' else cast(Aff.PR_name as varchar(60)) end as county,
ltrim(rtrim(Person.PE_LNAME)) +', '+ ltrim(rtrim(Person.PE_FNAME)) as staff,
cast( StaffType.CO_name as varchar(32)) as staff_type,
cast(Staff.ST_TITLE as varchar(32)) as staff_title,
user1.US_LOGINTZ as last_login_dt,
ENC_Staff.ST_HIREDT hire_dt,
ENC_staff.ST_TERMDT as termination_dt,
ltrim(rtrim( Supervisor.PE_LNAME)) +', '+
ltrim(rtrim(Supervisor.PE_FNAME)) as Supervisor,
cast(RecodLog2.RL_LOGTZ as DATE) as user_add_date,
provider.PR_NAME  as provider_name,
staff.ST_OKTOUSE as staff_ok_to_use,
user1.US_OKTOUSE as user_ok_to_use
from Encompass.dbo.PCCStaff as staff
join encompass.dbo.ENCStaff ENC_Staff on Staff.ST_RCDID = ENC_Staff.ST_RCDID
join Encompass.dbo.PCFPerson Person on Person.PE_RCDID = Staff.STF_PERID
left join Encompass.dbo.PCCProvider as aff on aff.pr_rcdid = STF_PRVIDA
left join Encompass.dbo.PCFCode as StaffType on
Staff.STF_STFTYP = StaffType.CO_RCDID
left join Encompass.dbo.PCCStaffXProvider as Staff_Provider on
Staff_Provider.SXF_STFID = Staff.ST_RCDID and Staff_Provider.SX_EXPDT is null
left join Encompass.dbo.PCCProvider as Provider on
Staff_Provider.SXF_PRVID = Provider.PR_RCDID
left join Encompass.dbo.PCFCode as COD2 on Provider.PRF_ORGTYP  = COD2.CO_RCDID
join Encompass.dbo.PCFUser as User1 on User1.US_RCDID = Person.PEF_USRID
left join ENCStaff as Super_Staff on Super_Staff.ST_RCDID = ENC_Staff.STF_STFID
left join PCCStaff as Staff2 on Staff2.ST_RCDID = Super_Staff.ST_RCDID
left join PCFPerson as Supervisor on Supervisor.PE_RCDID = Staff2.STF_PERID
and Supervisor.PE_EMAIL like '%ewashtenaw.org'
left join PCFRecordLog as RecodLog2 on User1.US_RCDID = RecodLog2.RL_SRCID AND
RecodLog2.RL_SRCFILE = 'PCFUSRPF'
and RecodLog2.RL_TYPE ='A' and RecodLog2.RL_UserPGM <> 'CVT141R' -- a: added, d: deleted
where
Staff.ST_RCDID not in (1000027, 1004772, 1008416) and
ltrim(rtrim(Person.PE_LNAME)) not like 'auditor%'
and cast( StaffType.CO_name as varchar(32)) <> 'Contract Provider Billing'
and (COD2.CO_name is null or COD2.CO_name not IN
('Contracted Service Location', 'Vendor', 'SUD Treatment Agency', 'Hospital'))
and  user1.US_USRNAME not in
(select usrname from tblE2_Provider_Staff PS
where PS.staff_email like '%umich.edu')
and User1.US_LOGINTZ is not null
and StaffType.CO_name not like ('Rights Officer%')"

# services ---
sql$query$services <- # in a list format b/c R has a limit for sprintf
  list(
# bio-psycho-social, intake assessment
"select distinct
	doc.case_no, cast(doc.DO_DATE as date) as doc_date,
	doc.begintime as begin_time, doc.endtime as end_time,
	doc.staff as author, doc.supervisor, bps.in_facetof as f2f,
	doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCInitialIntake as bps
join encompass.dbo.tblE2_document doc on bps.in_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# Emergency Note
"select distinct
	doc.case_no, doc.do_date as doc_date, doc.begintime as begin_time,
	doc.endtime as end_time, doc.staff as author, doc.supervisor,
	erm_note.ER_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCEmergencyNote as erm_note
join encompass.dbo.tblE2_document doc on erm_note.ER_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# SIS Assessment (2 ppl work at the PHIP, and we can safely assume f2f = 'Y'
"select distinct
doc.case_no, doc.do_date as doc_date, doc.begintime as begin_time,
doc.endtime as end_time, doc.staff as author, doc.supervisor,
'Y' as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.PCCSISAssessment as sis
join encompass.dbo.tblE2_document doc on sis.SI_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# Injection/Dispense Note
"select distinct
	doc.case_no, doc.do_date as doc_date, doc.begintime as begin_time,
	doc.endtime as end_time, doc.staff as author, doc.supervisor,
	phr.ID_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCInjectionDispenseNote as phr
join encompass.dbo.tblE2_document doc on phr.ID_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# Personal Health Review
"select distinct
  doc.case_no, doc.do_date as doc_date, doc.begintime as begin_time,
  doc.endtime as end_time, doc.staff as author, doc.supervisor,
  phr.PH_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCPersonalHealthReview as phr
join encompass.dbo.tblE2_document doc on phr.PH_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# Pre-Screening Assessment
"select distinct
	doc.case_no, doc.do_date as doc_date, doc.begintime as begin_time,
	doc.endtime as end_time, doc.staff as author, doc.supervisor,
	prescreen.PS_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCPreScreening as prescreen
join encompass.dbo.tblE2_document doc on prescreen.PS_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# psychiatric evaluation
"select distinct
	doc.case_no, cast(doc.do_date as date) as doc_date,	doc.begintime as begin_time,
doc.endtime as end_time, doc.staff as author, doc.supervisor,
pe.IP_FACETOF as f2f, doc.DO_TITLE as doc_type, doc.staff_type
from encompass.dbo.ENCInitialPsychiatricEvaluation as pe
join encompass.dbo.tblE2_document doc on PE.IP_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# medication review note
"select  distinct
	doc.Case_No, cast(doc.DO_DATE as date) as doc_date,
	doc.begintime as begin_time, doc.endtime as end_time,
	doc.staff as author, doc.supervisor,
	mr.MR_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCMedicationReviewNote as mr
join encompass.dbo.tblE2_document as doc on mr.MR_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# screening form: P is phone requrest, W is walk-in request
"select distinct
	 doc.Case_No, cast(doc.DO_DATE as date) as doc_date,
doc.begintime as begin_time, doc.endtime as end_time,
doc.staff as author, doc.supervisor,
case SC_CONTYPE
  when 'P' then 'N'
  when 'W' then 'Y'
  else SC_CONTYPE end as f2f,
doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCScreeningCall as SCall
join encompass.dbo.tblE2_document as doc on SCall.SC_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# specialized documents: OT Evaluation, Psychologist Evaluation,
# Nutrition Assessment, (3/6 used in WSH)
"select distinct
	doc.case_no, cast(doc.DO_DATE as date) as doc_date,
doc.begintime as begin_time, doc.endtime as end_time,
doc.staff as author, doc.supervisor, 'Y' as f2f,
doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCSpecializedAssessment as spec
join encompass.dbo.tblE2_document doc on spec.sp_RCDID  = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# wellness note
"select  distinct
	doc.case_no, cast(doc.DO_DATE as date) as doc_date,
doc.begintime as begin_time, doc.endtime as end_time,
doc.staff as author, doc.supervisor,
wn.WN_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCWellnessNoteHeader as WN
join encompass.dbo.tblE2_document doc on WN.WN_RCDID = doc.DO_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'",
"union",
# progress note
"select distinct
	doc.case_no, cast(doc.DO_DATE as date) as doc_date,
doc.begintime as begin_time, doc.endtime as end_time,
doc.staff as author, doc.supervisor,
PN.PR_FACETOF as f2f, doc.do_title as doc_type, doc.staff_type
from encompass.dbo.ENCProgressNote as PN
join encompass.dbo.tblE2_document as doc on doc.DO_RCDID = PR_RCDID
where doc.DO_DATE between '%1$s' and '%2$s' and doc.county = '%3$s'")
# sprintf has a 8192 character limit, so we break it up into lists
# and reassemble it
sql$query$services <-
  rapply(sql$query$services, f = function(x) {
    sprintf(fmt = x, modify$start_date, modify$end_date, "Washtenaw")
  })
sql$query$services <-
  lapply(sql$query$services, FUN = function(x) {
    result = gsub(x = x, pattern = "\n|\t", replacement = " ")
    result = gsub(x = result, pattern = "(?<=[\\s])\\s*|^\\s+|\\s+$",
                  replacement = "", perl = TRUE)
    return(result)
  })
sql$query$services <- paste(sql$query$services, collapse =" ")

# admissions ---
sql$query$cmh_adm <-
  sprintf("select distinct
  adm.case_no, adm.team2 as team, adm.assigned_staff, adm.staff_type,
  adm.supervisor, adm.team_effdt, adm.team_expdt, adm.cmh_effdt, adm.cmh_expdt,
  adm.staff_eff, adm.staff_exp
from encompass.dbo.tblE2_CMH_Adm_Consumers as adm
where adm.county = '%3$s' and
	team2 in ('WSH - DD Adult', 'WSH - MI - Adult', 'WSH - ACT', 'WSH - ATO',
	'WSH - Children''s Services', 'WSH - Children''s Services - Home Based',
	'WSH - Access/Engagement', 'Washtenaw County Community Mental Health') and
	(adm.team_effdt <= '%2$s' and (adm.team_expdt >= '%1$s' or adm.team_expdt is null)) and
	(adm.staff_eff <= '%2$s' and (adm.staff_exp >= '%1$s' or adm.staff_exp is null))",
  modify$start_date, modify$end_date, "Washtenaw")

sql$output <- sapply(
  names(sql$query),
  FUN = function(x) {
    output <-
      sqlQuery(query = get(x, with(sql, query)),
               channel = sql$channel, stringsAsFactors = FALSE)
    output <- data.table(output)
    # assign(x, output, envir = sql)
    return(output)
  },  USE.NAMES = TRUE)