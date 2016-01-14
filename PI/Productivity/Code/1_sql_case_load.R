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
	and person.pe_email like '%@ewashtenaw.org'
left join Encompass.dbo.PCCProvider as aff on aff.pr_rcdid = STF_PRVIDA
left join Encompass.dbo.PCFCode as StaffType on Staff.STF_STFTYP = StaffType.CO_RCDID
left join Encompass.dbo.PCCStaffXProvider as Staff_Provider on Staff_Provider.SXF_STFID = Staff.ST_RCDID and Staff_Provider.SX_EXPDT is null
left join Encompass.dbo.PCCProvider as Provider on Staff_Provider.SXF_PRVID = Provider.PR_RCDID
left join Encompass.dbo.PCFCode as COD2 on Provider.PRF_ORGTYP  = COD2.CO_RCDID
join Encompass.dbo.PCFUser as User1 on User1.US_RCDID = Person.PEF_USRID
left join ENCStaff as Super_Staff on Super_Staff.ST_RCDID = ENC_Staff.STF_STFID
left join PCCStaff as Staff2 on Staff2.ST_RCDID = Super_Staff.ST_RCDID --and Staff2.STF_PRVIDA = Adm.ADF_PRVIDA --and Staff2.ST_OKTOUSE = 'Y'
left join PCFPerson as Supervisor on Supervisor.PE_RCDID = Staff2.STF_PERID and Supervisor.PE_EMAIL like '%ewashtenaw.org'
left join PCFRecordLog as RecodLog2 on User1.US_RCDID = RecodLog2.RL_SRCID AND RecodLog2.RL_SRCFILE = 'PCFUSRPF'
and RecodLog2.RL_TYPE ='A' and RecodLog2.RL_UserPGM <> 'CVT141R' -- a: added, d: deleted
where
Staff.ST_RCDID not in (1000027, 1004772, 1008416) and
ltrim(rtrim(Person.PE_LNAME)) not like 'auditor%'
and cast( StaffType.CO_name as varchar(32)) <> 'Contract Provider Billing'
and (COD2.CO_name is null or COD2.CO_name not IN ('Contracted Service Location', 'Vendor', 'SUD Treatment Agency', 'Hospital')  )
and staff.st_email like '%@ewashtenaw.org'
and  user1.US_USRNAME not in (select usrname from tblE2_Provider_Staff PS where PS.staff_email like '%umich.edu')
and User1.US_LOGINTZ is not null
and Aff.PR_name like 'Washtenaw%'"

sql$query$services <-
  sprintf("select distinct *
          from James_CSTS.dbo.jd_f2f_services('%1$s', '%2$s')",
          modify$start_date, modify$end_date)
sql$query$cmh_adm <-
  sprintf("select distinct
            case_no, team, program, assigned_staff, staff_type,
            supervisor, team_effdt, team_expdt, cmh_effdt, cmh_expdt
           from James_CSTS.dbo.jd_cmh_adm('%1$s', '%2$s')",
          modify$start_date, modify$end_date)

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