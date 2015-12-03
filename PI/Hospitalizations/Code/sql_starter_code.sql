
-- hospitalization data for R
declare @startdate datetime = '10/1/2010', @enddate datetime = '9/30/2015'
select distinct
	hosp.case_no, hosp.auth_eff, hosp.auth_exp, hosp.hosp_disc, team_at_admit, auth_days
from encompass.dbo.tblE2_Hosp as hosp
where county = 'Washtenaw' and hosp.cpt_code like '01%'
	and hosp.auth_eff between @startdate and @enddate
	and hosp.contract_paneltype not like 'State Facility%'
go

-- admission data for R
declare @startdate datetime = '10/1/2010', @enddate datetime = '9/30/2015'
select distinct
	adm.case_no, adm.provider_eff as team_eff, adm.provider_exp as team_exp, provider
from encompass.dbo.tblE2_Adm_Consumers as adm
where adm.county = 'Washtenaw' and adm.provider in ('WSH - ACT', 'WSH - ATO' ,
	'WSH - Children''s Services', 'WSH - Children''s Services - Home Based', 
	'WSH - DD Adult', 'WSH - MI - Adult')
	and providertype = 'Direct Provider'
	and adm.provider_eff <= @enddate and (adm.provider_exp >= @startdate or adm.provider_exp is null)
go

/* -- consumers served: try 1 -- too slow
declare @startdate datetime = '10/1/2014', @enddate datetime = '9/30/2015'
select distinct
	svc.case_no, 
	encompass.dbo.E2_Fn_team_at_Given_date('Washtenaw', svc.case_no, svc.service_date) as team_at_service,
	E2_Funding_Bucket.dbo.fn_fiscalQuarter(svc.service_date) as span_label,
	'qtr' as span_type
from encompass.dbo.tblE2_SAL_claims_for4 as svc
where county = 'Washtenaw' and svc.service_date >= @startdate or svc.service_end_date <= @enddate
union
select distinct
	svc.case_no, 
	encompass.dbo.E2_Fn_team_at_Given_date('Washtenaw', svc.case_no, svc.service_date) as team_at_service,
	cast(E2_Funding_Bucket.dbo.fn_fiscalYear(svc.service_date) as nvarchar(4)) as span_label,
	'fy' as span_type
from encompass.dbo.tblE2_SAL_claims_for4 as svc
where county = 'Washtenaw' and svc.service_date >= @startdate or svc.service_end_date <= @enddate 
*/

-- consumers served: try 2
declare @startdate datetime = '7/1/2015', @enddate datetime = '9/30/2015'
select distinct
	Claim.Case_no, CMH.Team, cast(@startdate as date) as span_start, cast(@enddate as date) as span_end
from E2_Fn_Medicaid_Consumers_Served('Washtenaw', @startdate,@enddate) Claim
join PCCClient C on C.CL_RCDID = Claim.CLTID
left join E2_Fn_Active_Clients_Between2 ('Washtenaw', @startdate,@enddate) CMH on CMH.clientID = Claim.CLTID and CMH.county = Claim.county
where Claim.serviceType = 'MH' 
union
select distinct
	Claim.Case_no, CMH.Team, cast(@startdate as date) as span_start, cast(@enddate as date) as span_end
from E2_Fn_ABW_Consumers_Served('Washtenaw', @startdate,@enddate) Claim
join PCCClient C on C.CL_RCDID = Claim.CLTID
left join E2_Fn_Medicaid_Consumers_Served('Washtenaw', @startdate,@enddate) Claim2 on Claim.county = Claim2.county and 
Claim.case_no = Claim2.case_no and Claim2.serviceType = 'MH' 
left join E2_Fn_Active_Clients_Between2 ('Washtenaw', @startdate,@enddate) CMH on CMH.clientID = Claim.CLTID and CMH.county = Claim.county
where  Claim.serviceType = 'MH' and  Claim2.case_no is null
union
select  
	Claim.Case_no, CMH.Team, cast(@startdate as date) as span_start, cast(@enddate as date) as span_end
from tblE2_SAL_claims_for4 Claim
join PCCClient C on C.CL_RCDID = Claim.CLTID
left join E2_Fn_Medicaid_Consumers_Served('Washtenaw', @startdate, @enddate) Claim2 on Claim.county = Claim2.county and 
Claim.case_no = Claim2.case_no and Claim2.serviceType = 'MH' 
left join E2_Fn_ABW_Consumers_Served('Washtenaw', @startdate, @enddate) Claim3 on Claim.county = Claim3.county and 
Claim.case_no = Claim3.case_no and Claim3.serviceType = 'MH' 
left join E2_Fn_Active_Clients_Between2 ('Washtenaw', @startdate,@enddate) CMH on CMH.clientID = Claim.CLTID and CMH.county = Claim.county
where Claim.service_date between  @startdate and @enddate
and Claim.county like 'Washtenaw%'
and  Claim2.case_no is null
and  Claim3.case_no is null
GO



/* code for testing -----
select distinct ProviderType, provider
from encompass.dbo.tblE2_Adm_Consumers
where provider like 'WSH%'

select distinct ProviderType, provider
from encompass.dbo.tblE2_Adm_Consumers
where ProviderType = 'Direct Provider' and -- provider like 'WSH%'
	provider in ('WSH - ACT', 'WSH - ATO' ,'WSH - Children''s Services' ,'WSH - Children''s Services - Home Based' ,'WSH - DD Adult' ,'WSH - MI - Adult')
*/