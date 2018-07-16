
-----------------------------------------------------------------------
-- LGH: COMPARING LOS OF WEEKEND VS WEEKDAY ADMISSIONS 
-----------------------------------------------------------------------
-- Created: 2018-06-21

SELECT [AdmissionFacilityLongName]
	  ,[AdjustedAdmissionDate]
	  ,(year([AdjustedAdmissionDate]) - 2016) as yearsFrom2016
	  ,Datename(dw, AdjustedAdmissionDate) as day_of_week
      ,[PatientId] --
      ,[AdmissionAge] -- 
	  ,[Gender]
      ,[IsHomeless] -- 
      ,[AdmissionNursingUnitCode] -- 
	  ,[AdmissionNursingUnit] 
	  ,[AdmissionPatientServiceCode]
      ,[LOSDays] --

FROM [ADTCMart].[ADTC].[AdmissionDischargeView]
  
where AdmissionFacilityLongName = 'Lions Gate Hospital' 
	and AdjustedAdmissionDate between '2016-01-01' and '2017-12-31' 
	and AdmissionNursingUnitCode not in ('EIP', 'N/A', 'en1', 'en2', 'nsy', 'nsh', '3po', 'dcs', 'es1', 'es2', 'es3', 'end', 'nsh ssh' , 'scn') 
	--and AdmissionNursingUnitCode  = '4e' 

order by AdjustedAdmissionDate
	, [PatientId]



