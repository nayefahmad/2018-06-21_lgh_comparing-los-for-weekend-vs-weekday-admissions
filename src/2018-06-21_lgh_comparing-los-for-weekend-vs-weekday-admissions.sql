
-----------------------------------------------------------------------
-- LGH: COMPARING LOS OF WEEKEND VS WEEKDAY ADMISSIONS 
-----------------------------------------------------------------------
-- Created: 2018-06-21

SELECT [AdmissionFacilityLongName]
	  , AdjustedAdmissionDate
	  , Datename(dw, AdjustedAdmissionDate) as day_of_week
      ,[MRN] -- 
      ,[PatientId] --
      ,[AdmissionAge] -- 
      ,[IsHomeless] -- 
      ,[AdmissionNursingUnitCode] -- 
      ,[LOSDays] --
  FROM [ADTCMart].[ADTC].[AdmissionDischargeView]
  where AdmissionFacilityLongName = 'Lions Gate Hospital' 
  and AdjustedAdmissionDate between '2017-01-01' and '2017-12-31' 

