*----------------------------------------------------------------------------- 
* <Rating>-11</Rating>
*----------------------------------------------------------------------------- 
SUBROUTINE L.APAP.FETCH.CALENDAR.DATE(SYS.DATE) 
*-------------------------------------------------------------------------------- 
*Developed By      :Melvy Martinez
*Program   Name    :REDO.S.FETCH.SYS.DATE 
*--------------------------------------------------------------------------------- 

*DESCRIPTION       : Utilizada para obtener la fecha calendario en formato dd mon yy (e.g. 01 JAN 09) 
* ---------------------------------------------------------------------------------- 
*MODIFICATION HISTORY: 
* DATE            WHO               REFERENCE              DESCRIPTION 
* 15 SEP 2017     Melvy Martinez    CN007041	           Modificación RTE fecha y acumulado fines de semana con fecha del lunes.
*------------------------------------------------------------------------------------------------------------------------------------------------------- 
$INCLUDE T24.BP I_COMMON 
$INCLUDE T24.BP I_EQUATE 

GOSUB PROCESS 
RETURN 
*-------
PROCESS: 
*-------

Y.CAL.TODAY = OCONV(DATE(),"DYMD")
Y.CAL.TODAY = EREPLACE(Y.CAL.TODAY,' ', '')

TEMP.COMI = Y.CAL.TODAY ; TEMP.N1=N1 ; TEMP.T1 = T1 
COMI= Y.CAL.TODAY ; N1=8 ; T1=".D" 
CALL IN2D(N1,T1) 
SYS.DATE = V$DISPLAY 
COMI = TEMP.COMI ; N1 = TEMP.N1 ; T1 = TEMP.T1 

RETURN 
END 
*----------------------------------------------- End Of Record ---------------------------------- 
