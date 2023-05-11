* @ValidationCode : MjotMTY4NjQ4NDE5NzpDcDEyNTI6MTY4MjA3ODIwNzk0NDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 17:26:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.FETCH.CALENDAR.DATE(SYS.DATE)
*--------------------------------------------------------------------------------
*Developed By      :Melvy Martinez
*Program   Name    :REDO.S.FETCH.SYS.DATE
*---------------------------------------------------------------------------------

*DESCRIPTION       : Utilizada para obtener la fecha calendario en formato dd mon yy (e.g. 01 JAN 09)
* ----------------------------------------------------------------------------------
*MODIFICATION HISTORY:
* DATE            WHO               REFERENCE              DESCRIPTION
* 15 SEP 2017     Melvy Martinez    CN007041                   Modificación RTE fecha y acumulado fines de semana con fecha del lunes.
*
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-------------------------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion
    $INSERT I_EQUATE ;*R22 Auto conversion

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
