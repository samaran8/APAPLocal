* @ValidationCode : Mjo3ODIwODIwMTY6Q3AxMjUyOjE2ODA2ODk2ODkxNzc6MzMzc3U6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:44:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.CHK.AZ.DEPOSIT.PRINT
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* DESCRIPTION : This routine will be executed at check Record Routine for the deposit versions.
*------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN : -NA-
* OUT : -NA-
*------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : SUDHARSANAN S
* PROGRAM NAME : REDO.CHK.AZ.DEPOSIT.PRINT
*------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE WHO REFERENCE DESCRIPTION
* 08-11-2011 Sudharsanan S CR.18 Initial Creation.
* Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*05/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*05/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
* -----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT

    GOSUB PROCESS

RETURN
*-----------------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------------
    LOC.REF = 'AZ.ACCOUNT'
    LOC.FIELD = 'L.AC.OTH.REASON'
    LOC.POS = ''
    CALL GET.LOC.REF(LOC.REF,LOC.FIELD,LOC.POS)
    POS.L.AC.OTH.REASON = LOC.POS
    CALL ALLOCATE.UNIQUE.TIME(UNIQUE.TIME)
    Y.UNIQ.TIME = FIELD(UNIQUE.TIME,'.',1) + 1
    R.NEW(AZ.LOCAL.REF)<1,POS.L.AC.OTH.REASON> = Y.UNIQ.TIME
RETURN
*---------------------------------------------------------------------------------------------------------------
END
