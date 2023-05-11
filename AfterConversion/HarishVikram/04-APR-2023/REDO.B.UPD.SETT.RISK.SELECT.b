* @ValidationCode : MjotMTQwNzAxNTAwMjpDcDEyNTI6MTY4MDYwMzYyMTk1NjpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Apr 2023 15:50:21
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.UPD.SETT.RISK.SELECT
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Program   Name    : REDO.B.UPD.SETT.RISK.SELECT
*--------------------------------------------------------------------------------------------------------
*Description       : The routine is the .SELECT routine for the multithreade batch routine
*                    REDO.B.UPD.SETT.RISK. The files of REDO.APAP.FX.LIMIT are selected in this section
*In Parameter      : NA
*Out Parameter     : NA
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                            Reference                      Description
*   ------         ------                         -------------                    -------------
*  11/11/2010  	  A.SabariKumar                   ODR-2010-07-0075                Initial Creation
* 04-APR-2023     Conversion tool  	        	  R22 Auto conversion      	 No changes
* 04-APR-2023      Harishvikram C                 Manual R22 conversion      No changes
*
*********************************************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FX.PARAMETERS
    $INSERT I_F.REDO.APAP.FX.LIMIT
    $INSERT I_REDO.B.UPD.SETT.RISK.COMMON

    GOSUB SEL.REC
RETURN

*--------------------------------------------------------------------------------------------------------
SEL.REC:
*-------
* Calls the core routine EB.READLIST and selects the records of REDO.APAP.FX.LIMIT

    Y.TODAY = TODAY
    IF Y.TODAY GT Y.PARAMETER.DATE ELSE
        SEL.CMD = "SELECT ":FN.REDO.APAP.FX.LIMIT: " WITH PRE.SETT.RISK NE '' AND SETT.RISK EQ ''"
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)
        CALL BATCH.BUILD.LIST('',SEL.LIST)
    END
RETURN

*--------------------------------------------------------------------------------------------------------
END
