* @ValidationCode : MjotMTkzNTY2MDk1NTpDcDEyNTI6MTY4MjA2NTI3Nzc4OTphaml0aDotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 13:51:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.ATM
*MODIFICATION HISTORY:
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*21-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*21-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------




***This subroutine is attached at INTRF.MAPPING
*** This routine will get branch code of the account

SUBROUTINE AT.GET.ACCT.BRANCH(INCOMING,RETURNVAL)
*
    RETURNVAL = INCOMING[1,3]
RETURN
END
