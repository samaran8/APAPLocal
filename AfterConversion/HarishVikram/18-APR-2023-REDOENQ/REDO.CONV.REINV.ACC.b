* @ValidationCode : MjoxMzEwNDM0MTk1OkNwMTI1MjoxNjgxODAyNjI2NjE4OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 18 Apr 2023 12:53:46
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
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.CONV.REINV.ACC
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is attached as a conversion routine to the enquiry
* display the field L.AZ.REINVESTED from account application
*-------------------------------------------------------------------------
* HISTORY:
*---------
*   Date               who           Reference            Description

* 16-SEP-2011         RIYAS      ODR-2011-07-0162     Initial Creation
* 18-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.LOOKUP
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT

    GOSUB INITIALSE
    GOSUB CHECK.NOTES

RETURN
*-------------------------------------------------------------------------
INITIALSE:
*~~~~~~~~~

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT  = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    LOC.REF.APPLICATION="ACCOUNT"
    LOC.REF.FIELDS='L.AC.REINVESTED'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AC.REINVESTED = LOC.REF.POS<1,1>
RETURN
*-------------------------------------------------------------------------
CHECK.NOTES:
*~~~~~~~~~~~

    Y.REC.DATA = O.DATA
    CALL F.READ(FN.ACCOUNT,Y.REC.DATA,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
    O.DATA = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.REINVESTED>

RETURN
*-------------------------------------------------------------------------
END
