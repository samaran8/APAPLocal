* @ValidationCode : MjotNjA4NjM4NjU1OkNwMTI1MjoxNjgyMDcwODQwMTk2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:24:00
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
SUBROUTINE L.APAP.CLEAR.MV.CUSTOMER.MAIL(Y.OFS.TRANSACTION.ID, Y.DYN.MAPPING.IN, Y.DYN.REQUEST.OFS.KEY, Y.DYN.REQUEST.VALUE, Y.DYN.REQUEST.OFS.TYPE, Y.ADDNL.INFO, Y.ERROR)
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER

*CLEAR OUTPUTS VARIABLES
    Y.OFS.IN.REQUEST = ''
    Y.ERROR = ''
    Y.ERROR<3> = 'L.APAP.CLEAR.MV.CUSTOMER.MAIL'

    FN.CUSTOMER = 'F.CUSTOMER'
*FN.CUSTOMER<2> = 'NO.FATAL.ERROR'
    F.CUSTOMER = ''

    Y.LOCAL.REF = 0
    Y.MV.CNT = 0
    Y.SV.CNT = 0
    Y.MV.I = 1
    Y.SV.I = 1

*DEBUG
    Y.ID.COMPANY = "DO0010001"
    IF ID.COMPANY NE Y.ID.COMPANY THEN
        CALL LOAD.COMPANY(Y.ID.COMPANY)
    END

*DEBUG
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)
    CALL F.READ(FN.CUSTOMER,Y.OFS.TRANSACTION.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)

*Y.EB.CUS.LOCAL.REF = R.CUSTOMER<EB.CUS.LOCAL.REF>
*Y.EB.CUS.PHONE.1 = R.CUSTOMER<EB.CUS.PHONE.1>
*Y.EB.CUS.SMS.1 = R.CUSTOMER<EB.CUS.SMS.1>
*Y.EB.CUS.EMAIL.1 = R.CUSTOMER<EB.CUS.EMAIL.1>
*Y.EB.CUS.ADDR.LOCATION = R.CUSTOMER<EB.CUS.ADDR.LOCATION>

*DEBUG
*Y.MV.CNT = DCOUNT(Y.EB.CUS.PHONE.1, @VM)
    R.CUSTOMER<EB.CUS.PHONE.1> = ""
    R.CUSTOMER<EB.CUS.SMS.1> = ""
    R.CUSTOMER<EB.CUS.EMAIL.1> = ""
    R.CUSTOMER<EB.CUS.ADDR.LOCATION> = ""




*CALL F.WRITE(FN.CUSTOMER, ID.CUSTOMER, R.CUSTOMER)
    CALL F.LIVE.WRITE(FN.CUSTOMER, Y.OFS.TRANSACTION.ID, R.CUSTOMER)
    CALL JOURNAL.UPDATE(Y.OFS.TRANSACTION.ID)

RETURN
