*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.B.OVR.LOAN.REPORT.LOAD

    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.DATES
    $INCLUDE T24.BP I_F.AA.ARRANGEMENT
    $INCLUDE T24.BP I_F.ACCOUNT
    $INCLUDE T24.BP I_F.AA.INTEREST.ACCRUALS
    $INCLUDE T24.BP I_F.AA.INTEREST
    $INCLUDE T24.BP I_F.AA.CUSTOMER
    $INCLUDE T24.BP I_F.AA.OVERDUE
    $INCLUDE T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INCLUDE T24.BP I_F.CUSTOMER
    $INCLUDE T24.BP I_F.AA.ACCOUNT.DETAILS
    $INCLUDE T24.BP I_F.AA.BILL.DETAILS
    $INCLUDE T24.BP I_F.AA.ACTIVITY.HISTORY
    $INCLUDE T24.BP I_ENQUIRY.COMMON
    $INCLUDE T24.BP I_F.EB.LOOKUP
    $INCLUDE TAM.BP I_F.REDO.H.CUSTOMER.PROVISIONING
    $INCLUDE LAPAP.BP I_REDO.B.OVR.LOAN.REPORT.COMMON
    $INCLUDE T24.BP I_F.DATES
    $INCLUDE T24.BP I_F.AA.PAYMENT.SCHEDULE
    $INCLUDE TAM.BP I_F.REDO.CAMPAIGN.TYPES
    $INCLUDE T24.BP I_F.AA.PRODUCT


    GOSUB OPEN.FILES
    GOSUB GET.LOC.DETAILS

    RETURN

OPEN.FILES:
**********

    FN.AA = 'F.AA.ARRANGEMENT'
    F.AA = ''
    CALL OPF(FN.AA,F.AA)

    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    CALL OPF(FN.AC,F.AC)

    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AAA = ''
    CALL OPF(FN.AAA,F.AAA)

    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    CALL OPF(FN.CUS,F.CUS)

    FN.AA.AC = 'F.AA.ACCOUNT.DETAILS'
    F.AA.AC = ''
    CALL OPF(FN.AA.AC,F.AA.AC)

    FN.AA.INT = 'F.AA.INTEREST.ACCRUALS'
    F.AA.INT = ''
    CALL OPF(FN.AA.INT,F.AA.INT)

    FN.AA.BILL = 'F.AA.BILL.DETAILS'
    F.AA.BILL = ''
    CALL OPF(FN.AA.BILL,F.AA.BILL)

    FN.AA.HIS = 'F.AA.ACTIVITY.HISTORY'
    F.AA.HIS = ''
    CALL OPF(FN.AA.HIS,F.AA.HIS)

    FN.EB.LOOKUP = 'F.EB.LOOKUP'
    F.EB.LOOKUP = ''
    CALL OPF(FN.EB.LOOKUP,F.EB.LOOKUP)

    FN.REDO.H.CUSTOMER.PROVISIONING = "F.REDO.H.CUSTOMER.PROVISIONING"
    F.REDO.H.CUSTOMER.PROVISIONING  = ""
    CALL OPF(FN.REDO.H.CUSTOMER.PROVISIONING,F.REDO.H.CUSTOMER.PROVISIONING)

    FN.AA.BILL.HST = 'F.AA.BILL.DETAILS.HIST'
    F.AA.BILL.HST = ''
    CALL OPF(FN.AA.BILL.HST,F.AA.BILL.HST)

    FN.DR.REG.OVR.LOAN.WORKFILE = 'F.DR.REG.OVR.LOAN.WORKFILE'
    F.DR.REG.OVR.LOAN.WORKFILE = ''
    CALL OPF(FN.DR.REG.OVR.LOAN.WORKFILE, F.DR.REG.OVR.LOAN.WORKFILE)

    FN.AA.ACTIVITY.BALANCES = 'F.AA.ACTIVITY.BALANCES'
    F.AA.ACTIVITY.BALANCES = ''
    CALL OPF(FN.AA.ACTIVITY.BALANCES,F.AA.ACTIVITY.BALANCES)

    FN.DEPT.ACCT.OFFICER ='F.DEPT.ACCT.OFFICER'
    F.DEPT.ACCT.OFFICER = ''
    CALL OPF(FN.DEPT.ACCT.OFFICER, F.DEPT.ACCT.OFFICER)

    FN.COMPANY = 'F.COMPANY';
    F.COMPANY = ''
    CALL OPF(FN.COMPANY, F.COMPANY)

    FN.CAMPAIGN.TYPES = 'F.REDO.CAMPAIGN.TYPES';
    F.CAMPAIGN.TYPES = '';
    CALL OPF (FN.CAMPAIGN.TYPES, F.CAMPAIGN.TYPES)

    FN.AA.PRODUCT = 'F.AA.PRODUCT'
    F.AA.PRODUCT = ''
    CALL OPF(FN.AA.PRODUCT,F.AA.PRODUCT)

    FN.AA.ARR.OVERDUE = 'F.AA.ARR.OVERDUE'
    F.AA.ARR.OVERDUE = ''
    CALL OPF(FN.AA.ARR.OVERDUE,F.AA.ARR.OVERDUE)


    RETURN

GET.LOC.DETAILS:
****************

    Y.APPL = 'AA.PRD.DES.CUSTOMER':FM:'CUSTOMER':FM:'AA.PRD.DES.OVERDUE'
    Y.FLDS = 'L.AA.CAMP.TY':FM:'L.CU.CIDENT':VM:'L.CU.RNC':VM:'L.CU.NOUNICO':VM:'L.CU.ACTANAC':VM:'L.CU.TEL.NO':VM:'L.CU.TEL.AREA':VM:'L.CU.SCO.COB':VM:'L.CU.TIPO.CL':FM:'L.LOAN.STATUS.1':VM:'L.LOAN.COND':VM:'L.STATUS.CHG.DT'
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FLDS,Y.POS)

    Y.CA.POS = Y.POS<1,1>
    Y.CID.POS = Y.POS<2,1>
    Y.RNC.POS = Y.POS<2,2>
    Y.NOUN.POS = Y.POS<2,3>
    Y.ACT.POS = Y.POS<2,4>
    Y.TEL.NO.PS = Y.POS<2,5>
    POS.L.CU.TEL.AREA = Y.POS<2,6>
    Y.CU.CALIF.RIESGO.POS = Y.POS<2,7>
        L.CU.TIPO.CL.POS = Y.POS<2,8>
    Y.LN.ST.POS = Y.POS<3,1>
    Y.LN.COND.POS = Y.POS<3,2>
    Y.L.STATUS.CHG.POS = Y.POS<3,3>

    LAST.WORK.DAY = R.DATES(EB.DAT.LAST.WORKING.DAY)
    YTODAY = R.DATES(EB.DAT.TODAY)
    YLST.TODAY = YTODAY
    CALL CDT('',YLST.TODAY,'-1C')
    IF LAST.WORK.DAY[5,2] NE YLST.TODAY[5,2] THEN
        COMI = LAST.WORK.DAY[1,6]:'01'
        CALL LAST.DAY.OF.THIS.MONTH
        YLST.TODAY = COMI
    END

    RETURN

END