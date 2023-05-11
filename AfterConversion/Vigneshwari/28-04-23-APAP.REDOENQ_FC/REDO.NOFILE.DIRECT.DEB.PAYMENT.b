$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.DIRECT.DEB.PAYMENT(DIR.DEB.DET)


*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By :
* Program Name : REDO.NOFILE.SHOW.STO.TRANSFER
*-----------------------------------------------------------------------------
* Description : This subroutine is attached as a BUILD routine in the Enquiry AI.REDO.LOAN.ACCT.TO
* In Parameter : ENQ.DATA
* Out Parameter : None
*
**DATE           ODR                   DEVELOPER               VERSION
*
*
*
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 13-APRIL-2023      Conversion Tool       R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN , VM to @VM and Commented I_F.CUSTOMER.ACCOUNT
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER
    $INSERT I_F.RELATION.CUSTOMER
*   $INSERT I_F.CUSTOMER.ACCOUNT	;*R22 Auto Conversion  - Commented I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ACTIVITY.HISTORY
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.REDO.APAP.STO.DUPLICATE

    GOSUB INITIALISE
    GOSUB CUSTOMER.ACCT.DETAILS

RETURN
*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------


    FN.RELATION.CUSTOMER = 'F.RELATION.CUSTOMER'
    F.RELATION.CUSTOMER  = ''
    CALL OPF(FN.RELATION.CUSTOMER,F.RELATION.CUSTOMER)

    FN.CUS.ACC = 'F.CUSTOMER.ACCOUNT'
    F.CUS.ACC = ''
    CALL OPF(FN.CUS.ACC,F.CUS.ACC)

    FN.REDO.APAP.STO.DUPLICATE = 'F.REDO.APAP.STO.DUPLICATE'
    F.REDO.APAP.STO.DUPLICATE  = ''
    CALL OPF(FN.REDO.APAP.STO.DUPLICATE,F.REDO.APAP.STO.DUPLICATE)

    FN.ARC.DIR.DEB.LIST = 'F.ARC.DIR.DEB.LIST'
    F.ARC.DIR.DEB.LIST  = ''
    CALL OPF(FN.ARC.DIR.DEB.LIST,F.ARC.DIR.DEB.LIST)

    FN.ARCIB.PARAM = 'F.AI.REDO.ARCIB.PARAMETER'
    F.ARCIB.PARAM = ''
    CALL OPF(FN.ARCIB.PARAM,F.ARCIB.PARAM)

    FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY'
    F.AAA  = ''
    CALL OPF(FN.AAA,F.AAA)

    FN.ACTIVITY.HISTORY = 'F.AA.ACTIVITY.HISTORY'
    F.ACTIVITY.HISTORY  = ''
    CALL OPF(FN.ACTIVITY.HISTORY,F.ACTIVITY.HISTORY)

    FN.ACC = 'F.ACCOUNT'
    F.ACC  = ''
    CALL OPF(FN.ACC,F.ACC)


    L.APPLN = 'AA.PRD.DES.PAYMENT.SCHEDULE'
    L.FIELDS = 'L.AA.PAY.METHD':@VM:'L.AA.DEBT.AC':@VM:'L.AA.PYMT.REF'
    POS.PS = ''
    CALL MULTI.GET.LOC.REF(L.APPLN,L.FIELDS,POS.PS)
    Y.POS.METHD = POS.PS<1,1>
    Y.POS.DEB.AC = POS.PS<1,2>
    Y.POS.PYMT.REF = POS.PS<1,3>

    CUSTOMER.ID = System.getVariable("EXT.SMS.CUSTOMERS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN	;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        CUSTOMER.ID = ""
    END
    CALL F.READ(FN.ARC.DIR.DEB.LIST,CUSTOMER.ID,R.ARC.DIR.DEB.LIST,F.ARC.DIR.DEB.LIST,ARC.DIR.DEB.LIST.ERR)


RETURN

*----------------------------------------------------------------------------
CUSTOMER.ACCT.DETAILS:
*-----------------------------------------------------------------------------

    LOOP
        REMOVE STO.DUP.ID FROM R.ARC.DIR.DEB.LIST SETTING STO.DUP.POS
    WHILE STO.DUP.ID:STO.DUP.POS
        CALL F.READ(FN.REDO.APAP.STO.DUPLICATE,STO.DUP.ID,R.REDO.APAP.STO.DUPLICATE,F.REDO.APAP.STO.DUPLICATE,STO.DUP.ERR)
        IF R.REDO.APAP.STO.DUPLICATE THEN
            Y.DATE.TIME = R.REDO.APAP.STO.DUPLICATE<REDO.SO.DATE.TIME>
            Y.PAY.DEB.ACC  = R.REDO.APAP.STO.DUPLICATE<REDO.SO.ORIGIN.ACCT.NO>
            Y.LOAN.ACCT.NO = R.REDO.APAP.STO.DUPLICATE<REDO.SO.LOAN.ACCT.NO>
            Y.PROCESS.DATE = '20':Y.DATE.TIME[1,6]
            Y.AMOUNT = R.REDO.APAP.STO.DUPLICATE<REDO.SO.BILL.AMOUNT>
            Y.START.DATE = R.REDO.APAP.STO.DUPLICATE<REDO.SO.STO.START.DATE>
            DIR.DEB.DET<-1> = Y.PAY.DEB.ACC:"@":Y.LOAN.ACCT.NO:"@":Y.AMOUNT:"@":Y.PROCESS.DATE:"@":Y.START.DATE:"@":STO.DUP.ID
        END
    REPEAT

RETURN

END

*---------*END OF SUBROUTINE*-------------------------------
