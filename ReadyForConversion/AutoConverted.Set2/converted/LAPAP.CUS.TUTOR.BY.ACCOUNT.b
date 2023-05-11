*========================================================================
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE LAPAP.CUS.TUTOR.BY.ACCOUNT(ACCOUNT.NO, R.CUSTOMER, CUS.ID)
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.CUS.TUTOR.BY.ACCOUNT
* Date           : 2018-06-14
* Item ID        : CN008702
*========================================================================
* Brief description :
* -------------------
* This routine returns information from the tutor of a minor client account
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-06-14     Anthony Martinez  Initial Development
*========================================================================

    GOSUB PROCESS

PROCESS:

*-------
    FN.ACCOUNT = 'F.ACCOUNT' ; F.ACCOUNT = ''; R.ACCOUNT = ''; ACCOUNT.ERR = ''
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    CALL F.READ(FN.ACCOUNT,ACCOUNT.NO,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)

    FN.CUSTOMER = 'F.CUSTOMER' ; F.CUSTOMER = ''; CUSTOMER.ERR = ''
    CALL OPF(FN.CUSTOMER, F.CUSTOMER)

    Y.JOINT.HOLDER  = R.ACCOUNT<AC.JOINT.HOLDER>
    Y.RELATION.CODE = R.ACCOUNT<AC.RELATION.CODE>
    CUS.ID    = R.ACCOUNT<AC.CUSTOMER>

    Y.CANT.RECS = DCOUNT(Y.JOINT.HOLDER, @VM)

    FOR A = 1 TO Y.CANT.RECS STEP 1
        IF Y.RELATION.CODE<1,A> EQ "510" THEN
            CALL F.READ(FN.CUSTOMER,Y.JOINT.HOLDER<1,A>,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
            CUS.ID = Y.JOINT.HOLDER<1,A>
        END
    NEXT A

    IF NOT(R.CUSTOMER) THEN
        CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    END

RETURN
*-------
