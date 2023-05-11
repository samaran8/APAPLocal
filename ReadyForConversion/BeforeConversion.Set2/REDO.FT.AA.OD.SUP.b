*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.FT.AA.OD.SUP

* Client : APAP
* Description: The BEFORE.AUTH.RTN routine attached to verison 'FUNDS.TRANSFER,REDO.REV.TXN' for removing the unauth overdraft for Loan Accounts
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.ACCOUNT

    IF V$FUNCTION NE 'A' THEN
        RETURN
    END


    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
*****
    FN.ACCOUNT = 'F.ACCOUNT'; F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    RETURN

PROCESS:
********
    YCR.ACCT = ''; YDB.ACCT = ''
    YCR.ACCT = R.NEW(FT.CREDIT.ACCT.NO)
    YDB.ACCT = R.NEW(FT.DEBIT.ACCT.NO)

    IF ISDIGIT(YDB.ACCT) THEN
        RETURN
    END

    ERR.ACCOUNT = ''; R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,YCR.ACCT,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
    YARR.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
    IF YARR.ID [1,2] NE 'AA' THEN
        RETURN
    END

    YARRY.OVERRIDE = ''; YOVERRIDE = ''
    IF R.NEW(FT.OVERRIDE) NE '' THEN 
        YOVERRIDE = R.NEW(FT.OVERRIDE)
        FINDSTR 'ACCT.UNAUTH.OD' IN YOVERRIDE<1> SETTING YFM,YSM,YVM ELSE
            RETURN
        END
        YOD.CNT = 0; YOD.CNT = DCOUNT(YOVERRIDE,VM)
        FOR I = 1 TO YOD.CNT
            YOVERRIDE.CNT = YOVERRIDE<1,I>
            IF YOVERRIDE.CNT[1,14] EQ 'ACCT.UNAUTH.OD' THEN
                CONTINUE
            END
            YARRY.OVERRIDE<-1> = YOVERRIDE.CNT

        NEXT I

        IF YARRY.OVERRIDE THEN
            CHANGE FM TO VM IN YARRY.OVERRIDE
            R.NEW(FT.OVERRIDE) = YARRY.OVERRIDE
        END

    END
    RETURN

END
