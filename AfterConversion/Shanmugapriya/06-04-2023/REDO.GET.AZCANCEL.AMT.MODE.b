* @ValidationCode : MjotOTI0MzAwNjQ0OkNwMTI1MjoxNjgwNzE1OTg2MDUxOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 23:03:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.GET.AZCANCEL.AMT.MODE(REC.AZ.ACCOUNT,Y.AZ.ID,Y.MONTO.CANCEL,Y.CANCEL.MODE)
    
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 06.04.2023       Conversion Tool       R22            Auto Conversion     - FM TO @FM, VM TO @VM
* 06.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------------------
    
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.AZ.ACCOUNT


    GOSUB INITS
    GOSUB PROCESS
RETURN
INITS:

    Y.MONTO.CANCEL = ''
    Y.CANCEL.MODE = ''

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT, F.AZ.ACCOUNT)

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.STMT.ENTRY.DETAIL = 'F.STMT.ENTRY.DETAIL'
    F.STMT.ENTRY.DETAIL  = ''
    CALL OPF(FN.STMT.ENTRY.DETAIL,F.STMT.ENTRY.DETAIL)

    FN.ACCOUNT.CLOSURE = 'F.ACCOUNT.CLOSURE'
    F.ACCOUNT.CLOSURE = ''
    CALL OPF(FN.ACCOUNT.CLOSURE,F.ACCOUNT.CLOSURE)

    Y.APP.LIST = 'CUSTOMER':@FM:'AZ.ACCOUNT'
    Y.FIELD.LIST = 'L.CU.TIPO.CL':@FM:'L.AZ.METHOD.PAY':@VM:'L.AZ.AMOUNT':@VM:'L.AC.CAN.REASON':@VM:'L.TYPE.INT.PAY'
    Y.LOC.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APP.LIST, Y.FIELD.LIST, Y.LOC.POS)
    Y.CL.TYPE.POS = Y.LOC.POS<1,1>
    Y.AZ.METHOD.POS = Y.LOC.POS<2,1>
    Y.AZ.AMT.POS = Y.LOC.POS<2,2>
    Y.CAN.REASON.POS = Y.LOC.POS<2,3>
    Y.TYPE.INTPAY.POS = Y.LOC.POS<2,4>

RETURN


PROCESS:

    R.AZ.ACCOUNT = REC.AZ.ACCOUNT
    Y.TYPE.INT.PAY = R.AZ.ACCOUNT<AZ.LOCAL.REF, Y.TYPE.INTPAY.POS>

    IF Y.TYPE.INT.PAY NE 'Reinvested' THEN

        Y.OVERRIDE = R.AZ.ACCOUNT<AZ.OVERRIDE>
        FINDSTR "REDO.AZ.TOTAL.DUE" IN Y.OVERRIDE SETTING FIELD.NO, VALUE.NO THEN

            Y.OVERRIDE.STR = Y.OVERRIDE<FIELD.NO,VALUE.NO>
            Y.OVERRIDE.AMT = FIELD(Y.OVERRIDE.STR,'{',2)
            Y.OVERRIDE.AMT = TRIM(Y.OVERRIDE.AMT,",","A")
        END
        Y.MONTO.CANCEL = Y.OVERRIDE.AMT
        Y.CANCEL.MODE = R.AZ.ACCOUNT<AZ.NOMINATED.ACCOUNT>
    END
    ELSE

        Y.INT.LIQ.ACCT = R.AZ.ACCOUNT<AZ.INTEREST.LIQU.ACCT>
        FROM.DATE = R.AZ.ACCOUNT<AZ.VALUE.DATE>
        END.DATE  = TODAY
        CALL EB.ACCT.ENTRY.LIST(Y.INT.LIQ.ACCT,FROM.DATE,END.DATE,YID.LIST,OPENING.BAL,ER)

        LOOP
            REMOVE Y.STMT.ENTRY FROM YID.LIST SETTING STMT.POS
        WHILE Y.STMT.ENTRY:STMT.POS

            CALL F.READ(FN.STMT.ENTRY,Y.STMT.ENTRY,R.STMT.ENTRY,F.STMT.ENTRY,STMT.ERR)
            IF R.STMT.ENTRY EQ '' THEN
                CALL F.READ(FN.STMT.ENTRY.DETAIL,Y.STMT.ENTRY,R.STMT.ENTRY,F.STMT.ENTRY.DETAIL,STMT.ERR)
            END
            STMT.TXN.CODE = R.STMT.ENTRY<AC.STE.TRANSACTION.CODE>

            IF STMT.TXN.CODE EQ '126' THEN
                Y.MONTO.CANCEL = R.STMT.ENTRY<AC.STE.AMOUNT.LCY>
            END

        REPEAT

        Y.AZ.NOM.ACCT = R.AZ.ACCOUNT<AZ.NOMINATED.ACCOUNT>

        CALL F.READ(FN.ACCOUNT.CLOSURE,Y.AZ.NOM.ACCT,REC.ACCOUNT.CLOSURE,F.ACCOUNT.CLOSURE,ACC.CLOSE.ERR)
        IF REC.ACCOUNT.CLOSURE THEN
            Y.SETT.ACC = REC.ACCOUNT.CLOSURE<AC.ACL.SETTLEMENT.ACCT>
            IF Y.SETT.ACC THEN
                Y.CANCEL.MODE = Y.SETT.ACC
            END
            ELSE
                Y.CANCEL.MODE = 'EFECTIVO'
            END

        END
    END
RETURN

END
