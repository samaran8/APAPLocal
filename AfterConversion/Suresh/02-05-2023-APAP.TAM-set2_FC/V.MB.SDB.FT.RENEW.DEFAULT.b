* @ValidationCode : MjotMTI1NjM2ODg0MjpDcDEyNTI6MTY4MTg4OTk3MzY3MTozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 19 Apr 2023 13:09:33
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
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*19/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION             NOCHANGE
*19/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
* Modification History
* NSCU20091007 - A.VINOTH KUMAR - Fix for the issue HD0938664
SUBROUTINE V.MB.SDB.FT.RENEW.DEFAULT

    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.MB.SDB.PARAM
    $INSERT I_F.MB.SDB.STATUS
 
    FN.MB.SDB.PARAM = 'F.MB.SDB.PARAM'
    F.MB.SDB.PARAM = ''
    CALL OPF(FN.MB.SDB.PARAM,F.MB.SDB.PARAM)

    FN.MB.SDB.STATUS = 'F.MB.SDB.STATUS'
    F.MB.SDB.STATUS = ''
    CALL OPF(FN.MB.SDB.STATUS,F.MB.SDB.STATUS)

    MB.SDB.PARAM.ID = ID.COMPANY; R.MB.SDB.PARAM = ''; PARAM.YERR = ''
    CALL F.READ(FN.MB.SDB.PARAM,MB.SDB.PARAM.ID,R.MB.SDB.PARAM,F.MB.SDB.PARAM,PARAM.YERR)
    RENT.ACCT = R.MB.SDB.PARAM<SDB.PAR.ADV.RENT.ACCT>
    FT.TXN.RENT = R.MB.SDB.PARAM<SDB.PAR.FT.TXN.TYPE.RENT>
    SDBVAT = R.MB.SDB.PARAM<SDB.PAR.FT.COMM.1>
* NSCU20091007  -S
    RENT.PL = 'PL':R.MB.SDB.PARAM<SDB.PAR.RENT.PL>
    IF NOT(RENT.ACCT) THEN
        RENT.ACCT = RENT.PL
    END
* NSCU20091007  -E
    R.NEW(FT.TRANSACTION.TYPE) = FT.TXN.RENT
    R.NEW(FT.CREDIT.ACCT.NO) = RENT.ACCT
    R.NEW(FT.COMMISSION.TYPE) = SDBVAT

    IF R.NEW(FT.DEBIT.THEIR.REF) THEN
        SDB.NO = R.NEW(FT.DEBIT.THEIR.REF)
        MB.SDB.STATUS.ID = ID.COMPANY:'.':SDB.NO; R.MB.SDB.STATUS = '' ; STATUS.YERR = ''
        CALL F.READ(FN.MB.SDB.STATUS,MB.SDB.STATUS.ID,R.MB.SDB.STATUS,F.MB.SDB.STATUS,STATUS.YERR)
        IF NOT(STATUS.YERR) THEN
            RENEW.FREQUENCY = R.MB.SDB.STATUS<SDB.STA.RENEW.FREQUENCY>
            NBR.AMT = 0; SV.COMI = COMI
            LOOP UNTIL RENEW.FREQUENCY[1,8] GT TODAY
                COMI = RENEW.FREQUENCY; CALL CFQ
                RENEW.FREQUENCY = COMI
                NBR.AMT += 1
            REPEAT
            COMI = SV.COMI

            IF NBR.AMT GE 1 THEN
* R.NEW(FT.CREDIT.AMOUNT) = R.MB.SDB.STATUS<SDB.STA.RENT.AMT> * (NBR.AMT -1) + R.MB.SDB.STATUS<SDB.STA.PERIODIC.RENT>
                R.NEW(FT.CREDIT.AMOUNT) = (NBR.AMT) * R.MB.SDB.STATUS<SDB.STA.PERIODIC.RENT>
            END ELSE
                R.NEW(FT.CREDIT.AMOUNT) = R.MB.SDB.STATUS<SDB.STA.PERIODIC.RENT>
            END

            R.NEW(FT.CREDIT.THEIR.REF) = SDB.NO
            R.NEW(FT.DEBIT.ACCT.NO) = R.MB.SDB.STATUS<SDB.STA.CUSTOMER.ACCT>

            T(FT.DEBIT.ACCT.NO)<3> = 'NOINPUT'
            T(FT.DEBIT.THEIR.REF)<3> = 'NONINPUT'
            CALL REBUILD.SCREEN
        END

        RETURN

    END
