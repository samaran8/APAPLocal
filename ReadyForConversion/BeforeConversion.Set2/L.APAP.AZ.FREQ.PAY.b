*-----------------------------------------------------------------------------
* <Rating>98</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.AZ.FREQ.PAY

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.DATES

    CALL GET.LOC.REF("AZ.ACCOUNT","PAYMENT.DATE",ACC.POS)

    PAY.DATE = R.NEW(AZ.LOCAL.REF)<1,ACC.POS>

    PAY.DATE.OLD = R.OLD(AZ.LOCAL.REF)<1,ACC.POS>

    IF  PAY.DATE NE PAY.DATE.OLD  THEN

        IF PAY.DATE NE "" THEN

            IF PAY.DATE <= 31 THEN


                Y.TODAY = R.DATES(EB.DAT.TODAY)

                FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
                F.AZ.ACCOUNT = ''
                CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

                VAR.VALUE.DATE = Y.TODAY

                ACT.FREQ = R.NEW(AZ.FREQUENCY)

                IF LEN(PAY.DATE)= 1 THEN

                    Y.MONTH.FREQ = "M010":PAY.DATE

                END

                IF LEN(PAY.DATE)= 2 THEN

                    Y.MONTH.FREQ = "M01":PAY.DATE

                END

                TEMP.COMI =  COMI

                COMI = Y.TODAY:Y.MONTH.FREQ

                CALL CFQ

                Y.FREQ = COMI

                COMI = TEM.COMI

*R.NEW(AZ.FREQUENCY) = Y.FREQ

                Y.TRANS.ID = ID.NEW

                Y.VER.NAME = "AZ.ACCOUNT,OFS"

                Y.APP.NAME = "AZ.ACCOUNT"

                Y.FUNC = "I"

                Y.PRO.VAL = "PROCESS"

                Y.GTS.CONTROL = ""

                Y.NO.OF.AUTH = ""

                FINAL.OFS = ""

                OPTIONS = ""

                Y.CAN.NUM = 0

                Y.CAN.MULT = ""

                R.ACC<AZ.FREQUENCY> = Y.FREQ

                CALL OFS.BUILD.RECORD(Y.APP.NAME,Y.FUNC,Y.PRO.VAL,Y.VER.NAME,Y.GTS.CONTROL,Y.NO.OF.AUTH,Y.TRANS.ID,R.ACC,FINAL.OFS)

*CALL OFS.GLOBUS.MANAGER("AZ.MIG.PERIOD", FINAL.OFS)

                CALL OFS.POST.MESSAGE(FINAL.OFS,Y.TRANS.ID,"DM.OFS.SRC.VAL",OPTIONS)

            END

        END

    END

END
