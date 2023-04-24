$PACKAGE APAP.LAPAP
SUBROUTINE REDO.UPD.ATM.REJ
**************************************************************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S DHAMU
* ORIGINAL PROGRAM NAME: REDO.UPD.ATM.REJ
* ODR NO : ODR-2010-08-0469
*-----------------------------------------------------------------------------------------------------
*DESCRIPTION: This routine is to update the details of transaction rejected
*******************************************************************************************************
*linked with :
*In parameter:
*Out parameter:
*****************************************************************************************************
*Modification History
* Date Who Reference Description
* 24 Aug 2011 Balagurunathan ODR-2010-08-0469 addressed the issue PACS00084788 to handle rejected message
* 01 Sep 2011 Balagurunathan ODR-2010-08-0469 addressed the issue PACS00121355 to handle rejected advice message
* 01 Oct 2011 Balagurunathan ODR-2010-08-0469 added missing fields and compiled
* 12 May 2022 Juan Garcia    CTO-06 Crear proceso de cobro de cargo por insuficiencia de fondos en tarjeta de debito.
*DATE           WHO                 REFERENCE               DESCRIPTION
*21-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     INSERT FILE MODIFIED
*21-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   CALL ROUTINE ADDED
*-------------------------------------------------------------------------

    $INSERT I_COMMON ;*AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_AT.ISO.COMMON
    $INSERT I_ATM.BAL.ENQ.COMMON
    $INSERT I_F.REDO.TXN.REJECT
    $INSERT I_F.REDO.IST.RESP.CODE
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.REDO.CARD.BIN
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AC.CHARGE.REQUEST ;*MANUAL R22 CODE CONVERSION END

    Y.COMPANY           = "DO0010001";
    Y.PROCESS.CODE.COM = '010000':@FM:'011000':@FM:'012000':@FM:'013000'

    IF LOC.MSG EQ 'APPROVED ADVICE' THEN

        RETURN
    END

    IF (AT$INCOMING.ISO.REQ(1) EQ '0220' AND (LOC.MSG EQ '' OR LOC.MSG EQ '0' OR LOC.MSG EQ 'REJECTED ADVICE' OR LOC.MSG EQ 'Card status changed to PIN EXCEEDED' ) AND AT$INCOMING.ISO.REQ(39) NE '00') THEN

        AT$AT.ISO.RESP.CODE=AT$INCOMING.ISO.REQ(39)
        LOC.MSG='REJECTED ADVICE'
        UNIQUE.ID=TRIM(AT$INCOMING.ISO.REQ(38))
    END

* UNIQUE.ID=''
* RETURN
* END


    IF UNIQUE.ID EQ '' OR UNIQUE.ID EQ 0 THEN
        IF NOT(TRIM(AT$INCOMING.ISO.REQ(38))) THEN
            UNIQUE.ID=AT$INCOMING.ISO.REQ(38)
        END

        CARD.NUMM=AT$INCOMING.ISO.REQ(2)
*CALL REDO.TXN.AUTH.CODE (CARD.NUMM,RET.VAL)
*R22 MANUAL CONVERSION
        CALL APAP.TAM.REDO.TXN.AUTH.CODE (CARD.NUMM,RET.VAL)

    END
    IF AT$AT.ISO.RESP.CODE EQ '85' THEN

        IF Y.UNIQUE.ID EQ '' OR Y.UNIQUE.ID EQ '0' THEN

            Y.UNIQUE.ID=UNIQUE.ID

        END
*CALL V.FT.UPD.ENQ.ATM.KEY.ID
*R22 MANUAL CONVERSION
        CALL APAP.ATM.V.FT.UPD.ENQ.ATM.KEY.ID
        RETURN
    END

    GOSUB INIT
    GOSUB PROCESS
    UNIQUE.ID=''
RETURN

****
INIT:
*****

    FN.REDO.TXN.REJECT = 'F.REDO.TXN.REJECT'
    F.REDO.TXN.REJECT = ''
    CALL OPF(FN.REDO.TXN.REJECT,F.REDO.TXN.REJECT)

    FN.REDO.IST.RESP.CODE='F.REDO.IST.RESP.CODE'
    F.REDO.IST.RESP.CODE=''
    CALL OPF(FN.REDO.IST.RESP.CODE,F.REDO.IST.RESP.CODE)

    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.ATM.WAIVE.CHARGE = 'F.REDO.ATM.WAIVE.CHARGE'
    F.REDO.ATM.WAIVE.CHARGE = ''
    CALL OPF(FN.REDO.ATM.WAIVE.CHARGE,F.REDO.ATM.WAIVE.CHARGE)

    FN.ATM.BRANCH = 'F.ATM.BRANCH'
    F.ATM.BRANCH = ''
    CALL OPF(FN.ATM.BRANCH,F.ATM.BRANCH)

    FN.LATAM.CARD.ORDER='F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER=''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)

    FN.AC.CHARGE.REQUEST = 'F.AC.CHARGE.REQUEST'
    F.AC.CHARGE.REQUEST = ''
    CALL OPF(FN.AC.CHARGE.REQUEST,F.AC.CHARGE.REQUEST)

    FN.ST.L.APAP.CHARGE.INS.FUN = 'F.ST.L.APAP.CHARGE.INS.FUN'
    F.ST.L.APAP.CHARGE.INS.FUN = ''
    CALL OPF(FN.ST.L.APAP.CHARGE.INS.FUN,F.ST.L.APAP.CHARGE.INS.FUN)

    FN.REDO.CARD.BIN='F.REDO.CARD.BIN'
    F.REDO.CARD.BIN=''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

    CALL GET.LOC.REF('ACCOUNT','L.AC.AV.BAL',BAL.POS)

RETURN
********
PROCESS:
********
    R.REDO.TXN.REJECT=''

    MSG.TYPE=AT$INCOMING.ISO.REQ(1)
    RET.MSG.TYPE=MSG.TYPE+10
    RET.MSG.TYPE=FMT(RET.MSG.TYPE,"R%4")
    R.REDO.TXN.REJECT<TXN.REJ.MTI.RESP>=RET.MSG.TYPE

    R.REDO.TXN.REJECT<TXN.REJ.MESSAGE.TYPE>=AT$INCOMING.ISO.REQ(1)
    R.REDO.TXN.REJECT<TXN.REJ.CARD.NUMBER>=AT$INCOMING.ISO.REQ(2)
    R.REDO.TXN.REJECT<TXN.REJ.PROCESS.CODE>=AT$INCOMING.ISO.REQ(3)
    Y.PROCESS.CODE = AT$INCOMING.ISO.REQ(3)
    Y.TXN.AMT=AT$INCOMING.ISO.REQ(4)* 1
    Y.TXN.AMT=Y.TXN.AMT/100
    Y.TXN.AMT1=FIELD(Y.TXN.AMT,'.',2)
    Y.TXN.AMT2=FIELD(Y.TXN.AMT,'.',1)
    Y.TXN.AMT1=FMT(Y.TXN.AMT1,'L%2')
    Y.TXN.AMT=Y.TXN.AMT2:'.':Y.TXN.AMT1
    R.REDO.TXN.REJECT<TXN.REJ.TRANSACTION.AMT>=Y.TXN.AMT

    Y.BILL.AMT=AT$INCOMING.ISO.REQ(6) *1

    Y.BILL.AMT=Y.BILL.AMT/100
    Y.BILL.AMT1=FIELD(Y.BILL.AMT,'.',2)
    Y.BILL.AMT2=FIELD(Y.BILL.AMT,'.',1)
    Y.BILL.AMT1=FMT(Y.BILL.AMT1,'L%2')
    Y.BILL.AMT=Y.BILL.AMT2:'.':Y.BILL.AMT1

    R.REDO.TXN.REJECT<TXN.REJ.BILLING.AMT>=Y.BILL.AMT
    R.REDO.TXN.REJECT<TXN.REJ.TRACE>=AT$INCOMING.ISO.REQ(11)
    R.REDO.TXN.REJECT<TXN.REJ.RESPONSE.CODE>=AT$AT.ISO.RESP.CODE
    BIN.NO=AT$INCOMING.ISO.REQ(2)[1,6]

    CALL CACHE.READ(FN.REDO.CARD.BIN,BIN.NO,R.REDO.CARD.BIN,ERR.BIN)
    CRD.TYP=R.REDO.CARD.BIN<REDO.CARD.BIN.CARD.TYPE>

* changing code to accomadate multivalue of CARD.TYPE in REDO.CARD.BIN for issue PACS00033279
    CRD.TYP.VAL=CRD.TYP

    LOOP

        REMOVE Y.CRD.TYP FROM CRD.TYP.VAL SETTING POS.CRD.VAL

    WHILE Y.CRD.TYP:POS.CRD.VAL

        CRD.NUM=Y.CRD.TYP:".":AT$INCOMING.ISO.REQ(2)

        CALL F.READ (FN.LATAM.CARD.ORDER,CRD.NUM,R.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER,CRD.ERR)
        IF R.LATAM.CARD.ORDER THEN
            ACCT.NUM=R.LATAM.CARD.ORDER<CARD.IS.ACCOUNT>
            CALL F.READ(FN.ACCOUNT,ACCT.NUM,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)

            R.REDO.TXN.REJECT< TXN.REJ.ACCOUNT.NUMBER>=ACCT.NUM
            R.REDO.TXN.REJECT< TXN.REJ.ACCOUNT.NUMBER.OLD>=R.ACCOUNT<AC.ALT.ACCT.ID,1>

            R.REDO.TXN.REJECT<TXN.REJ.AVAILABLE.BALANCE >= R.ACCOUNT<AC.LOCAL.REF><1,BAL.POS>

            BREAK
        END
    REPEAT
*changing end code to accomadate multivalue of CARD.TYPE in REDO.CARD.BIN for issue PACS00033279

    CALL CACHE.READ(FN.REDO.IST.RESP.CODE,'SYSTEM',R.REDO.IST.RESP.CODE,ERR)

    RESP.CODE=AT$AT.ISO.RESP.CODE
    LOCATE RESP.CODE IN R.REDO.IST.RESP.CODE<REDO.IST.RESP.CODE.RESP.CODE,1> SETTING POS.CODE THEN
        RES.DESC=R.REDO.IST.RESP.CODE<REDO.IST.RESP.CODE.DESCRIPTION,POS.CODE>
    END
    R.REDO.TXN.REJECT<TXN.REJ.IST.DESC>=RES.DESC
    R.REDO.TXN.REJECT<TXN.REJ.T24.ERR.MSG>=LOC.MSG

    R.REDO.TXN.REJECT<TXN.REJ.AUTH.CODE>=UNIQUE.ID

    R.REDO.TXN.REJECT<TXN.REJ.REFERENCE.NUMBER>=AT$INCOMING.ISO.REQ(37)
    R.REDO.TXN.REJECT<TXN.REJ.LOCAL.DATE>=AT$INCOMING.ISO.REQ(13)
    R.REDO.TXN.REJECT<TXN.REJ.LOCAL.TIME>=AT$INCOMING.ISO.REQ(12)

    SYS.DATE=OCONV(DATE(),'D\')
    SYS.DATE=SYS.DATE[7,4]:SYS.DATE[1,2]:SYS.DATE[4,2]

    R.REDO.TXN.REJECT<TXN.REJ.CAPTURE.DATE>=SYS.DATE


    R.REDO.TXN.REJECT<TXN.REJ.TERM.ID>=AT$INCOMING.ISO.REQ(41)
    R.REDO.TXN.REJECT<TXN.REJ.MERCHANT.ID>=AT$INCOMING.ISO.REQ(42)
    R.REDO.TXN.REJECT<TXN.REJ.ACCEPTOR.NAME>=AT$INCOMING.ISO.REQ(43)
    R.REDO.TXN.REJECT<TXN.REJ.CURRENCY.CODE>=AT$INCOMING.ISO.REQ(49)

    Y.TERMINAL.ID = AT$INCOMING.ISO.REQ(41)

    ISO.CONV.RATE=AT$INCOMING.ISO.REQ(10)
    DEC.POS=ISO.CONV.RATE[1,1]
    CONV.AMT=ISO.CONV.RATE[2,7]
    CONV.AMT= STR("0",(9-LEN(CONV.AMT))):CONV.AMT
    DEC.PRE=9-DEC.POS
    DEC.VAL=9-DEC.PRE
    IF DEC.VAL EQ 0 THEN
        Y.DEC.VAL=''
    END ELSE
        Y.DEC.VAL= ".":CONV.AMT[DEC.PRE+1,DEC.VAL]
    END

    IF DEC.PRE EQ 0 THEN
        Y.CONV.RATE ="0.":CONV.AMT
    END ELSE

        Y.CONV.RATE=CONV.AMT[1,DEC.PRE] * 1:Y.DEC.VAL

    END
    R.REDO.TXN.REJECT<TXN.REJ.CONVERSION.RATE>=Y.CONV.RATE
*AT$INCOMING.ISO.REQ(10)
    R.REDO.TXN.REJECT<TXN.REJ.ISSUER>=AT$INCOMING.ISO.REQ(2)[1,6]
    R.REDO.TXN.REJECT<TXN.REJ.TRANS.DATETIME>=AT$INCOMING.ISO.REQ(7)

    R.REDO.TXN.REJECT<TXN.REJ.CARD.EXPIRY>=AT$INCOMING.ISO.REQ(14)
    R.REDO.TXN.REJECT<TXN.REJ.ACQ.COUNTRY.CDE>=AT$INCOMING.ISO.REQ(19)
    R.REDO.TXN.REJECT<TXN.REJ.ACQ.INST.CDE>=AT$INCOMING.ISO.REQ(32)
    R.REDO.TXN.REJECT<TXN.REJ.FWD.INST.CDE>=AT$INCOMING.ISO.REQ(33)
    R.REDO.TXN.REJECT<TXN.REJ.ADD.AMT>=AT$INCOMING.ISO.REQ(54)
    R.REDO.TXN.REJECT<TXN.REJ.MRCHT.CATEG>=AT$INCOMING.ISO.REQ(18)
    R.REDO.TXN.REJECT<TXN.REJ.POS.ENTRY.MOD>=AT$INCOMING.ISO.REQ(22)[1,2]
    R.REDO.TXN.REJECT<TXN.REJ.POS.COND>=AT$INCOMING.ISO.REQ(25)
    R.REDO.TXN.REJECT<TXN.REJ.T24.DATE>=TODAY

    TXN.ID = AT$INCOMING.ISO.REQ(2):'.':UNIQUE.ID

    WRITE R.REDO.TXN.REJECT TO F.REDO.TXN.REJECT,TXN.ID ON ERROR

    END
*-------------Logica para generar el cargo por valance insuficiente------------

    FINDSTR Y.PROCESS.CODE IN Y.PROCESS.CODE.COM SETTING V.FLD, V.VAL THEN

        IF RESP.CODE EQ '51' THEN

*-------------Verificar si el cargo esta encendido o apagado------------
            Y.ON.OFF.ID    = "SYSTEM"

            R.APAP.CHARGE.ON.OFF = ''; ON.OFF.ERR = '';
            CALL F.READ (FN.ST.L.APAP.CHARGE.INS.FUN,Y.ON.OFF.ID,R.APAP.CHARGE.ON.OFF,F.ST.L.APAP.CHARGE.INS.FUN,ON.OFF.ERR)
            Y.STATUS = R.APAP.CHARGE.ON.OFF<1>

            IF Y.STATUS EQ 'ENCENDIDO' THEN

*-------------Si el cajero es APAP para omitir el carho------------
                R.ATM.BRANCH = ''; ATM.ERR = '';
                CALL F.READ (FN.ATM.BRANCH,Y.TERMINAL.ID,R.ATM.BRANCH,F.ATM.BRANCH,ATM.ERR)

                IF NOT (R.ATM.BRANCH) THEN

*-------------Si el cajero es UNARED para omitir el carho------------
                    Y.ATM.CODE     = Y.TERMINAL.ID[1,4]
                    NO.OF.REC = ''; SEL.ERR = ''; Y.COUNT.ATM = ''; ATM.POS = '';
                    SEL.CMD = "SELECT ":FN.REDO.ATM.WAIVE.CHARGE:" WITH @ID LIKE ":Y.ATM.CODE:"...";
                    CALL EB.READLIST(SEL.CMD, SEL.LIST, "", NO.OF.REC, SEL.ERR);
                    IF NOT (SEL.LIST) THEN

                        Y.VER.NAME = 'AC.CHARGE.REQUEST,INS.FUN'
                        Y.APP.NAME = 'AC.CHARGE.REQUEST'
                        Y.FUNC = 'I'
                        Y.PRO.VAL = "PROCESS"
                        Y.GTS.CONTROL = ""
                        Y.NO.OF.AUTH = ""
                        FINAL.OFS = ""
                        Y.TRANS.ID = ""

                        R.CHARGE<CHG.CHARGE.CODE> = "CAINSFATMNP"
                        R.CHARGE<CHG.DEBIT.ACCOUNT> = ACCT.NUM

                        CALL OFS.BUILD.RECORD(Y.APP.NAME,Y.FUNC,Y.PRO.VAL,Y.VER.NAME,Y.GTS.CONTROL,Y.NO.OF.AUTH,Y.TRANS.ID,R.CHARGE,FINAL.OFS)

                        OFS.MSG.ID = ''; OFS.SOURCE.ID = "DM.OFS.SRC.VAL"; OPTIONS = ''
                        CALL OFS.POST.MESSAGE(FINAL.OFS,OFS.MSG.ID,OFS.SOURCE.ID,OPTIONS)
                        CALL JOURNAL.UPDATE(Y.CARD.REQUEST.ID)
                    END
                END
            END
        END
    END

RETURN
*********************************************
END

*----------------------End of Program------------------------------------------
