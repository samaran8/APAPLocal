* @ValidationCode : Mjo3ODE3MTA2NzY6Q3AxMjUyOjE2ODI2NzM1ODQ4OTE6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 28 Apr 2023 14:49:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
*-----------------------------------------------------------------------------
* <Rating>-163</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.E.CASH.TRANS.NAU(Y.FINAL.DATA)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Pradeep M
* Program Name : REDO.E.CASH.TRANS.NAU
*-----------------------------------------------------------------------------
* Description :Built routine to assign value to set variable
* Linked with :
* In Parameter :
* Out Parameter :
*
**DATE          DEVELOPER           ODR              VERSION
* 10-11-2011    Pradeep M           ODR2011080055
* 05/03/2013    Vignesh Kumaar R    PACS00245176     Displaying the RNAO records in the enq list
* 16/04/2013    Vignesh Kumaar R    PACS00265099     Displaying the INAU records in the enq list for TT
* 19/04/2013    Nava V.             PACS00269527     Adding selection criteria fields
* 27/05/2013    Vignesh Kumaar R    PACS00245167     AUTH.DATE - current variable issue
* 23/11/2017    Gopala Krishnan R   PACS00619093     DATE.TIME field in within the last 12 months
* 28-APRIL-2023      Conversion Tool       R22 Auto Conversion - VM to @VM and FM to @FM
* 28-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER

    $INSERT I_F.USER
    $INSERT I_F.T24.FUND.SERVICES

    $INSERT I_F.REDO.APAP.CLEAR.PARAM
    $INSERT I_F.REDO.AUT.INP.VERSION.NAME         ;* Added for PACS00245167

    GOSUB OPEN.PROCESS
    GOSUB LOC.CRT.FLDS
    GOSUB PROCESS
RETURN

OPEN.PROCESS:
*-----------

    FN.FUNDS.TRANSFER$NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANSFER$NAU = ''
    CALL OPF(FN.FUNDS.TRANSFER$NAU,F.FUNDS.TRANSFER$NAU)

    FN.TELLER$NAU = 'F.TELLER$NAU'
    F.TELLER$NAU = ''
    CALL OPF(FN.TELLER$NAU,F.TELLER$NAU)

    FN.T24.FUND.SERVICES$NAU = 'F.T24.FUND.SERVICES$NAU'
    F.T24.FUND.SERVICES$NAU = ''
    CALL OPF(FN.T24.FUND.SERVICES$NAU,F.T24.FUND.SERVICES$NAU)

    FN.USER = 'F.USER'
    F.USER = ''
    CALL OPF(FN.USER,F.USER)

* Fix for PACS00245167 [AUTH.DATE - current variable issue]

    FN.REDO.AUT.INP.VERSION.NAME = 'F.REDO.AUT.INP.VERSION.NAME'
    F.REDO.AUT.INP.VERSION.NAME = ''
    CALL OPF(FN.REDO.AUT.INP.VERSION.NAME,F.REDO.AUT.INP.VERSION.NAME)

    FN.REDO.APAP.CLEAR.PARAM = 'F.REDO.APAP.CLEAR.PARAM'
    F.REDO.APAP.CLEAR.PARAM = ''
    CALL OPF(FN.REDO.APAP.CLEAR.PARAM,F.REDO.APAP.CLEAR.PARAM)

*    CALL F.READ(FN.REDO.AUT.INP.VERSION.NAME,'SYSTEM',R.REDO.AUT.INP.VERSION.NAME,F.REDO.AUT.INP.VERSION.NAME,VER.ERR)
    CALL CACHE.READ(FN.REDO.AUT.INP.VERSION.NAME,'SYSTEM',R.REDO.AUT.INP.VERSION.NAME,RAIVN.ERR)
    Y.INP.VERSION.NAME  = R.REDO.AUT.INP.VERSION.NAME<REDO.PRE.INP.VER.NAME>
    CHANGE @VM TO @FM IN Y.INP.VERSION.NAME
    Y.AUTH.VERSION.NAME = R.REDO.AUT.INP.VERSION.NAME<REDO.PRE.AUTH.VER.NAME>
    CHANGE @VM TO @FM IN Y.AUTH.VERSION.NAME
    Y.DELT.VERSION.NAME = R.REDO.AUT.INP.VERSION.NAME<REDO.PRE.DEL.VER.NAME>
    CHANGE @VM TO @FM IN Y.DELT.VERSION.NAME

    CALL CACHE.READ(FN.REDO.APAP.CLEAR.PARAM,'SYSTEM',R.REDO.APAP.CLEAR.PARAM,PARAM.ERR)

* End of Fix

RETURN
*
*------------
LOC.CRT.FLDS:
*------------
*
    WAPPL        = 'USER' : @FM : 'FUNDS.TRANSFER' : @FM : 'TELLER' : @FM : 'T24.FUND.SERVICES'
    WCAMPO       = 'L.US.IDC.BR' : @VM : 'L.US.IDC.CODE' : @FM : 'L.ACTUAL.VERSIO' : @FM : 'L.ACTUAL.VERSIO' : @VM : 'T24.FS.REF' : @FM : 'L.T24FS.TRA.DAY'
    YPOS         = ''
    CALL MULTI.GET.LOC.REF(WAPPL,WCAMPO,YPOS)
    Y.BRANCH.POS = YPOS<1,1>
    Y.DEPT.POS   = YPOS<1,2>

* Fix for PACS00245167 [AUTH.DATE - current variable issue]

    LOC.FT.VER.POS = YPOS<2,1>
    LOC.TT.VER.POS = YPOS<3,1>
    POS.T24.FS.REF = YPOS<3,2>
    LOC.TFS.VER.POS= YPOS<4,1>


* End of Fix

    Y.TTID = '' ; Y.USER = '' ; Y.TXNID = ''
*
    LOCATE "CASHIER" IN D.FIELDS<1> SETTING Y.TTID.POS THEN
        Y.TTID  = D.RANGE.AND.VALUE<Y.TTID.POS>
    END
*
    LOCATE "USER" IN D.FIELDS<1> SETTING Y.USER.POS THEN
        Y.USER  = D.RANGE.AND.VALUE<Y.USER.POS>
    END
*
    LOCATE "TRANS.ID" IN D.FIELDS<1> SETTING Y.TXNID.POS THEN
        Y.TXNID  = D.RANGE.AND.VALUE<Y.TXNID.POS>
    END
    YSKIP.FLAG = 0

    TTODAY = TODAY
    TY = TTODAY[1,4]
    DIFF.DATE = TY - 1
    TD = TTODAY[5,4]
    DIFF.DT = DIFF.DATE:TD
    DIFF.DATE.CAL = DIFF.DT[3,6]
*
RETURN
*
PROCESS:
*-------

    Y.FINAL.DATA = ''
    GOSUB FT.PROCESS
    GOSUB TT.PROCESS
    GOSUB TFS.PROCESS
RETURN
*
*-----------
SEL.FLDS.TT:
*-----------
*
    IF Y.TTID THEN
        SEL.CMD1:=" AND WITH TELLER.ID.1 EQ " :Y.TTID
    END
*
    IF Y.USER THEN
        SEL.CMD1:=" AND WITH L.INP.USER.ID EQ ":Y.USER
    END
*
    IF Y.TXNID THEN
        SEL.CMD1:=" AND WITH @ID EQ " :Y.TXNID
    END
*
RETURN
*
*-----------
SEL.FLDS.FT:
*-----------
*
    IF Y.USER THEN
        SEL.CMD:=" AND WITH L.INP.USER.ID EQ ":Y.USER
    END
*
    IF Y.TXNID THEN
        SEL.CMD:=" AND WITH @ID EQ " :Y.TXNID
    END
*
RETURN
*
*-----------
SEL.FLDS.TFS:
*-----------
    IF Y.USER THEN
        SEL.CMD:=" AND WITH L.INP.USER.ID EQ ":Y.USER
    END

    IF Y.TXNID THEN
        SEL.CMD:=" AND WITH @ID EQ " :Y.TXNID
    END

RETURN
*
*----------
TT.PROCESS:
*----------
*

    SEL.LIST1 = ''
    NO.OF.REC1 = ''
    ERR.TT = ''
*
    SEL.CMD1 = "SELECT ":FN.TELLER$NAU
    GOSUB SEL.FLDS.TT
    SEL.CMD1:= " AND WITH CO.CODE EQ ":ID.COMPANY:" AND EVAL'DATE.TIME[1,6]' GE ": DIFF.DATE.CAL

    CALL EB.READLIST(SEL.CMD1,SEL.LIST1,'',NO.OF.REC1,ERR.TT)

    Y.DATA.TMP = SEL.LIST1
    GOSUB SEL.PRE.TT
*
RETURN
*
*----------
FT.PROCESS:
*----------
*
    SEL.CMD = ''; SEL.LIST = ''; NO.OF.REC = ''; ERR.FT = ''

    SEL.CMD = "SELECT ":FN.FUNDS.TRANSFER$NAU
    GOSUB SEL.FLDS.FT
    SEL.CMD:= " AND WITH CO.CODE EQ ":ID.COMPANY:" AND EVAL'DATE.TIME[1,6]' GE ": DIFF.DATE.CAL

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.FT)

    IF Y.TTID THEN
        SEL.LIST = ''
    END
*
    Y.DATA.TMP = SEL.LIST
    GOSUB SEL.PRE.FT
*
RETURN
*
*----------------------
TFS.PROCESS:
*----------------------

    SEL.CMD = ''
    SEL.LIST = ''
    NO.OF.REC = ''
    ERR.TFS = ''
    SEL.CMD = "SELECT ":FN.T24.FUND.SERVICES$NAU
    GOSUB SEL.FLDS.TFS
    SEL.CMD:= " AND WITH CO.CODE EQ ":ID.COMPANY:" AND EVAL'DATE.TIME[1,6]' GE ": DIFF.DATE.CAL
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.TFS)

    IF Y.TTID THEN
        SEL.LIST = ''
    END

    Y.DATA.TMP = SEL.LIST
    GOSUB SEL.PRE.TFS

RETURN
*
*----------
SEL.PRE.TT:
*----------
*
    Y.DATA.CNT = "" ; Y.DATA.CNT = DCOUNT(Y.DATA.TMP,@FM) ; Y.CNT = 1

    LOOP
    WHILE Y.CNT LE Y.DATA.CNT
*
        TELLER$NAU.ID = '' ; TELLER$NAU.ID = Y.DATA.TMP<Y.CNT>
        R.TELLER$NAU  = '' ; YERR          = ''
        CALL F.READ(FN.TELLER$NAU,TELLER$NAU.ID,R.TELLER$NAU,F.TELLER$NAU,YERR)

        IF R.TELLER$NAU THEN
            Y.APP.USER     = ''
            Y.APP.USER     = R.TELLER$NAU<TT.TE.INPUTTER>
            Y.APP.USER = FIELD(Y.APP.USER,"_",2)

* Fix for PACS00245167 [AUTH.DATE - current variable issue]

            Y.VERSION.NAME  = R.TELLER$NAU<TT.TE.LOCAL.REF,LOC.TT.VER.POS>
            Y.T24.FS.REF    = R.TELLER$NAU<TT.TE.LOCAL.REF,POS.T24.FS.REF>
            IF Y.T24.FS.REF NE '' THEN
                Y.CNT++
                CONTINUE
            END

            GOSUB CHECK.VERSION.NAME
            IF YSKIP.FLAG EQ 0 THEN
                Y.CNT++
                CONTINUE
            END
* End of Fix

            GOSUB GET.USER.IDC
            IF Y.CODE.VAL NE "" AND Y.CODE.VAL LE 200 THEN
                Y.DATA<-1> = TELLER$NAU.ID
                Y.FINAL.DATA<-1> = TELLER$NAU.ID:'*':Y.VERSION.NAME:'*':Y.DEL.VERSION.NAME
            END
        END
        Y.CNT++
*
    REPEAT
*
RETURN
*
*----------
SEL.PRE.FT:
*----------
*
    Y.DATA.CNT = "" ; Y.DATA.CNT = DCOUNT(Y.DATA.TMP,@FM) ; Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.DATA.CNT
*
        FUNDS.TRANSFER$NAU.ID = '' ; FUNDS.TRANSFER$NAU.ID = Y.DATA.TMP<Y.CNT>
        R.FUNDS.TRANSFER$NAU  = '' ; YERR          = ''
        CALL F.READ(FN.FUNDS.TRANSFER$NAU,FUNDS.TRANSFER$NAU.ID,R.FUNDS.TRANSFER$NAU,F.FUNDS.TRANSFER$NAU,YERR)
        Y.APP.USER     = ''
        Y.APP.USER     = R.FUNDS.TRANSFER$NAU<FT.INPUTTER>
        Y.APP.USER = FIELD(Y.APP.USER,"_",2)

* Fix for PACS00245167 [AUTH.DATE - current variable issue]

        Y.VERSION.NAME  = R.FUNDS.TRANSFER$NAU<FT.LOCAL.REF,LOC.FT.VER.POS>
        GOSUB CHECK.VERSION.NAME
        IF YSKIP.FLAG EQ 0 THEN
            Y.CNT++
            CONTINUE
        END

* End of Fix

        GOSUB GET.USER.IDC
        IF Y.CODE.VAL NE "" AND Y.CODE.VAL LE 200 THEN
            Y.DATA<-1> = FUNDS.TRANSFER$NAU.ID
            Y.FINAL.DATA<-1> = FUNDS.TRANSFER$NAU.ID:'*':Y.VERSION.NAME:'*':Y.DEL.VERSION.NAME
        END
        Y.CNT++
*
    REPEAT
*
RETURN
*
*-----------
SEL.PRE.TFS:
*-----------
*
    Y.DATA.CNT = "" ; Y.DATA.CNT = DCOUNT(Y.DATA.TMP,@FM) ; Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.DATA.CNT
*
        T24.FUND.SERVICES$NAU.ID = '' ; T24.FUND.SERVICES$NAU.ID = Y.DATA.TMP<Y.CNT>
        R.T24.FUND.SERVICES$NAU  = '' ; YERR          = ''
        CALL F.READ(FN.T24.FUND.SERVICES$NAU,T24.FUND.SERVICES$NAU.ID,R.T24.FUND.SERVICES$NAU,F.T24.FUND.SERVICES$NAU,YERR)
        Y.APP.USER     = ''
        Y.APP.USER     = R.T24.FUND.SERVICES$NAU<TFS.INPUTTER>
        Y.TFS.REV.MARK = R.T24.FUND.SERVICES$NAU<TFS.R.UL.STATUS>
        Y.REV.MARK     = R.T24.FUND.SERVICES$NAU<TFS.REVERSAL.MARK>


        Y.APP.USER = FIELD(Y.APP.USER,"_",2)

* Fix for PACS00245167 [AUTH.DATE - current variable issue]

        Y.VERSION.NAME = R.T24.FUND.SERVICES$NAU<TFS.LOCAL.REF,LOC.TFS.VER.POS>


        IF 'RNAU' MATCHES Y.TFS.REV.MARK OR 'R' MATCHES Y.REV.MARK THEN
            Y.VERSION.NAME = R.REDO.APAP.CLEAR.PARAM<CLEAR.PARAM.REV.VERSION.NAME>
            YSKIP.FLAG = 1
        END ELSE
            GOSUB CHECK.VERSION.NAME
        END

        IF YSKIP.FLAG EQ 0 THEN
            Y.CNT++
            CONTINUE
        END
* End of Fix

        GOSUB GET.USER.IDC
        IF Y.CODE.VAL NE "" AND Y.CODE.VAL LE 200 THEN
            Y.DATA<-1> = T24.FUND.SERVICES$NAU.ID
            Y.FINAL.DATA<-1> = T24.FUND.SERVICES$NAU.ID:'*':Y.VERSION.NAME:'*':Y.DEL.VERSION.NAME
        END
        Y.CNT++
*
    REPEAT
*
RETURN
*
*-------------
BRCH.DEPT.SEL:
*-------------
*
    POS.BR        = '' ; Y.CODE.VAL = '' ; Y.BRANCH.LIST = '' ; Y.DEPT.LIST = ''
    Y.BRANCH.LIST = R.USER.ARR<EB.USE.LOCAL.REF,Y.BRANCH.POS>
    Y.DEPT.LIST   = R.USER.ARR<EB.USE.LOCAL.REF,Y.DEPT.POS>
*
    LOCATE ID.COMPANY IN Y.BRANCH.LIST<1,1,1> SETTING POS.BR THEN
        Y.CODE.VAL = Y.DEPT.LIST<1,1,POS.BR>
    END ELSE
        Y.CODE.VAL = ''
    END
*
RETURN
*
*------------
GET.USER.IDC:
*------------
*
    R.USER.ARR = '' ; YERR = ''
    CALL F.READ(FN.USER,Y.APP.USER,R.USER.ARR,F.USER,YERR)
    IF R.USER.ARR NE "" THEN
        GOSUB BRCH.DEPT.SEL
    END
*
RETURN
*
*------------------*
CHECK.VERSION.NAME:
*------------------*
* Fix for PACS00245167 [AUTH.DATE - current variable issue]
    YSKIP.FLAG = 0
    LOCATE Y.VERSION.NAME IN Y.INP.VERSION.NAME SETTING Y.POS THEN
        Y.VERSION.NAME = Y.AUTH.VERSION.NAME<Y.POS>
        Y.DEL.VERSION.NAME = Y.DELT.VERSION.NAME<Y.POS>
        IF Y.DEL.VERSION.NAME EQ '' THEN
            Y.DEL.VERSION.NAME = Y.VERSION.NAME
        END
        YSKIP.FLAG = 1
    END

* End of Fix

RETURN

*---------------------
END
