* @ValidationCode : MjotMzc1MjY0MjA4OkNwMTI1MjoxNjgwNjgwNDA0NTgxOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 13:10:04
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.LY.PGEN.BIRTH(CUS.ID)
*-------------------------------------------------------------------------------------------------
*DESCRIPTION:
*  This routine is attached to the batch record BNK/REDO.B.LY.PGEN.BIRTH
*  This routine selects the necesary records from various files and generate the points for
*  birthday event
* ------------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 17-JUN-2013   RMONDRAGON        ODR-2011-06-0243      Initial Creation
* 04-APR-2023     Conversion tool   R22 Auto conversion   FM TO @FM, VM to @VM, ++ to +=, TNO to C$T24.SESSION.NO
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*----------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT

    $INSERT I_F.REDO.LY.POINTS
    $INSERT I_F.REDO.LY.POINTS.TOT
    $INSERT I_F.REDO.LY.MASTERPRGDR
    $INSERT I_REDO.B.LY.PGEN.BIRTH.COMMON

    GOSUB OPEN.FILE
    GOSUB PROCESS

RETURN

*---------
OPEN.FILE:
*---------

    FN.TEMP.LY.PGEN.BIRTH = 'F.TEMP.LY.PGEN.BIRTH'
    F.TEMP.LY.PGEN.BIRTH = ''
    OPEN FN.TEMP.LY.PGEN.BIRTH TO F.TEMP.LY.PGEN.BIRTH ELSE

        TEXT = 'Error in opening : ':FN.TEMP.LY.PGEN.BIRTH
        CALL FATAL.ERROR('REDO.B.LY.PGEN.BIRTH')
    END

RETURN

*-------
PROCESS:
*-------

    CRT 'PROCESSING CUSTOMER NO. ':CUS.ID

    Y.MOD.TOT = DCOUNT(PRG.MOD.LST,@FM)

    R.CUSTOMER = '' ; CUS.ERR = ''
    CALL F.READ(FN.CUSTOMER,CUS.ID,R.CUSTOMER,F.CUSTOMER,CUS.ERR)
    IF R.CUSTOMER THEN
        CUS.LG = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.L.CU.G.LEALTAD>
    END

    MOD.CNT = 1
    LOOP
    WHILE MOD.CNT LE Y.MOD.TOT
        Y.PRG.TOT = DCOUNT(PRG.PER.MOD<MOD.CNT>,@VM)
        PRG.CNT = 1
        LOOP
        WHILE PRG.CNT LE Y.PRG.TOT
            PRG.ID = PRG.LST<MOD.CNT,PRG.CNT>
            Y.PRG.LG = PRG.CUS.GRP.LST<MOD.CNT,PRG.CNT>
            GOSUB CHECK.PRG.PARAMS
            IF Y.PROCESS EQ 'Y' THEN
                IF Y.PRG.LG NE 'ALLCG' AND Y.PRG.LG EQ CUS.LG THEN
                    GOSUB CHECK.CUS.ACCT
                END
                IF Y.PRG.LG EQ 'ALLCG' AND CUS.LG NE '' THEN
                    GOSUB CHECK.CUS.ACCT
                END
            END
            PRG.CNT += 1
        REPEAT
        MOD.CNT += 1
    REPEAT

RETURN

*----------------
CHECK.PRG.PARAMS:
*----------------

    Y.PROCESS = 'Y'

    IF PRG.END.DATE.LST<MOD.CNT,PRG.CNT> THEN
        IF TODAY LE PRG.END.DATE.LST<MOD.CNT,PRG.CNT> THEN
            GOSUB UPD.EXP.DATE
        END ELSE
            Y.PROCESS = 'N'
        END
    END ELSE
        GOSUB UPD.EXP.DATE
    END

RETURN

*------------
UPD.EXP.DATE:
*------------

    IF PRG.EXP.DATE.LST<MOD.CNT,PRG.CNT> EQ '' THEN
        DAYS.EXP = PRG.DAYS.EXP.LST<MOD.CNT,PRG.CNT>
        EXP.DATE = TODAY
        TEMP.DAYS.EXP = '+':DAYS.EXP:'C'
        CALL CDT('',EXP.DATE,TEMP.DAYS.EXP)
    END ELSE
        EXP.DATE = PRG.EXP.DATE.LST<MOD.CNT,PRG.CNT>
    END

RETURN

*--------------
CHECK.CUS.ACCT:
*--------------


*  READ Y.CUS.ACCTS FROM F.CUSTOMER.ACCOUNT,CUS.ID ELSE ;*Tus Start
    CALL F.READ(FN.CUSTOMER.ACCOUNT,CUS.ID,Y.CUS.ACCTS,F.CUSTOMER.ACCOUNT,Y.CUS.ACCTS.ERR)
    IF Y.CUS.ACCTS.ERR THEN  ;* Tus End
        CRT 'No Accounts for this customer'
        RETURN
    END
    Y.CUS.ACCTS.CNT = DCOUNT(Y.CUS.ACCTS,@FM)

    Y.PROD.LIST.CNT = DCOUNT(PRG.PROD.LST<MOD.CNT,PRG.CNT>,'*')

    Y.PROD.TO.CNT = 1
    Y.ADD.PTS = 0
    Y.ACCT.PROC.OK = 0
    LOOP
    WHILE Y.PROD.TO.CNT LE Y.PROD.LIST.CNT
        Y.PROD.CAT = FIELD(PRG.PROD.LST<MOD.CNT,PRG.CNT>,'*',Y.PROD.TO.CNT)
        Y.ACCT.NO = 1
        LOOP
        WHILE Y.ACCT.NO LE Y.CUS.ACCTS.CNT
            Y.CUS.ACCT.NO = FIELD(Y.CUS.ACCTS,@FM,Y.ACCT.NO)
            R.ACCOUNT = ''; ACCT.ERR = ''
            CALL F.READ(FN.ACCOUNT,Y.CUS.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ACCT.ERR)
            IF R.ACCOUNT THEN
                Y.CCY = R.ACCOUNT<AC.CURRENCY>
                Y.CAT = R.ACCOUNT<AC.CATEGORY>
                PROD.ID = Y.CAT
                IF Y.PROD.CAT EQ Y.CAT AND PRG.CCY.IN.MOD<MOD.CNT,PRG.CNT> EQ Y.CCY THEN
                    GOSUB VAL.ACC.IECOND
                    IF GEN EQ 'G' THEN
                        GOSUB CHECK.PT.GEN
                        Y.ACCT.NO = Y.CUS.ACCTS.CNT
                        Y.PROD.TO.CNT = Y.PROD.LIST.CNT
                    END
                END
            END
            Y.ACCT.NO += 1
        REPEAT
        Y.PROD.TO.CNT += 1
    REPEAT

RETURN

*--------------
VAL.ACC.IECOND:
*--------------

    GEN = 'G'

    IF Y.CAT GE '3000' AND Y.CAT LE '3999' THEN
        RETURN
    END

    IPRG.COND.TYPE.EXINC = PRG.COND.TYPE.EXINC<MOD.CNT,PRG.CNT>
    IPRG.APP.EXC.COND = PRG.APP.EXC.COND<MOD.CNT,PRG.CNT>
    IPRG.EXC.EST.ACCT = PRG.EXC.EST.ACCT<MOD.CNT,PRG.CNT>
    CHANGE '*' TO @FM IN IPRG.EXC.EST.ACCT
    IPRG.APP.INC.COND = PRG.APP.INC.COND<MOD.CNT,PRG.CNT>
    IPRG.INC.EST.ACCT = PRG.INC.EST.ACCT<MOD.CNT,PRG.CNT>
    CHANGE '*' TO @FM IN IPRG.INC.EST.ACCT

    Y.AC.STATUS1 = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.STATUS1>
    Y.AC.STATUS2 = R.ACCOUNT<AC.LOCAL.REF,POS.L.AC.STATUS2>

    IF IPRG.COND.TYPE.EXINC EQ '' THEN
        RETURN
    END

    IF IPRG.COND.TYPE.EXINC EQ 'ESTADO.CUENTA' AND IPRG.APP.EXC.COND EQ 'ESPECIFICA' THEN
        GOSUB CHECK.STATUS.FOR.EXC
    END ELSE
        Y.PROC.INC.COND = 1
    END

    IF Y.PROC.INC.COND EQ '1' THEN
        IF IPRG.COND.TYPE.EXINC EQ 'ESTADO.CUENTA' AND IPRG.APP.INC.COND EQ 'ESPECIFICA' THEN
            GOSUB CHECK.STATUS.FOR.INC
        END
    END

RETURN

*--------------------
CHECK.STATUS.FOR.EXC:
*--------------------

    LOCATE Y.AC.STATUS1 IN IPRG.EXC.EST.ACCT SETTING EXC.EST.ACCT.POS THEN
        GEN = 'NG'
    END ELSE
        LOCATE Y.AC.STATUS2 IN IPRG.EXC.EST.ACCT SETTING EXC.EST.ACCT.POS THEN
            GEN = 'NG'
        END
    END

RETURN

*--------------------
CHECK.STATUS.FOR.INC:
*--------------------

    LOCATE Y.AC.STATUS1 IN IPRG.INC.EST.ACCT SETTING INC.EST.ACCT.POS ELSE
        LOCATE Y.AC.STATUS2 IN IPRG.INC.EST.ACCT SETTING INC.EST.ACCT.POS ELSE
            GEN = 'NG'
        END
    END

RETURN

*------------
CHECK.PT.GEN:
*------------

    CRT '-> GENERATING POINTS FOR PROGRAM ID ':PRG.ID:'...'
    PT.QTY = PRG.PTS.IN.MOD<MOD.CNT,PRG.CNT>
    PT.VALUE = PRG.POINT.VALUE.LST<MOD.CNT,PRG.CNT>
    QTY.VALUE = PT.QTY * PT.VALUE
    GOSUB UPD.REDO.LY.PTS

RETURN

*---------------
UPD.REDO.LY.PTS:
*---------------

    R.REDO.LY.POINTS = ''
    CALL F.READ(FN.REDO.LY.POINTS,CUS.ID,R.REDO.LY.POINTS,F.REDO.LY.POINTS,RLP.ERR)
    GOSUB ASSIGN.AUDIT
    PRO.LST = R.REDO.LY.POINTS<REDO.PT.PRODUCT>
    GOSUB ASSIGN.LY.PTS
    CALL F.WRITE(FN.REDO.LY.POINTS,CUS.ID,R.REDO.LY.POINTS)
    GOSUB ASSIGN.LY.PTS.TOT

RETURN

*------------
ASSIGN.AUDIT:
*------------

    CURR.NO = ''
    CUR.TIME = OCONV(TIME(), "MT")
    CHANGE ':' TO '' IN CUR.TIME
    CURR.NO = R.REDO.LY.POINTS<REDO.PT.CURR.NO>
    IF CURR.NO EQ '' THEN
        CURR.NO = 1
    END ELSE
        CURR.NO += 1
    END
    R.REDO.LY.POINTS<REDO.PT.RECORD.STATUS> = ''
    R.REDO.LY.POINTS<REDO.PT.CURR.NO> = CURR.NO
    R.REDO.LY.POINTS<REDO.PT.INPUTTER> = C$T24.SESSION.NO:'_':OPERATOR
    R.REDO.LY.POINTS<REDO.PT.DATE.TIME> = G.DATE[3,6]:CUR.TIME
    R.REDO.LY.POINTS<REDO.PT.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR
    R.REDO.LY.POINTS<REDO.PT.CO.CODE> = ID.COMPANY
    R.REDO.LY.POINTS<REDO.PT.DEPT.CODE> = 1

RETURN

*-------------
ASSIGN.LY.PTS:
*-------------

    PTS.CHG = 1
    CHANGE @VM TO @FM IN PRO.LST
    LOCATE PROD.ID IN PRO.LST SETTING PRO.INS.POS ELSE
        R.REDO.LY.POINTS<REDO.PT.PRODUCT,PRO.INS.POS> = PROD.ID
    END
    R.REDO.LY.POINTS<REDO.PT.PROGRAM,PRO.INS.POS,-1> = PRG.ID
    R.REDO.LY.POINTS<REDO.PT.TXN.ID,PRO.INS.POS,-1> = Y.CUS.ACCT.NO
    R.REDO.LY.POINTS<REDO.PT.QUANTITY,PRO.INS.POS,-1> = PT.QTY
    R.REDO.LY.POINTS<REDO.PT.QTY.VALUE,PRO.INS.POS,-1> = QTY.VALUE
    IF PRG.AIR.LST<MOD.CNT,PRG.CNT> EQ 'SI' THEN
        R.REDO.LY.POINTS<REDO.PT.STATUS,PRO.INS.POS,-1> = 'Pendiente.Someter'
    END ELSE
        R.REDO.LY.POINTS<REDO.PT.STATUS,PRO.INS.POS,-1> = 'Liberada'
    END
    R.REDO.LY.POINTS<REDO.PT.GEN.DATE,PRO.INS.POS,-1> = TODAY
    R.REDO.LY.POINTS<REDO.PT.AVAIL.DATE,PRO.INS.POS,-1> = TODAY
    R.REDO.LY.POINTS<REDO.PT.EXP.DATE,PRO.INS.POS,-1> = EXP.DATE
    R.REDO.LY.POINTS<REDO.PT.MAN.DATE,PRO.INS.POS,-1> = ''
    R.REDO.LY.POINTS<REDO.PT.MAN.QTY,PRO.INS.POS,-1> = ''
    R.REDO.LY.POINTS<REDO.PT.MAN.DESC,PRO.INS.POS,-1> = ''
    R.REDO.LY.POINTS<REDO.PT.MAN.USER,PRO.INS.POS,-1> = ''

RETURN

*-----------------
ASSIGN.LY.PTS.TOT:
*-----------------

    GOSUB UPD.PTS.MMYY
    GOSUB UPD.PTS.PGM.YY
    GOSUB UPD.PTS.YYYY
    GOSUB UPD.TOTALS
    IF PRG.POINT.USE.LST<MOD.CNT,PRG.CNT> EQ 4 THEN
        GOSUB UPD.PTS.CUS
    END
    IF PRG.POINT.USE.LST<MOD.CNT,PRG.CNT> EQ 3 THEN
        GOSUB UPD.PTS.BUS
    END

    Y.UPD.ONLINE = 0
    GOSUB CHECK.MASTER.PRG
    IF Y.UPD.ONLINE EQ 1  THEN
        GOSUB UPD.PTS.FOR.DEB
        IF PRG.ID.OLD NE '' THEN
            PRG.ID = PRG.ID.OLD
        END
    END

RETURN

*----------------
CHECK.MASTER.PRG:
*----------------

    Y.MASTER.PRG = ''
    Y.SLAVES.PRG = ''
    R.REDO.LY.MASTERPRGDR = '' ; MAS.ERR = ''

*  CALL F.READ(FN.REDO.LY.MASTERPRGDR,'SYSTEM',R.REDO.LY.MASTERPRGDR,F.REDO.LY.MASTERPRGDR,MAS.ERR) ;*Tus Start
    CALL CACHE.READ(FN.REDO.LY.MASTERPRGDR,'SYSTEM',R.REDO.LY.MASTERPRGDR,MAS.ERR) ; * Tus End
    IF R.REDO.LY.MASTERPRGDR THEN
        Y.MASTER.PRG = R.REDO.LY.MASTERPRGDR<REDO.MASPRG.MASTER.PRG>
        Y.SLAVES.PRG = R.REDO.LY.MASTERPRGDR<REDO.MASPRG.SLAVE.PRG>
    END

    Y.SLAVES.PRG.CNT = DCOUNT(Y.SLAVES.PRG,@VM)

    IF Y.SLAVES.PRG.CNT EQ 0 AND Y.MASTER.PRG NE '' THEN
        IF PRG.ID EQ Y.MASTER.PRG THEN
            PRG.ID = Y.MASTER.PRG
            Y.UPD.ONLINE = 1
            RETURN
        END
    END

    Y.SLAVE.CNT = 1
    LOOP
    WHILE Y.SLAVE.CNT LE Y.SLAVES.PRG.CNT
        Y.SLAVE.PRG = FIELD(Y.SLAVES.PRG,@VM,Y.SLAVE.CNT)
        IF PRG.ID EQ Y.MASTER.PRG OR PRG.ID EQ Y.SLAVE.PRG THEN
            PRG.ID.OLD = PRG.ID
            PRG.ID = Y.MASTER.PRG
            Y.UPD.ONLINE = 1
            Y.SLAVE.CNT = Y.SLAVES.PRG.CNT
        END
        Y.SLAVE.CNT += 1
    REPEAT

RETURN

*------------
UPD.PTS.MMYY:
*------------

    TOT.POINTS.ID = CUS.ID:PRG.ID:CUR.MONTH:CUR.YEAR
    R.REDO.LY.POINTS.TOT =''
    CALL F.READ(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,TOT.ERR)
    GOSUB UPD.PROCESS
    GOSUB ASSIGN.AUDIT.TOT
    CALL F.WRITE(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT)

RETURN

*--------------
UPD.PTS.PGM.YY:
*--------------

    TOT.POINTS.ID = CUS.ID:PRG.ID:'ALL':CUR.YEAR
    R.REDO.LY.POINTS.TOT =''
    CALL F.READ(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,TOT.ERR)
    GOSUB UPD.PROCESS
    GOSUB ASSIGN.AUDIT.TOT
    CALL F.WRITE(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT)

RETURN

*------------
UPD.PTS.YYYY:
*------------

    TOT.POINTS.ID = CUS.ID:'ALL':CUR.YEAR
    R.REDO.LY.POINTS.TOT =''
    CALL F.READ(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,TOT.ERR)
    GOSUB UPD.PROCESS
    GOSUB ASSIGN.AUDIT.TOT
    CALL F.WRITE(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT)

RETURN

*-----------
UPD.PTS.CUS:
*-----------

    TOT.POINTS.ID = CUS.ID:'C'
    R.REDO.LY.POINTS.TOT =''
    CALL F.READ(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,TOT.ERR)
    GOSUB UPD.PROCESS
    GOSUB ASSIGN.AUDIT.TOT
    CALL F.WRITE(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT)

RETURN

*-----------
UPD.PTS.BUS:
*-----------

    TOT.POINTS.ID = CUS.ID:'B'
    R.REDO.LY.POINTS.TOT =''
    CALL F.READ(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,TOT.ERR)
    GOSUB UPD.PROCESS
    GOSUB ASSIGN.AUDIT.TOT
    CALL F.WRITE(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT)

RETURN

*---------------
UPD.PTS.FOR.DEB:
*---------------

    TOT.POINTS.ID = CUS.ID:'ONLINEDEB'
    R.REDO.LY.POINTS.TOT =''
    CALL F.READ(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT,F.REDO.LY.POINTS.TOT,TOT.ERR)
    GOSUB UPD.PROCESS.DEB
    GOSUB ASSIGN.AUDIT.TOT
    CALL F.WRITE(FN.REDO.LY.POINTS.TOT,TOT.POINTS.ID,R.REDO.LY.POINTS.TOT)

RETURN

*-----------
UPD.PROCESS:
*-----------

    VAR.AVAIL = 0 ; VAR.AVAIL.VALUE = 0 ; VAR.NAVAIL = 0 ; VAR.NAVAIL.VALUE = 0
    VAR.TOT.GEN = 0 ; VAR.TOT.GEN.VALUE = 0
    IF R.REDO.LY.POINTS.TOT EQ '' THEN
        VAR.NAVAIL = 0
        VAR.NAVAIL.VALUE = 0
        VAR.TOT.GEN = 0
        VAR.TOT.GEN.VALUE = 0
        VAR.AVAIL = 0
        VAR.AVAIL.VALUE = 0
    END ELSE
        VAR.TOT.GEN = R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.GEN.POINTS>
        VAR.TOT.GEN.VALUE = R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.GEN.VALUE>
        VAR.AVAIL   =  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.POINTS>
        VAR.AVAIL.VALUE =  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.VALUE>
        VAR.NAVAIL   =  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.NAVAIL.POINTS>
        VAR.NAVAIL.VALUE =  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.NAVAIL.VALUE>
    END
    IF PT.QTY GT 0 THEN
        VAR.TOT.GEN += PT.QTY
        VAR.TOT.GEN.VALUE += QTY.VALUE
    END
    VAR.AVAIL += PT.QTY
    VAR.AVAIL.VALUE += QTY.VALUE
    IF PT.QTY LT 0 THEN
        VAR.NAVAIL = VAR.NAVAIL + NEG(PT.QTY)
        VAR.NAVAIL.VALUE = VAR.NAVAIL.VALUE + NEG(QTY.VALUE)
    END
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.POINTS> = VAR.AVAIL
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.VALUE> = VAR.AVAIL.VALUE
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.GEN.POINTS> = VAR.TOT.GEN
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.GEN.VALUE> = VAR.TOT.GEN.VALUE
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.NAVAIL.POINTS> = VAR.NAVAIL
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.NAVAIL.VALUE> = VAR.NAVAIL.VALUE

RETURN

*----------
UPD.TOTALS:
*----------


*  READU VAR.T.GEN FROM F.TEMP.LY.PGEN.BIRTH,'VAR.T.GEN' ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.TEMP.LY.PGEN.BIRTH,'VAR.T.GEN',VAR.T.GEN,F.TEMP.LY.PGEN.BIRTH,VAR.T.GEN.ERR,RETRY.VAR)
    IF VAR.T.GEN.ERR THEN  ;* Tus End
        VAR.T.GEN<MOD.CNT,PRG.CNT> = 0
    END


*  READU VAR.T.GEN.VALUE FROM F.TEMP.LY.PGEN.BIRTH,'VAR.T.GEN.VALUE' ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.TEMP.LY.PGEN.BIRTH,'VAR.T.GEN.VALUE',VAR.T.GEN.VALUE,F.TEMP.LY.PGEN.BIRTH,VAR.T.GEN.VALUE.ERR,RETRY.VAR)
    IF VAR.T.GEN.VALUE.ERR THEN  ;* Tus End
        VAR.T.GEN.VALUE<MOD.CNT,PRG.CNT> = 0
    END


*  READU VAR.T.AVAIL FROM F.TEMP.LY.PGEN.BIRTH,'VAR.T.AVAIL' ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.TEMP.LY.PGEN.BIRTH,'VAR.T.AVAIL',VAR.T.AVAIL,F.TEMP.LY.PGEN.BIRTH,VAR.T.AVAIL.ERR,RETRY.VAR)
    IF VAR.T.AVAIL.ERR THEN  ;* Tus End
        VAR.T.AVAIL<MOD.CNT,PRG.CNT> = 0
    END


*  READU VAR.T.AVAIL.VALUE FROM F.TEMP.LY.PGEN.BIRTH,'VAR.T.AVAIL.VALUE' ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.TEMP.LY.PGEN.BIRTH,'VAR.T.AVAIL.VALUE',VAR.T.AVAIL.VALUE,F.TEMP.LY.PGEN.BIRTH,VAR.T.AVAIL.VALUE.ERR,RETRY.VAR)
    IF VAR.T.AVAIL.VALUE.ERR THEN  ;* Tus End
        VAR.T.AVAIL.VALUE<MOD.CNT,PRG.CNT> = 0
    END


*  READU VAR.T.NAVAIL FROM F.TEMP.LY.PGEN.BIRTH,'VAR.T.NAVAIL' ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.TEMP.LY.PGEN.BIRTH,'VAR.T.NAVAIL',VAR.T.NAVAIL,F.TEMP.LY.PGEN.BIRTH,VAR.T.NAVAIL.ERR,RETRY.VAR)
    IF VAR.T.NAVAIL.ERR THEN  ;* Tus End
        VAR.T.NAVAIL<MOD.CNT,PRG.CNT> = 0
    END


*  READU VAR.T.NAVAIL.VALUE FROM F.TEMP.LY.PGEN.BIRTH,'VAR.T.NAVAIL.VALUE' ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.TEMP.LY.PGEN.BIRTH,'VAR.T.NAVAIL.VALUE',VAR.T.NAVAIL.VALUE,F.TEMP.LY.PGEN.BIRTH,VAR.T.NAVAIL.VALUE.ERR,RETRY.VAR)
    IF VAR.T.NAVAIL.VALUE.ERR THEN  ;* Tus End
        VAR.T.NAVAIL.VALUE<MOD.CNT,PRG.CNT> = 0
    END

    IF PT.QTY GT 0 THEN
        VAR.T.GEN<MOD.CNT,PRG.CNT> += PT.QTY
        VAR.T.GEN.VALUE<MOD.CNT,PRG.CNT> += QTY.VALUE
    END
    VAR.T.AVAIL<MOD.CNT,PRG.CNT> += PT.QTY
    VAR.T.AVAIL.VALUE<MOD.CNT,PRG.CNT> += QTY.VALUE
    IF PT.QTY LT 0 THEN
        VAR.T.NAVAIL<MOD.CNT,PRG.CNT> = VAR.T.NAVAIL + NEG(PT.QTY)
        VAR.T.NAVAIL.VALUE<MOD.CNT,PRG.CNT> = VAR.T.NAVAIL.VALUE + NEG(QTY.VALUE)
    END

    WRITEU VAR.T.GEN TO F.TEMP.LY.PGEN.BIRTH,'VAR.T.GEN'
    WRITEU VAR.T.GEN.VALUE TO F.TEMP.LY.PGEN.BIRTH,'VAR.T.GEN.VALUE'
    WRITEU VAR.T.AVAIL TO F.TEMP.LY.PGEN.BIRTH,'VAR.T.AVAIL'
    WRITEU VAR.T.AVAIL.VALUE TO F.TEMP.LY.PGEN.BIRTH,'VAR.T.AVAIL.VALUE'
    WRITEU VAR.T.NAVAIL TO F.TEMP.LY.PGEN.BIRTH,'VAR.T.NAVAIL'
    WRITEU VAR.T.NAVAIL.VALUE TO F.TEMP.LY.PGEN.BIRTH,'VAR.T.NAVAIL.VALUE'

RETURN

*---------------
UPD.PROCESS.DEB:
*---------------

    VAR.AVAIL = 0 ; VAR.AVAIL.VALUE = 0

    IF R.REDO.LY.POINTS.TOT EQ '' THEN
        VAR.AVAL = 0
        VAR.AVAIL.VALUE = 0
    END ELSE
        VAR.AVAIL = R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.POINTS>
        VAR.AVAIL.VALUE =  R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.VALUE>
    END

    VAR.AVAIL += PT.QTY
    VAR.AVAIL.VALUE += QTY.VALUE

    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.POINTS> = VAR.AVAIL
    R.REDO.LY.POINTS.TOT<REDO.PT.T.TOT.AVAIL.VALUE> = VAR.AVAIL.VALUE

RETURN

*----------------
ASSIGN.AUDIT.TOT:
*----------------

    CURR.NO = ''
    CUR.TIME = OCONV(TIME(), "MT")
    CHANGE ':' TO '' IN CUR.TIME
    CURR.NO = R.REDO.LY.POINTS.TOT<REDO.PT.T.CURR.NO>
    IF CURR.NO EQ '' THEN
        CURR.NO = 1
    END ELSE
        CURR.NO += 1
    END
    R.REDO.LY.POINTS.TOT<REDO.PT.T.RECORD.STATUS> = ''
    R.REDO.LY.POINTS.TOT<REDO.PT.T.CURR.NO> = CURR.NO
    R.REDO.LY.POINTS.TOT<REDO.PT.T.INPUTTER> = C$T24.SESSION.NO:'_':OPERATOR
    R.REDO.LY.POINTS.TOT<REDO.PT.T.DATE.TIME> = G.DATE[3,6]:CUR.TIME
    R.REDO.LY.POINTS.TOT<REDO.PT.T.AUTHORISER> = C$T24.SESSION.NO:'_':OPERATOR
    R.REDO.LY.POINTS.TOT<REDO.PT.T.CO.CODE> = ID.COMPANY
    R.REDO.LY.POINTS.TOT<REDO.PT.T.DEPT.CODE> = 1

RETURN

END
