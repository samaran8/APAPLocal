* @ValidationCode : MjotMTE3OTgzMTA4OkNwMTI1MjoxNjgyNTk4OTkzMTgxOmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 27 Apr 2023 18:06:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.DRREG
*
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*04-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   NO CHANGE
*04-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION       CALL RTN FORMAT MODIFIED
*----------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE DR.REG.213IF01.MONTHLY.EXTRACT(REC.ID)
*********
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_TSA.COMMON
    $INSERT I_F.DATES
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER

    $INSERT I_DR.REG.213IF01.MONTHLY.EXTRACT.COMMON
    $INSERT I_F.DR.REG.213IF01.PARAM
    $INSERT I_F.DR.REG.213IF01.CONCAT
    $USING APAP.LAPAP


    GOSUB PROCESS

RETURN
*--------------------------------------------------------------------------------
INIT.PROCESS:
****************
*
    CUST.TYPE = ''
    CUST.CODE = ''
    CUST.NAME = ''
    INITIALS = ''
    NATION = ''
    TYPE.OF.OPER = ''
    AC.AFFECT = ''
    ACC.STATUS = ''
    AMT.TXN = ''
    CCY = ''
    INTER.OPER = ''
    INTERMEDIARY = ''
    INTERM.NAME = ''
    INTERM.SURNAME = ''
    INT.NATION = ''
    CLNT.TYPE = ''
    PERS.BENEF = ''
    BEN.NAME1 = ''
    BEN.NAME2 = ''
    PAIS = ''
    TXN.DATE = ''
    TXN.TIME = ''
    TXN.ACCTS = ''
    CUST.INDUS = ''
    PER.PERSON = ''
    CUS.PEP = ''
    ACT.INT = ''
    PEP.INTERM = ''
    TYPE.PEP.INT = ''
*
RETURN
*--------------------------------------------------------------------------------
PROCESS:
********
*
    GOSUB INIT.PROCESS
    R.DR.REG.213IF01.CONCAT = ''
    DR.REG.213IF01.CONCAT.ERR = ''
    CONCAT.ID = REC.ID
    CALL F.READ(FN.DR.REG.213IF01.CONCAT,CONCAT.ID,R.DR.REG.213IF01.CONCAT,F.DR.REG.213IF01.CONCAT,DR.REG.213IF01.CONCAT.ERR)
    CNT.TXN = DCOUNT(R.DR.REG.213IF01.CONCAT<DR.213IF01.CONCAT.STMT.ID>,@VM)
    STMT.VAL = R.DR.REG.213IF01.CONCAT<DR.213IF01.CONCAT.STMT.ID,CNT.TXN>
    CALL F.READ(FN.STMT.ENTRY,STMT.VAL,R.STMT.ENTRY,F.STMT.ENTRY,STMT.ENTRY.ERR)
*
    CUST.ID = FIELD(CONCAT.ID,'-',1)
    CALL F.READ(FN.CUSTOMER,CUST.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    TIPO.CL.POS.VAL = R.CUSTOMER<EB.CUS.LOCAL.REF,TIPO.CL.POS>
    GOSUB GET.CUSTOMER.DETAILS
    TXN.REF = R.STMT.ENTRY<AC.STE.TRANS.REFERENCE>[1,12]
    TYPE.OF.OPER = R.STMT.ENTRY<AC.STE.TRANSACTION.CODE>
    GOSUB TYPE.OF.OPERATION
    TYPE.OF.OPER = FMT(TYPE.OPER,'2L')
    ACCT.TYPE = R.STMT.ENTRY<AC.STE.TRANSACTION.CODE>
    ACC.CODE.AFFCT.LIST = '23':@FM:'24':@FM:'25':@FM:'39':@FM:'69'      ;* Parameterize
    ACC.ID = R.STMT.ENTRY<AC.STE.ACCOUNT.NUMBER>
    R.ACCOUNT = ''
    CALL F.READ(FN.ACCOUNT,ACC.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    IF R.ACCOUNT THEN
        GOSUB GET.ACC.STATUS
    END ELSE
        ACC.STATUS = ''
    END
    ACC.STATUS = FMT(ACC.STATUS,'1L')
*
    AC.TYPE.AFFECTED = ''
*
    R.FT = ''
    R.TT = ''

    BEGIN CASE
        CASE R.STMT.ENTRY<AC.STE.SYSTEM.ID> EQ 'FT'
            FT.ID = TXN.REF:';1'
            CALL F.READ(FN.FT.HIS,FT.ID,R.FT,F.FT.HIS,FT.ERR)
            IF R.FT THEN
                INTERMEDIARY = R.FT<FT.LOCAL.REF,FT.L.NEW.ID.CARD.POS>
                IF INTERMEDIARY EQ '' THEN
                    INTERMEDIARY = R.FT<FT.LOCAL.REF,FT.L.OLD.ID.CARD.POS>
                END
                IF INTERMEDIARY EQ '' THEN
                    INTERMEDIARY = R.FT<FT.LOCAL.REF,FT.L.PASSPORT.POS>
                END
                INTERM.NAME = R.FT<FT.LOCAL.REF,FT.L.1ST.NAME.POS>:" ":R.FT<FT.LOCAL.REF,FT.L.2ND.NAME.POS>
                INTERM.SURNAME = R.FT<FT.LOCAL.REF,FT.LAST.NAME.POS>:" ":R.FT<FT.LOCAL.REF,FT.2ND.LAST.POS>
                INT.NATION = R.FT<FT.LOCAL.REF,FT.L.NATIONALITY.POS>
                SEX.VAL = R.FT<FT.LOCAL.REF,FT.L.SEX.POS>
                GOSUB GET.INTER.OPER
                BEN.NAME1 = R.FT<FT.LOCAL.REF,FT.BEN.NAME.POS,1>
                BEN.NAME2 = R.FT<FT.LOCAL.REF,FT.BEN.NAME.POS,2>
                CLNT.TYPE = R.FT<FT.LOCAL.REF,FT.L.FT.CLNT.TYPE.POS>
                PAIS = R.FT<FT.LOCAL.REF,FT.L.PAIS.POS>
                ID.PERS.BENEF = R.FT<FT.LOCAL.REF,FT.L.ID.PERS.BENEF.POS>
                PERS.BENEF = PAIS - ID.PERS.BENEF
                ACT.INT = R.FT<FT.LOCAL.REF,FT.L.ACT.INT.POS>
                PEP.INTERM = R.FT<FT.LOCAL.REF,FT.L.PEP.INTERM.POS>
                TYPE.PEP.INT = R.FT<FT.LOCAL.REF,FT.L.TYPE.PEP.INT.POS>
            END
        CASE R.STMT.ENTRY<AC.STE.SYSTEM.ID> EQ 'TT'
            TT.ID = TXN.REF:';1'
            CALL F.READ(FN.TT.HIS,TT.ID,R.TT,F.TT.HIS,TT.ERR)
            IF R.TT THEN
                INTERMEDIARY = R.TT<TT.TE.LOCAL.REF,TT.L.NEW.ID.CARD.POS>
                IF INTERMEDIARY EQ '' THEN
                    INTERMEDIARY = R.TT<TT.TE.LOCAL.REF,TT.L.OLD.ID.CARD.POS>
                END
                IF INTERMEDIARY EQ '' THEN
                    INTERMEDIARY = R.TT<TT.TE.LOCAL.REF,TT.L.PASSPORT.POS>
                END
                INTERM.NAME = R.TT<TT.TE.LOCAL.REF,TT.L.1ST.NAME.POS>:" ":R.TT<TT.TE.LOCAL.REF,TT.L.2ND.NAME.POS>
                INTERM.SURNAME = R.TT<TT.TE.LOCAL.REF,TT.LAST.NAME.POS>:" ":R.TT<TT.TE.LOCAL.REF,TT.2ND.LAST.POS>
                LOCATE ACCT.TYPE IN ACC.CODE.AFFCT.LIST<1> SETTING ACCT.TYPE.POS THEN
                    AC.TYPE.AFFECTED = R.TT<TT.TE.LOCAL.REF,L.TT.FXSN.NUM.POS>
                END
                INT.NATION = R.TT<TT.TE.LOCAL.REF,TT.L.NATIONALITY.POS>
                SEX.VAL = R.TT<TT.TE.LOCAL.REF,TT.L.SEX.POS>
                GOSUB GET.INTER.OPER

                BEN.NAME1 = R.TT<TT.TE.LOCAL.REF,TT.BEN.NAME.POS>
                BEN.NAME2 = BEN.NAME1
                CLNT.TYPE = R.TT<TT.TE.LOCAL.REF,TT.L.FT.CLNT.TYPE.POS>
                PAIS = R.TT<TT.TE.LOCAL.REF,TT.L.PAIS.POS>
                ID.PERS.BENEF = R.TT<TT.TE.LOCAL.REF,TT.L.ID.PERS.BENEF.POS>
                PERS.BENEF = PAIS - ID.PERS.BENEF
                ACT.INT = R.TT<TT.TE.LOCAL.REF,TT.L.ACT.INT.POS>
                PEP.INTERM = R.TT<TT.TE.LOCAL.REF,TT.L.PEP.INTERM.POS>
                TYPE.PEP.INT = R.TT<TT.TE.LOCAL.REF,TT.L.TYPE.PEP.INT.POS>
            END
    END CASE
    AC.AFFECT = FMT(AC.TYPE.AFFECTED,'27L')
**    AMT.TXN = FMT(R.STMT.ENTRY<AC.STE.AMOUNT.LCY>,'14L')
    CCY = FMT(R.STMT.ENTRY<AC.STE.CURRENCY>,'3L')
    INTERMEDIARY = FMT(INTERMEDIARY,'17L')
    INTERM.NAME = FMT(INTERM.NAME,'30L')
    INTERM.SURNAME = FMT(INTERM.SURNAME,'30L')
*
    VAL.DATE = R.STMT.ENTRY<AC.STE.VALUE.DATE>
    TXN.DATE = VAL.DATE[7,2]:"/":VAL.DATE[5,2]:"/":VAL.DATE[1,4]
    TXN.DATE = FMT(TXN.DATE,'10L')
    STMT.TIME = R.STMT.ENTRY<AC.STE.DATE.TIME>[7,4]
    TXN.TIME = FMT(STMT.TIME[1,2]:":":STMT.TIME[3,2],'5L')
    GOSUB GET.OTHER.TXN.ACCT
    TXN.ACCTS = FMT(ACC.TXN,'60L')
    AMT.TXN = FMT(AMT.LCY,'14L')
*   CUST.INDUS = FMT(R.CUSTOMER<EB.CUS.INDUSTRY>,'6L')
    CUST.INDUS = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.APAP.INDUS.POS>
    CUST.INDUS = FMT(CUST.INDUS,'6L')
    CUS.PEP.VAL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CUS.PEP.POS>
    CU.PERS.VAL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.PEPS.POS>
    IF CU.PERS.VAL EQ 'SI' THEN
        PER.PERSON = 'S'
    END ELSE
        PER.PERSON = 'N'
    END
    PER.PERSON = FMT(PER.PERSON,'1L')
*
    INTER.OPER = FMT(INTER.OPER,'2L')
    INT.NATION = FMT(INT.NATION,'15L')
    CLNT.TYPE = FMT(CLNT.TYPE,'2L')
    PERS.BENEF = FMT(PERS.BENEF,'17L')
    BEN.NAME1 = FMT(BEN.NAME1,'50L')
    BEN.NAME2 = FMT(BEN.NAME2,'30L')
    PAIS = FMT(PAIS,'15L')
    CUS.PEP = FMT(CUS.PEP.VAL,'1L')
    ACT.INT = FMT(ACT.INT,'6L')
    PEP.INTERM = FMT(PEP.INTERM,'1L')
    TYPE.PEP.INT = FMT(TYPE.PEP.INT,'1L')
*
    REP.LINE = CUST.TYPE:CUST.CODE:CUST.NAME:INITIALS:NATION:TYPE.OF.OPER:AC.AFFECT:ACC.STATUS:AMT.TXN:CCY:INTER.OPER:INTERMEDIARY:INTERM.NAME:INTERM.SURNAME:INT.NATION:CLNT.TYPE:PERS.BENEF:BEN.NAME1:BEN.NAME2:PAIS:TXN.DATE:TXN.TIME:TXN.ACCTS:CUST.INDUS:PER.PERSON:CUS.PEP:ACT.INT:PEP.INTERM:TYPE.PEP.INT
*
    CALL F.WRITE(FN.DR.REG.213IF01.WORKFILE, CUST.ID, REP.LINE)
*
RETURN
*-----------------------------------------------------------------------------------
GET.INTER.OPER:
***************
*
    BEGIN CASE
        CASE INT.NATION EQ 'DO' AND SEX.VAL EQ 'MALE'
            INTER.OPER = 'P3'
        CASE INT.NATION NE 'DO' AND R.CUSTOMER<EB.CUS.RESIDENCE> EQ 'DO'
            INTER.OPER = 'P4'
        CASE INT.NATION EQ 'DO' AND SEX.VAL EQ 'FEMALE'
            INTER.OPER = 'P5'
        CASE INT.NATION NE 'DO' AND SEX.VAL EQ 'FEMALE'
            INTER.OPER = 'P6'
        CASE INT.NATION NE 'DO' AND R.CUSTOMER<EB.CUS.RESIDENCE> NE 'DO' AND SEX.VAL EQ 'MALE'
            INTER.OPER = 'P7'
        CASE INT.NATION NE 'DO' AND R.CUSTOMER<EB.CUS.RESIDENCE> NE 'DO' AND SEX.VAL EQ 'FEMALE'
            INTER.OPER = 'P8'
    END CASE
*
RETURN
*-----------------------------------------------------------------------------------
GET.OTHER.TXN.ACCT:
*----------------*
*
    AMT.LCY = 0
    ACC.TXN = ''
*
    CNT.TXN = DCOUNT(R.DR.REG.213IF01.CONCAT<DR.213IF01.CONCAT.STMT.ID>,@VM)
    CTR.TXN = 1
    LOOP
    WHILE CTR.TXN LE CNT.TXN
        STMT.VAL = R.DR.REG.213IF01.CONCAT<DR.213IF01.CONCAT.STMT.ID,CTR.TXN>
        R.STMT.ENTRY1 = ''
        CALL F.READ(FN.STMT.ENTRY,STMT.VAL,R.STMT.ENTRY1,F.STMT.ENTRY,STMT.ENTRY.ERR)
        AMT.LCY += R.STMT.ENTRY1<AC.STE.AMOUNT.LCY>
        LOCATE R.STMT.ENTRY1<AC.STE.ACCOUNT.NUMBER> IN ACC.TXN<1> SETTING AC.POS ELSE
            ACC.TXN<-1> = R.STMT.ENTRY1<AC.STE.ACCOUNT.NUMBER>
        END
        CTR.TXN += 1
    REPEAT
*
    CHANGE @FM TO ' ' IN ACC.TXN
*
RETURN
*-----------------------------------------------------------------------------------
GET.ACC.STATUS:
*------------*
*
    AC.STATUS1.VAL = R.ACCOUNT<AC.LOCAL.REF,AC.STATUS1.POS>
    AC.STATUS2.VAL = R.ACCOUNT<AC.LOCAL.REF,AC.STATUS2.POS>
*
    BEGIN CASE
        CASE AC.STATUS1.VAL EQ 'ACTIVA' AND AC.STATUS2.VAL EQ ''
            ACC.STATUS = 'A'
        CASE AC.STATUS1.VAL[1,8] EQ 'INACTIVA'
            ACC.STATUS = 'I'
        CASE AC.STATUS2.VAL EQ 'GARANTIA'
            ACC.STATUS = 'G'
        CASE AC.STATUS2.VAL EQ 'EMBARGO'
            ACC.STATUS = 'E'
        CASE AC.STATUS2.VAL EQ 'FALLECIDO'
            ACC.STATUS = 'R'
    END CASE
*
RETURN
*-----------------------------------------------------------------------------------
TYPE.OF.OPERATION:
*----------------*
*
    BEGIN CASE
        CASE TYPE.OF.OPER EQ '21'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '10'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '14'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '15'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '81'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '82'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '135'
            TYPE.OPER = '1'
        CASE TYPE.OF.OPER EQ '12'
            TYPE.OPER = '3'
        CASE TYPE.OF.OPER EQ '24'
            TYPE.OPER = '5'
        CASE TYPE.OF.OPER EQ '69'
            TYPE.OPER = '5'
        CASE TYPE.OF.OPER EQ '23'
            TYPE.OPER = '6'
        CASE TYPE.OF.OPER EQ '25'
            TYPE.OPER = '6'
        CASE TYPE.OF.OPER EQ '39'
            TYPE.OPER = '6'
        CASE TYPE.OF.OPER EQ '16'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '17'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '20'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '32'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '33'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '301'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '303'
            TYPE.OPER = '7'
        CASE TYPE.OF.OPER EQ '6'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '9'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '160'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '161'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '170'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '175'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '180'
            TYPE.OPER = '8'
        CASE TYPE.OF.OPER EQ '6'
            TYPE.OPER = '9'
        CASE TYPE.OF.OPER EQ '27'
            TYPE.OPER = '9'
        CASE TYPE.OF.OPER EQ '7'
            TYPE.OPER = '11'
        CASE TYPE.OF.OPER EQ '4'
            TYPE.OPER = '12'
        CASE TYPE.OF.OPER EQ '5'
            TYPE.OPER = '12'
    END CASE
*
RETURN
*-----------------------------------------------------------------------------------
GET.CUSTOMER.DETAILS:
*-------------------*
*
    CUSTOMER.TYPE = R.CUSTOMER
    CALL APAP.LAPAP.drReg213if02GetCustType(CUSTOMER.TYPE,TIPO.CL.POS);*R22 MANUAL CODE CONVERSION
    
    CUST.TYPE = FMT(CUSTOMER.TYPE,'2L')
    CUSTOMER.CODE = R.CUSTOMER
*CALL APAP.DRREG.DR.REG.213IF02.GET.CUST.CODE(CUSTOMER.CODE,CIDENT.POS,RNC.POS)
    CALL APAP.DRREG.drReg213if02GetCustCode(CUSTOMER.CODE,CIDENT.POS,RNC.POS) ;*R22 MANUAL CODE CONVERSION
    CUST.CODE = FMT(CUSTOMER.CODE,'17L')
    GOSUB GET.NAMES
    CUST.NAME = FMT(NAMES,'50L')
    GOSUB GET.NAME.INITIALS
    INITIALS = FMT(NAME.INITIALS,'30L')
    NATION = FMT(R.CUSTOMER<EB.CUS.NATIONALITY>,'15L')
*
RETURN
*-----------------------------------------------------------------------------------
GET.NAME.INITIALS:
*----------------*
*
    BEGIN CASE
        CASE TIPO.CL.POS.VAL EQ "PERSONA FISICA" OR "CLIENTE MENOR"
            NAME.INITIALS = R.CUSTOMER<EB.CUS.FAMILY.NAME>
        CASE TIPO.CL.POS.VAL EQ "PERSONA JURIDICA"
            NAME.INITIALS = R.CUSTOMER<EB.CUS.SHORT.NAME,1>
    END CASE
*
RETURN
*-----------------------------------------------------------------------------------
GET.NAMES:
*--------*
*
    BEGIN CASE
        CASE TIPO.CL.POS.VAL EQ "PERSONA FISICA" OR "CLIENTE MENOR"
*        NAMES = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
            NAMES = R.CUSTOMER<EB.CUS.GIVEN.NAMES>
        CASE TIPO.CL.POS.VAL EQ "PERSONA JURIDICA"
            NAMES = R.CUSTOMER<EB.CUS.NAME.1,1>:' ':R.CUSTOMER<EB.CUS.NAME.2,1>
    END CASE

RETURN
*-----------------------------------------------------------------------------------
END
