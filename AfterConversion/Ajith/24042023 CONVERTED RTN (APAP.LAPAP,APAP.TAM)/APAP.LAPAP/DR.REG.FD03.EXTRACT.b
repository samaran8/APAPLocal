* @ValidationCode : MjotMjE2MTQ5MzM5OkNwMTI1MjoxNjgyMzIzMDQyNTg4OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 24 Apr 2023 13:27:22
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
$PACKAGE APAP.LAPAP
SUBROUTINE DR.REG.FD03.EXTRACT(REC.ID)
*-----------------------------------------------------------------------------
* Company Name   : APAP
* Developed By   :
* Program Name   : DR.REG.FD03.EXTRACT
* Date           : 2-May-2013
*-----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the Customer Details for
*-----------------------------------------------------------------------------
*
* Modification History :
* ----------------------
*   Date       Author              Modification Description
* 02-Aug-2014  Ashokkumar.V.P      PACS00316981 - Corrected the fields based on mapping.
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*24-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   T24.BP ,REGREP.BP ,LAPAP.BP  is removed ,$INCLUDE to $INSERT
*24-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------


*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_TSA.COMMON
    $INSERT I_F.DATES
    $INSERT I_F.COMPANY
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.RELATION
    $INSERT I_DR.REG.FD03.EXTRACT.COMMON
    $INSERT I_F.DR.REG.FD03.PARAM
    $INSERT I_F.DR.REG.FD03.CONCAT

  
    GOSUB PROCESS
RETURN

PROCESS:
********
    R.DR.REG.FD03.CONCAT = ''; DR.REG.FD03.CONCAT.ERR = ''
    CALL F.READ(FN.DR.REG.FD03.CONCAT,REC.ID,R.DR.REG.FD03.CONCAT,F.DR.REG.FD03.CONCAT,DR.REG.FD03.CONCAT.ERR)
    CNT.REC = DCOUNT(R.DR.REG.FD03.CONCAT,@FM)
    CTR.REC = 1
    LOOP
    WHILE CTR.REC LE CNT.REC
        STMT.TXN.TYPE = R.DR.REG.FD03.CONCAT<CTR.REC>
        ERR.FT.HIS = ''; R.TXN = ''; YTXN.ID = ''
        YTXN.ID = STMT.TXN.TYPE
        CALL EB.READ.HISTORY.REC(F.FT.HIS,STMT.TXN.TYPE,R.TXN,ERR.FT.HIS)
        TXN.CODE =  R.TXN<FT.TRANSACTION.TYPE>
        YDB.CCY =  R.TXN<FT.DEBIT.CURRENCY>
        YCR.CCY =  R.TXN<FT.CREDIT.CURRENCY>

        IF (YDB.CCY EQ LCCY AND YCR.CCY NE LCCY) OR (YDB.CCY NE LCCY AND YCR.CCY EQ LCCY) OR (YDB.CCY NE LCCY AND YCR.CCY NE LCCY) THEN
            R.FT.TXN.TYPE.CONDITION = ''; FT.TXN.TYPE.CONDITION.ERR = ''; L.FTTC.PAY.TYPE.VAL = ''
            CALL F.READ(FN.FT.TXN.TYPE.CONDITION,TXN.CODE,R.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION,FT.TXN.TYPE.CONDITION.ERR)
            L.FTTC.PAY.TYPE.VAL = R.FT.TXN.TYPE.CONDITION<FT6.LOCAL.REF,L.FTTC.PAY.TYPE.POS>
            IF L.FTTC.PAY.TYPE.VAL EQ 6 THEN
                GOSUB GET.TYPE6.FLDS1.7
                GOSUB GET.TYPE6.FLDS8.16
                GOSUB GET.TYPE6.FLDS17.25
            END ELSE
                GOSUB GET.TYPE10.FLDS1.7
                GOSUB GET.TYPE10.FLDS8.16
                GOSUB GET.TYPE10.FLDS17.25
            END
            GOSUB TYPE.WRITE.FILE
        END
        CTR.REC += 1
    REPEAT
RETURN

GET.TYPE6.FLDS1.7:
******************
    FLD1 = FMT('E','L#1')
    FLD2 = FMT('4010000131','L#10')
    FLD3 = FMT("ASOC. POPULAR AHORROS Y PRESTAMOS (APAP)",'L#40')
    FLD4 = FMT(R.TXN<FT.LOCAL.REF,L.FT.ACH.B.ACC.POS>,'L#40')
    GOSUB GET.CUST.ID
    FLD5 = FMT(CUST.ID.VAL,'L#15')
    GOSUB GET.CUS.NAME
    FLD6 = FMT(CUS.NAME,'L#60')
    FLD7 = FMT(R.TXN<FT.LOCAL.REF,L.PAIS.POS>,'L#2')
RETURN
*-----------------------------------------------------------------------------------
GET.TYPE6.FLDS8.16:
******************
    FLD8 = FMT(R.TXN<FT.LOCAL.REF,L.ESTADO.POS>,'L#7')
    FLD9 = FMT(R.TXN<FT.DEBIT.AMOUNT>,'R2#14')
    FLD10 = FMT(R.TXN<FT.DEBIT.CURRENCY>,'L#3')
    FLD11 = FMT(R.DR.REG.FD03.CONCAT<CTR.REC>,'L#15')
    GOSUB DATE.FORMAT
    FLD12 = FMT(PROCESS.DATE,'L#10')
    FLD13 = FMT(R.TXN<FT.LOCAL.REF,L.FT.CLNT.TYPE.POS>,'L#2')
    FLD14 = FMT(R.TXN<FT.LOCAL.REF,L.PAIS.POS>,'L#15')
    FLD15 = FMT(R.TXN<FT.LOCAL.REF,BENEFIC.NAME.POS>,'L#60')
    FLD16 = FMT(R.TXN<FT.CREDIT.CURRENCY>,'L#3')
RETURN
*-----------------------------------------------------------------------------------
GET.TYPE6.FLDS17.25:
********************
    GOSUB GET.COMP.NAME
    FLD17 = FMT(COMP.NAME,'L#6')
    FLD18 = FMT(R.TXN<FT.LOCAL.REF,L.PROPOSITO.POS>,'L#2')
    FLD19 = FMT(R.TXN<FT.LOCAL.REF,L.FT.PARTY.NAME.POS>,'L#30')
    FLD20 = FMT(R.TXN<FT.LOCAL.REF,L.CR.FACILITY.POS>,'R#3')
    FLD21 = FMT('','L#27')
    TOT.CH.AMT = R.TXN<FT.TOTAL.CHARGE.AMOUNT>
    TOT.CH.AMT.LEN = LEN(TOT.CH.AMT)
    FLD22 = FMT(TOT.CH.AMT[4,TOT.CH.AMT.LEN],'R2#14')
    FLD23 = FMT('','R2#14')
    FLD24 = FMT('','R2#14')
    FLD25 = FMT(TOT.CH.AMT[1,3],'L#3')
*
RETURN
*-----------------------------------------------------------------------------------
GET.TYPE10.FLDS1.7:
*******************
*
    FLD1 = FMT('R','L#1')
    FLD2 = FMT(R.TXN<FT.LOCAL.REF,L.ID.PERS.ORD.POS>,'L#10')
    FLD3 = FMT(R.TXN<FT.ORDERING.BANK>,'L#40')
    FLD4 = FMT(R.TXN<FT.LOCAL.REF,L.FT.ACH.B.ACC.POS>,'L#40')
    FLD5 = FMT(R.TXN<FT.LOCAL.REF,L.ID.PERS.ORD.POS>,'L#15')
    DB.AC = R.TXN<FT.DEBIT.ACCT.NO>
    GOSUB READ.ACCOUNT
    CUSTOMER.ID = R.ACCOUNT<AC.CUSTOMER>
    GOSUB READ.CUSTOMER
    GOSUB GET.CUS.NAME
    FLD6 = FMT(CUS.NAME,'L#60')
    FLD7 = FMT(R.TXN<FT.LOCAL.REF,L.PAIS.POS>,'L#2')
*
RETURN
*-----------------------------------------------------------------------------------
GET.TYPE10.FLDS8.16:
********************
*
    FLD8 = FMT(R.TXN<FT.LOCAL.REF,L.ESTADO.POS>,'L#7')
    FLD9 = FMT(R.TXN<FT.DEBIT.AMOUNT>,'R2#14')
    FLD10 = FMT(R.TXN<FT.DEBIT.CURRENCY>,'L#3')
    FLD11 = FMT(R.DR.REG.FD03.CONCAT<CTR.REC>,'L#15')
    GOSUB DATE.FORMAT
    FLD12 = FMT(PROCESS.DATE,'L#10')
    FLD13 = FMT(R.TXN<FT.LOCAL.REF,L.FT.CLNT.TYPE.POS>,'L#2')
    FLD14 = FMT(R.TXN<FT.LOCAL.REF,L.ID.PERS.BENEF.POS>,'L#15')
    FLD15 = FMT(R.TXN<FT.LOCAL.REF,BENEFIC.NAME.POS>,'L#60')
    FLD16 = FMT(R.TXN<FT.CREDIT.CURRENCY>,'L#3')
RETURN

GET.TYPE10.FLDS17.25:
*********************
*
    TOT.CH.AMT = ''
    GOSUB GET.COMP.NAME
    FLD17 = FMT(COMP.NAME,'L#6')
    FLD18 = FMT(R.TXN<FT.LOCAL.REF,L.PROPOSITO.POS>,'L#2')
    FLD19 = FMT(R.TXN<FT.LOCAL.REF,L.FT.PARTY.NAME.POS>,'L#30')
    FLD20 = FMT(R.TXN<FT.LOCAL.REF,L.CR.FACILITY.POS>,'R#3')
    FLD21 = FMT(R.TXN<FT.LOCAL.REF,L.FT.CR.CARD.NO.POS>,'L#27')
    FLD22 = FMT('','R2#14')
    FLD23 = FMT('','R2#14')
    FLD24 = FMT('','R2#14')
    TOT.CH.AMT = R.TXN<FT.TOTAL.CHARGE.AMOUNT>
    FLD25 = FMT(TOT.CH.AMT[1,3],'L#3')
RETURN

TYPE.WRITE.FILE:
***************
    R.DR.REG.FD03.WORKFILE = ''
    R.DR.REG.FD03.WORKFILE = FLD1:FLD2:FLD3:FLD4:FLD5:FLD6:FLD7:FLD8:FLD9:FLD10:FLD11:FLD12:FLD13:FLD14:FLD15:FLD16:FLD17:FLD18:FLD19:FLD20:FLD21:FLD22:FLD23:FLD24:FLD25
    CALL F.WRITE(FN.DR.REG.FD03.WORKFILE,YTXN.ID,R.DR.REG.FD03.WORKFILE)
    FLD1 = ''; FLD2 = ''; FLD3 = ''; FLD4 = ''; FLD5 = ''; FLD6 = ''; FLD7 = ''
    FLD8 = ''; FLD9 = ''; FLD10 = ''; FLD11 = ''; FLD12 = ''; FLD13 = ''; FLD14 = ''
    FLD15 = ''; FLD16 = ''; FLD17 = ''; FLD18 = ''; FLD19 = ''; FLD20 = ''; FLD21 = ''
    FLD22 = ''; FLD23 = ''; FLD24 = ''; FLD25 = ''
RETURN

GET.COMP.NAME:
**************
    COMP.NAME = ''
    IF L.FTTC.PAY.TYPE.VAL EQ 6 THEN
        IF R.CUSTOMER THEN
            COMP.NAME = R.CUSTOMER<EB.CUS.LOCAL.REF,L.LOCALIDAD.POS>
        END
    END
*
    IF L.FTTC.PAY.TYPE.VAL EQ 10 THEN
        CR.AC = R.TXN<FT.CREDIT.ACCT.NO>
        CALL F.READ(FN.ACCOUNT,CR.AC,R.ACCT,F.ACCOUNT,ACCOUNT.ERR)
        IF NOT(R.ACCT) THEN
            COMP.NAME = ''
            RETURN
        END
        CUSTOMER.ID = R.ACCT<AC.CUSTOMER>
        GOSUB READ.CUSTOMER
        IF R.CUSTOMER THEN
            COMP.NAME = R.CUSTOMER<EB.CUS.LOCAL.REF,L.LOCALIDAD.POS>
        END
    END
RETURN

GET.CUS.NAME:
*************
    L.CU.TIPO.CL.VAL = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.TIPO.CL.POS>
    Y.CUS.NAME = ''
    BEGIN CASE
        CASE L.CU.TIPO.CL.VAL EQ 'PERSONA FISICA'
            CUS.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
        CASE L.CU.TIPO.CL.VAL EQ 'CLIENTE MENOR'
            CUS.NAME = R.CUSTOMER<EB.CUS.GIVEN.NAMES>:' ':R.CUSTOMER<EB.CUS.FAMILY.NAME>
        CASE L.CU.TIPO.CL.VAL EQ 'PERSONA JURIDICA'
            CUS.NAME = R.CUSTOMER<EB.CUS.NAME.1,1>:' ':R.CUSTOMER<EB.CUS.NAME.2,1>
    END CASE
RETURN

READ.CUSTOMER:
**************
    R.CUSTOMER  = '';    CUSTOMER.ER = ''
    CALL F.READ(FN.CUSTOMER,CUSTOMER.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ER)
RETURN

READ.ACCOUNT:
*************
    R.ACCOUNT = ''; ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,DB.AC,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
RETURN

GET.CUST.ID:
************
    R.ACCOUNT = ''; ACCOUNT.ERR = ''; R.CUSTOMER = ''
    CUSTOMER.ERR = ''; OUT.ARR = ''
    DB.AC = R.TXN<FT.DEBIT.ACCT.NO>
    GOSUB READ.ACCOUNT
    CUSTOMER.ID = R.ACCOUNT<AC.CUSTOMER>
    GOSUB READ.CUSTOMER
    CALL DR.REG.GET.CUST.TYPE(R.CUSTOMER,OUT.ARR)
    CUST.ID.VAL = OUT.ARR<2>
RETURN

DATE.FORMAT:
************
    PROCESS.DATE = R.TXN<FT.PROCESSING.DATE>
    PROCESS.DATE = PROCESS.DATE[7,2]:"/":PROCESS.DATE[5,2]:"/":PROCESS.DATE[1,4]
RETURN
END
