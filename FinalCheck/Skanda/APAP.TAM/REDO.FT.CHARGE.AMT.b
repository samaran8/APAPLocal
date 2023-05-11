* @ValidationCode : MjoxMjg1Nzk2ODg3OkNwMTI1MjoxNjgwNzc3Njg0Njc3OjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 16:11:24
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
SUBROUTINE  REDO.FT.CHARGE.AMT
*--------------------------------------------------------------------------
*Company Name      : APAP Bank
*Developed By      : Temenos Application Management
*Program Name      : REDO.FT.CHARGE.AMT
*Date              : 09.12.2010
*Description       : This routine is to calculate the amount and charges and currency conversion
*This wil be attached to All version involving AC.LOCKED.EENTS
*-------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : --N/A--
*-----------------------------------------------------------------------------
* Revision History:
* -----------------
* Date            Name                        Reference             Version
* -------         ----                        ----------            --------
* 09/12/2010      saktharrasool@temenos.com   ODR-2010-08-0469      Initial Version
* 01-10-2011      Balagurunathan.B            PACS00121355          Recompiled the routine to make fucntions work
* 12-10-2018      Vignesh Kumaar M R          2795723               BRD002 DECIMAL POSITIONS BY COUNTRY
* 22-01-2019      Vignesh Kumaar M R          2795723               BRD003 UNARED
* 26-11-2019      Vignesh Kumaar M R          3334597               MERCHANDISE RETURN Y CREDIT VOUCHER
*-----------------------------------------------------------------------------------
* Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*06/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION            F.READ TO CACHE.READ,F.FT.CHARGE.TYPE TO R.FT.CHARGE.TYPE,VM TO @VM, = TO EQ, FM TO @FM
*06/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------------

*------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AT.ISO.COMMON
    $INSERT I_F.COMPANY
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.REDO.APAP.H.PARAMETER
    $INSERT I_F.NUMERIC.CURRENCY
    $INSERT I_ATM.BAL.ENQ.COMMON ;*AUTO R22 CODE CONVERSION
    $INSERT I_F.REDO.ATM.WAIVE.CHARGE   ;* Fix for 2795723 [BRD003 UNARED]
    $INSERT I_F.COUNTRY
    $INSERT I_F.CURRENCY
    $INSERT I_F.REDO.CAPTURE.CREDIT.VOUCHER ;*AUTO R22 CODE CONVERSION

    IF V$FUNCTION EQ 'R' THEN
        RETURN
    END

    GOSUB OPEN.FILES
    GOSUB PROCESS


RETURN


*------------------------------------------------------------------------------------
OPEN.FILES:
*------------------------------------------------------------------------------------

    FN.FT.TXN.TYPE.CONDITION='F.FT.TXN.TYPE.CONDITION'
    F.FT.TXN.TYPE.CONDITION=''
    CALL OPF(FN.FT.TXN.TYPE.CONDITION,F.FT.TXN.TYPE.CONDITION)

    FN.FT.CHARGE.TYPE='F.FT.CHARGE.TYPE'
    F.FT.CHARGE.TYPE=''
    CALL OPF(FN.FT.CHARGE.TYPE,F.FT.CHARGE.TYPE)
    FN.COUNTRY='F.COUNTRY'

* Fix for 2795723 [BRD003 UNARED #1]

    FN.REDO.ATM.WAIVE.CHARGE = 'F.REDO.ATM.WAIVE.CHARGE'
    F.REDO.ATM.WAIVE.CHARGE  = ''
    CALL OPF(FN.REDO.ATM.WAIVE.CHARGE,F.REDO.ATM.WAIVE.CHARGE)

* End of Fix

    FN.REDO.APAP.H.PARAMETER='F.REDO.APAP.H.PARAMETER'

    CALL CACHE.READ(FN.REDO.APAP.H.PARAMETER,'SYSTEM',R.REDO.APAP.H.PARAMETER,ERR.PARAM)
    CCY.MARKET=R.REDO.APAP.H.PARAMETER<PARAM.CURRENCY.MARKET>

* This value will be used to calculate the excess amount to be summed with transaction amount

    Y.EXCESS.PER=R.REDO.APAP.H.PARAMETER<PARAM.ATM.EXCESS.PER>

    IF Y.EXCESS.PER NE '' AND Y.EXCESS.PER NE 0 THEN
        Y.EXCESS.PER=Y.EXCESS.PER/100
    END ELSE
        Y.EXCESS.PER=0
    END
***
    CHG.AMT.TOT=0


RETURN

*------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------
*READING

    AT.ISO=AT$INCOMING.ISO.REQ(19)
    LOC.REF.APPLICATION="AC.LOCKED.EVENTS":@FM:'COUNTRY'
    LOC.REF.FIELDS='L.CHARGE.AMT':@VM:'L.TXN.AMT':@VM:'L.FTTC.ID':@VM:'L.TXN.AMT.LOCAL':@VM:'L.TXN.CCY.CODE':@VM:'AT.UNIQUE.ID':@FM:'L.COUNTRY.CODE'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    CHG.AMT.POS=LOC.REF.POS<1,1>
    TXN.AMT.POS=LOC.REF.POS<1,2>
    FTTC.ID.POS=LOC.REF.POS<1,3>
    TXN.LOC.POS=LOC.REF.POS<1,4>
    TXN.CCY.CDE.POS=LOC.REF.POS<1,5>
    TXN.UNIQUE.ID.POS=LOC.REF.POS<1,6>
    COUNTRY.CDE.POS=LOC.REF.POS<2,1>

    IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
        Y.TXN.AMT=R.NEW(AC.LCK.LOCAL.REF)<1,TXN.AMT.POS>
        Y.FTTC.ID=R.NEW(AC.LCK.LOCAL.REF)<1,FTTC.ID.POS>
        Y.TXN.CCY.CDE=R.NEW(AC.LCK.LOCAL.REF)<1,TXN.CCY.CDE.POS>
    END ELSE
        Y.TXN.AMT=R.NEW(ATM.CRED.TXN.AMOUNT)
        Y.FTTC.ID=R.NEW(ATM.CRED.FTTC.ID)
        Y.TXN.CCY.CDE=R.NEW(ATM.CRED.TXN.CCY.CODE)
    END

    CALL CACHE.READ(FN.FT.TXN.TYPE.CONDITION, Y.FTTC.ID, R.FT.TXN.TYPE.CONDITION, FT.ERR) ;*AUTO R22 CODE CONVERSION
    TXN.COUNTRY=AT$INCOMING.ISO.REQ(19)
    APAP.COUNTRY=R.COMPANY(EB.COM.LOCAL.COUNTRY)
    CALL CACHE.READ(FN.COUNTRY,APAP.COUNTRY,R.COUNTRY,COUNTRY.ERR)
    APAP.COUNTRY.CDE=R.COUNTRY<EB.COU.LOCAL.REF><1,COUNTRY.CDE.POS>

* Fix for 2795723 [BRD003 UNARED #2]

    GET.TERMINAL.ID  = TRIM(AT$INCOMING.ISO.REQ(41))
    GET.TRANS.SOURCE = AT$INCOMING.ISO.REQ(32)
    ATM.WAIVE = GET.TERMINAL.ID:'-':GET.TRANS.SOURCE
    CALL F.READ(FN.REDO.ATM.WAIVE.CHARGE,ATM.WAIVE,R.REDO.ATM.WAIVE.CHARGE,F.REDO.ATM.WAIVE.CHARGE,ERR.REDO.ATM.WAIVE.CHARGE)

    IF NOT(R.REDO.ATM.WAIVE.CHARGE) THEN
        ERR.REDO.ATM.WAIVE.CHARGE = ''
        ATM.WAIVE = GET.TERMINAL.ID[1,4]:'*-':GET.TRANS.SOURCE
        CALL F.READ(FN.REDO.ATM.WAIVE.CHARGE,ATM.WAIVE,R.REDO.ATM.WAIVE.CHARGE,F.REDO.ATM.WAIVE.CHARGE,ERR.REDO.ATM.WAIVE.CHARGE)
    END

    IF R.FT.TXN.TYPE.CONDITION AND ERR.REDO.ATM.WAIVE.CHARGE THEN

* End of Fix

        Y.FT.TXN.CHRG=R.FT.TXN.TYPE.CONDITION<FT6.CHARGE.TYPES>

        Y.FT.TXN.CHRG.CNT=DCOUNT(R.FT.TXN.TYPE.CONDITION<FT6.CHARGE.TYPES>,@VM)
        T.DATAA=''
        LOOP
            REMOVE Y.FT.CHRG FROM Y.FT.TXN.CHRG SETTING POS.CHRG

        WHILE Y.FT.CHRG:POS.CHRG

*FT5.CURRENCY TO 7,            FT5.FLAT.AMT
            T.DATAA<1,-1>=Y.FT.CHRG
            T.DATAA<2,-1>='CHG'

        REPEAT

        Y.FT.COMM=R.FT.TXN.TYPE.CONDITION<FT6.COMM.TYPES>
        Y.FT.COMM.CNT=DCOUNT(R.FT.TXN.TYPE.CONDITION<FT6.COMM.TYPES>,@VM)

        LOOP
            REMOVE Y.FT.COM FROM Y.FT.TXN.CHRG SETTING POS.CHRG

        WHILE Y.FT.CHRG:POS.CHRG

*FT5.CURRENCY TO 7,            FT5.FLAT.AMT
            T.DATAA<1,-1>=Y.FT.COM
            T.DATAA<2,-1>='COM'

        REPEAT
    END


    FN.NUMERIC.CURRENCY='F.NUMERIC.CURRENCY'

    CALL  CACHE.READ(FN.NUMERIC.CURRENCY,Y.TXN.CCY.CDE,R.NUMERIC.CURRENCY,ERR.CCY)

    BUY.CCY=R.NUMERIC.CURRENCY< EB.NCN.CURRENCY.CODE>

    IF BUY.CCY EQ LCCY THEN
        TXN.COUNTRY=APAP.COUNTRY.CDE
    END

    IF BUY.CCY EQ '' THEN
        BUY.CCY=LCCY
    END

    FN.CURRENCY='F.CURRENCY'
    CALL CACHE.READ(FN.CURRENCY,BUY.CCY,R.CURRENCY,CCY.ERR)
    CURR.DEC = R.CURRENCY<EB.CUR.NO.OF.DECIMALS>
    Y.TXN.AMT = FIELD(Y.TXN.AMT,'.',1)
    DIV.VAL = PWR(10,CURR.DEC)
    Y.TXN.AMT = Y.TXN.AMT/DIV.VAL

    IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
        R.NEW(AC.LCK.LOCAL.REF)<1,TXN.AMT.POS>=Y.TXN.AMT
    END ELSE
        R.NEW(ATM.CRED.TXN.AMOUNT) = Y.TXN.AMT
    END

    GOSUB GET.TXN.CHRG

    IF TXN.COUNTRY NE APAP.COUNTRY.CDE AND TXN.COUNTRY NE '' THEN

*Modified to use the conversion rate from ISO message

        EXH.RATE=AT$INCOMING.ISO.REQ(10)
        GOSUB PROCESS.APAP.COUNTRY.CODE

    END ELSE
        CALL EB.ROUND.AMOUNT('DOP',Y.TXN.AMT,'','')
        IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
            R.NEW(AC.LCK.LOCAL.REF)<1,TXN.LOC.POS> =Y.TXN.AMT
        END ELSE
            R.NEW(ATM.CRED.TXN.AMTLOC) = Y.TXN.AMT
        END
        Y.TXN.LOC.AMT=CHG.AMT.TOT + Y.TXN.AMT
        CALL EB.ROUND.AMOUNT('DOP',Y.TXN.LOC.AMT,'','')

        IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
            R.NEW(AC.LCK.LOCKED.AMOUNT)=Y.TXN.LOC.AMT
        END ELSE
            R.NEW(ATM.CRED.TXN.AMTLOC) = Y.TXN.LOC.AMT
        END

        BEGIN CASE
            CASE AT$INCOMING.ISO.REQ(3)[1,2] EQ '01'AND (AT$INCOMING.ISO.REQ(32) EQ '04' OR AT$INCOMING.ISO.REQ(32) EQ '4')
                TXN.SOURCE='LOC VISA ATM'
            CASE AT$INCOMING.ISO.REQ(3)[1,2] EQ '01'AND (AT$INCOMING.ISO.REQ(32) EQ '03' OR AT$INCOMING.ISO.REQ(32) EQ '3')
                TXN.SOURCE='LOC ATH ATM'
            CASE (AT$INCOMING.ISO.REQ(3)[1,2] EQ '00' OR AT$INCOMING.ISO.REQ(3)[1,2] EQ '14' OR AT$INCOMING.ISO.REQ(3)[1,2] EQ '09' ) AND (AT$INCOMING.ISO.REQ(32) EQ '03' OR AT$INCOMING.ISO.REQ(32) EQ '3')
                TXN.SOURCE='LOC ATH POS'
            CASE OTHERWISE
                TXN.SOURCE='LOC VISA POS'
        END CASE
    END

    IF CHG.AMT.TOT NE 0 THEN
        IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
            R.NEW(AC.LCK.LOCAL.REF)<1,CHG.AMT.POS>=CHG.AMT.TOT
        END
    END

RETURN

*------------------------*
PROCESS.APAP.COUNTRY.CODE:
*------------------------*

    IF EXH.RATE EQ '' OR EXH.RATE EQ 0 THEN
        EXH.RATE=1
    END ELSE

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
        EXH.RATE= Y.CONV.RATE
    END

*included excess amount to be added to lock

* Fix provided for the Enhancement# 2795723 [BRD002 DECIMAL POSITIONS BY COUNTRY]

*       LCCY.AMT= Y.TXN.AMT*EXH.RATE
    LCCY.AMT = AT$INCOMING.ISO.REQ(6)   ;* Added for CR
    Y.LCCY.LEN = LEN(LCCY.AMT)
    LCCY.AMT = LCCY.AMT[1,(Y.LCCY.LEN)-2]:'.':LCCY.AMT[(Y.LCCY.LEN-1),2]
    LCCY.AMT = TRIM(LCCY.AMT,"0","L")

* End of Fix

    LCCY.AMT= LCCY.AMT + (LCCY.AMT * Y.EXCESS.PER)

    CALL EB.ROUND.AMOUNT('DOP',LCCY.AMT,'','')

    IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
        R.NEW(AC.LCK.LOCAL.REF)<1,TXN.LOC.POS> =LCCY.AMT
    END ELSE
        R.NEW(ATM.CRED.TXN.AMTLOC) = LCCY.AMT
    END

    CHG.AMT.TOT=CHG.AMT.TOT + (CHG.AMT.TOT*Y.EXCESS.PER)

    Y.TXN.LOC.AMT=CHG.AMT.TOT + LCCY.AMT

    CALL EB.ROUND.AMOUNT('DOP',Y.TXN.LOC.AMT,'','')

    IF APPLICATION EQ 'AC.LOCKED.EVENTS' THEN
        R.NEW(AC.LCK.LOCKED.AMOUNT)=Y.TXN.LOC.AMT
    END ELSE
        R.NEW(ATM.CRED.TXN.AMTLOC) = Y.TXN.LOC.AMT
    END

* To Identify the source of transaction

    IF AT$INCOMING.ISO.REQ(3)[1,2] EQ '01' THEN
        TXN.SOURCE='INTL ATM'
    END ELSE
        TXN.SOURCE='INTL POS'
    END

RETURN

*-----------------------------------------------------------------------------------------------------------------------
GET.TXN.CHRG:
*-----------------------------------------------------------------------------------------------------------------------

    CALL CACHE.READ(FN.FT.CHARGE.TYPE, Y.FT.CHRG, R.FT.CHARGE.TYPE, FT.ERR) ;*AUTO R22 CODE CONVERSION


    IF TXN.COUNTRY NE APAP.COUNTRY.CDE THEN


        DEAL.CURRENCY='USD'
        DEAL.AMOUNT=  Y.TXN.AMT


        CALL CALCULATE.CHARGE(CUSTOMER, DEAL.AMOUNT, DEAL.CURRENCY, CCY.MARKET,CROSS.RATE, CROSS.CURRENCY, DRAWDOWN.CCY, T.DATAA, CUST.COND, CHG.AMT.TOT, TOT.CHARGE.FCCY)


    END ELSE


        DEAL.CURRENCY='DOP'
        DEAL.AMOUNT=  Y.TXN.AMT


        CALL CALCULATE.CHARGE(CUSTOMER, DEAL.AMOUNT, DEAL.CURRENCY, CCY.MARKET,CROSS.RATE, CROSS.CURRENCY, DRAWDOWN.CCY, T.DATAA, CUST.COND,CHG.AMT.TOT, TOT.CHARGE.FCCY)

    END

RETURN



*SUM THE CHARGE AMOUNT IN LOOP AND ASSIGN BACK TO CHARGE.AMOUNT FIELD ;*AUTO R22 CODE CONVERSION
*NEED TO USE EXCH RATE COMMAND
*
*
*
END
