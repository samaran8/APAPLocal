* @ValidationCode : Mjo3NjMxNzY3OTc6Q3AxMjUyOjE2ODE4MTg3OTQ4NjU6MzMzc3U6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 18 Apr 2023 17:23:14
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
SUBROUTINE REDO.VISA.STLMT.FILE.PROCESS.LOAD
*--------------------------------------------------------------------------
*Company Name      : APAP Bank
*Developed By      : Temenos Application Management
*Program Name      : REDO.VISA.STLMT.FILE.PROCESS.LOAD
*Date              : 23.11.2010
*-------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : --N/A--
*-----------------------------------------------------------------------------
* Revision History:
* -----------------
* Date                   Name                   Reference               Version
* -------                ----                   ----------              --------
*23/11/2010      saktharrasool@temenos.com   ODR-2010-08-0469       Initial Version
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*18/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION       FM TO @FM, VM TO @VM,++ TO +=
*18/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.VISA.STLMT.FILE.PROCESS.COMMON



    GOSUB INIT
    GOSUB GET.LOC.REF
    GOSUB PROCESS
RETURN

*------------------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------------------

    FN.REDO.STLMT.CNCT.PROCESS='F.REDO.STLMT.CNCT.PROCESS'
    F.REDO.STLMT.CNCT.PROCESS=''
    CALL OPF(FN.REDO.STLMT.CNCT.PROCESS,F.REDO.STLMT.CNCT.PROCESS)

    FN.REDO.VISA.STLMT.MAPPING='F.REDO.VISA.STLMT.MAPPING'
    F.REDO.VISA.STLMT.MAPPING=''
    CALL OPF(FN.REDO.VISA.STLMT.MAPPING,F.REDO.VISA.STLMT.MAPPING)

    FN.CARD.TYPE='F.CARD.TYPE'
    F.CARD.TYPE=''
    CALL OPF(FN.CARD.TYPE,F.CARD.TYPE)

    FN.REDO.CARD.BIN='F.REDO.CARD.BIN'
    F.REDO.CARD.BIN=''
    CALL OPF(FN.REDO.CARD.BIN,F.REDO.CARD.BIN)

    FN.LATAM.CARD.ORDER='F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER=''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)

    FN.REDO.VISA.OUTGOING='F.REDO.VISA.OUTGOING'
    F.REDO.VISA.OUTGOING=''
    CALL OPF(FN.REDO.VISA.OUTGOING,F.REDO.VISA.OUTGOING)

    FN.REDO.VISA.STLMT.05TO37='F.REDO.VISA.STLMT.05TO37'
    F.REDO.VISA.STLMT.05TO37=''
    CALL OPF(FN.REDO.VISA.STLMT.05TO37,F.REDO.VISA.STLMT.05TO37)

    FN.ATM.REVERSAL='F.ATM.REVERSAL'
    F.ATM.REVERSAL=''
    CALL OPF(FN.ATM.REVERSAL,F.ATM.REVERSAL)

    FN.REDO.VISA.CNCT.DATE='F.REDO.VISA.CNCT.DATE'
    F.REDO.VISA.CNCT.DATE=''
    CALL OPF(FN.REDO.VISA.CNCT.DATE,F.REDO.VISA.CNCT.DATE)

    FN.REDO.MERCHANT.CATEG='F.REDO.MERCHANT.CATEG'
    F.REDO.MERCHANT.CATEG=''
    CALL OPF(FN.REDO.MERCHANT.CATEG,F.REDO.MERCHANT.CATEG)

    FN.AC.LOCKED.EVENTS = 'F.AC.LOCKED.EVENTS'
    F.AC.LOCKED.EVENTS = ''
    CALL OPF(FN.AC.LOCKED.EVENTS,F.AC.LOCKED.EVENTS)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.STLMT.REASON.CODE = 'F.REDO.STLMT.REASON.CODE'
    F.REDO.STLMT.REASON.CODE = ''
    CALL OPF(FN.REDO.STLMT.REASON.CODE,F.REDO.STLMT.REASON.CODE)

    FN.REDO.VISA.CHGBCK.LOG='F.REDO.VISA.CHGBCK.LOG'
    F.REDO.VISA.CHGBCK.LOG=''
    CALL OPF(FN.REDO.VISA.CHGBCK.LOG,F.REDO.VISA.CHGBCK.LOG)

    FN.REDO.VISA.GEN.OUT='F.REDO.VISA.GEN.OUT'
    F.REDO.VISA.GEN.OUT=''
    CALL OPF(FN.REDO.VISA.GEN.OUT,F.REDO.VISA.GEN.OUT)

    FN.CARD.ISSUE='F.CARD.ISSUE'
    F.CARD.ISSUE=''
    CALL OPF(FN.CARD.ISSUE,F.CARD.ISSUE)

    FN.REDO.APAP.H.PARAMETER='F.REDO.APAP.H.PARAMETER'

    FN.REDO.VISA.STLMT.PARAM='F.REDO.VISA.STLMT.PARAM'

    FN.REDO.DC.STLMT.ERR.CODE='F.REDO.DC.STLMT.ERR.CODE'

    Y.PURCHASE.DETAIL=''

RETURN
*------------------------------------------------------------------------------------
GET.LOC.REF:
*------------------------------------------------------------------------------------

    LOC.REF.APPLICATION="FUNDS.TRANSFER":@FM:'ACCOUNT':@FM:'AC.LOCKED.EVENTS'
    LOC.REF.FIELDS='AT.UNIQUE.ID':@VM:'POS.COND':@VM:'BIN.NO':@VM:'AT.AUTH.CODE':@VM:'ATM.TERM.ID':@VM:'L.STLMT.ID':@VM:'L.STLMT.APPL':@FM:'L.AC.AV.BAL':@FM:'L.TXN.AMT':@VM:'L.TXN.CHG.LOCAL'
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.AT.UNIQUE.ID=LOC.REF.POS<1,1>
    POS.POS.COND=LOC.REF.POS<1,2>
    POS.BIN.NO=LOC.REF.POS<1,3>
    POS.AT.AUTH.CODE=LOC.REF.POS<1,4>
    POS.ATM.TERM.ID=LOC.REF.POS<1,5>
    POS.L.STLMT.ID=LOC.REF.POS<1,6>
    POS.L.STLMT.APPL=LOC.REF.POS<1,7>
    POS.L.AC.AV.BAL = LOC.REF.POS<2,1>
    POS.L.TXN.AMT=LOC.REF.POS<3,1>
    POS.L.TXN.CHG.LOCAL=LOC.REF.POS<3,2>

RETURN


*------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------

    Y.PARA.ID='SYSTEM'
    CALL CACHE.READ(FN.REDO.APAP.H.PARAMETER,Y.PARA.ID,R.REDO.APAP.H.PARAMETER,PARA.ERR)
    CALL CACHE.READ(FN.REDO.VISA.STLMT.PARAM,Y.PARA.ID,R.REDO.VISA.STLMT.PARAM,PARA.ERR)
    CALL CACHE.READ(FN.REDO.DC.STLMT.ERR.CODE,Y.PARA.ID,R.REDO.DC.STLMT.ERR.CODE,PARA.ERR)

    SEL.CMD.DATE='SELECT ':FN.REDO.VISA.CNCT.DATE
    CALL EB.READLIST(SEL.CMD.DATE,SEL.LIST.DATE,'',SEL.NOR,SEL.RET)
    Y.VAR1=1
    LOOP
    WHILE Y.VAR1 LE SEL.NOR
        CALL F.READ(FN.REDO.VISA.CNCT.DATE,SEL.LIST.DATE<Y.VAR1>,R.REDO.VISA.CNCT.DATE,F.REDO.VISA.CNCT.DATE,CNCT.DATE.ERR)
        Y.PURCHASE.DETAIL<1,-1>=SEL.LIST.DATE<Y.VAR1>
        Y.PURCHASE.DETAIL<2,-1>=R.REDO.VISA.CNCT.DATE<1>

        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

RETURN

END
