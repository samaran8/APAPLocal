* @ValidationCode : MjotNjkyNzkxMzY6Q3AxMjUyOjE2ODA2MDY0MTk0ODQ6MzMzc3U6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:36:59
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
SUBROUTINE LATAM.V.AUTH.UPD.FILES
*-----------------------------------------------------------------------------
* DESCRIPTION
*-----------
* This is a routine will be called by LATAM.CARD.ORDER during authorization
*
*-------------------------------------------------------------------------------------
* Input / Output
* --------------
* IN     : -na-
* OUT    : -na-
* Dependencies
* ------------
*-------------------------------------------------------------------------------------------
* Revision History
*-------------------------
*    Date             Who               Reference                 Description
* 27-Oct-2008        Mohan.N            STBP20081027               Initial Creation
* 15 Oct 2010        GANESH             ODR-2010-08-0467          C.15 INTERFACE
* Modification History:
*
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*04/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION     VM TO @VM, FM TO @FM, SM TO @SM, = TO EQ, CONVERT TO CHANGE, = NO.OF.ACCT+1 TO += 1
*04/04/2023         SURESH           MANUAL R22 CODE CONVERSION             NOCHANGE
*---------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CARD.TYPE
    $INSERT I_F.CARD.ISSUE
    $INSERT I_F.CARD.CHARGE
    $INSERT I_F.STOCK.ENTRY
    $INSERT I_F.STOCK.REGISTER
    $INSERT I_F.ACCOUNT
*----------
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.LATAM.CARD.REGISTER
    $INSERT I_F.LATAM.CARD.CUSTOMER
*-----------------------------------------------------------------------------------------------

    GOSUB INITIALIZE
    GOSUB OPEN.FILE
    GOSUB PROCESS

RETURN
*-----------------------------------------------------------------------------------------------
INITIALIZE:
*-----------------------------------------------------------------------------------------------
*
    APPLN = "CARD.ISSUE":@FM:"STOCK.ENTRY" ;*AUTO R22 CODE CONVERSION

    FLD.NAMES = 'L.CI.CUST.NO':@VM:'L.CI.TYP.OF.CARD':@VM:'L.CI.CARD.NO' ;*AUTO R22 CODE CONVERSION
    FLD.NAMES = FLD.NAMES:@VM:'L.CI.PLASTIC':@VM:'L.CI.ACCT.OFF'
    FLD.NAMES = FLD.NAMES:@VM:'L.CI.PLACE.DELV':@VM:'L.CI.DELV.ADDR':@VM:'L.CI.TYPE.ISSU'
    FLD.NAMES = FLD.NAMES:@VM:'L.CI.ISSU.STAGE':@VM:'L.CI.BLOCK.STAT':@VM:'L.CI.TYPE.BLOCK'
    FLD.NAMES = FLD.NAMES:@VM:'L.CI.DATE.DELIV':@VM:'L.CI.TYPE.MOD':@VM:'L.CI.TXN.FEES'
    FLD.NAMES = FLD.NAMES:@VM:'L.CI.CATEG.CARD':@VM:'L.CI.CARD.TYPE'
    FLD.NAMES = FLD.NAMES:@FM:'L.SE.DATE.MVMT':@VM:'L.SE.COMMENTS'

    FLD.POS = ''
    CALL MULTI.GET.LOC.REF(APPLN,FLD.NAMES,FLD.POS)

    CUSTOMER.NO.POS = FLD.POS<1,1>
    TYPE.OF.CARD.POS = FLD.POS<1,2>
    CARD.NUMBER.POS = FLD.POS<1,3>
    NAME.ON.PLASTIC.POS = FLD.POS<1,4>
    ACCOUNT.OFFICER.POS =FLD.POS<1,5>
    PLACE.OF.DELIV.POS = FLD.POS<1,6>
    DELIV.ADDRESS.POS = FLD.POS<1,7>
    TYPE.ISSUE.POS = FLD.POS<1,8>
    ISSUE.STAGE.POS = FLD.POS<1,9>
    BLOCK.STATUS.POS = FLD.POS<1,10>
    TYPE.OF.BLOCK.POS = FLD.POS<1,11>
    DATE.DELIVERED.POS = FLD.POS<1,12>
    TYPE.MODIFY.POS = FLD.POS<1,13>
    TXN.FEES.POS = FLD.POS<1,14>
    CATEGORY.CARD.POS = FLD.POS<1,15>
    CARD.TYPE.POS = FLD.POS<1,16>

    DATE.MVMT.POS = FLD.POS<2,1>
    COMMENTS.POS = FLD.POS<2,2>

RETURN
*-----------------------------------------------------------------------------------------------
OPEN.FILE:
*-----------------------------------------------------------------------------------------------
*
    FN.CARD.ISSUE = "F.CARD.ISSUE"
    F.CARD.ISSUE = ""
    CALL OPF(FN.CARD.ISSUE,F.CARD.ISSUE)

    FN.CARD.ORDER.HIS = "F.LATAM.CARD.ORDER$HIS"
    F.CARD.ORDER.HIS = ''
    CALL OPF(FN.CARD.ORDER.HIS,F.CARD.ORDER.HIS)

    FN.CARD.CUSTOMER = "F.LATAM.CARD.CUSTOMER"
    F.CARD.CUSTOMER = ""
    CALL OPF(FN.CARD.CUSTOMER,F.CARD.CUSTOMER)

    FN.CARD.REGISTER = "F.LATAM.CARD.REGISTER"
    F.CARD.REGISTER = ''
    CALL OPF(FN.CARD.REGISTER,F.CARD.REGISTER)

    FN.STOCK.ENTRY = 'F.STOCK.ENTRY'
    F.STOCK.ENTRY = ''
    CALL OPF(FN.STOCK.ENTRY,F.STOCK.ENTRY)

    FN.STOCK.REGISTER = 'F.STOCK.REGISTER'
    F.STOCK.REGISTER = ''
    CALL OPF(FN.STOCK.REGISTER,F.STOCK.REGISTER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

*------------------------------
* C.15-------------------------
    FN.LATAM.CARD.ORDER='F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER=''
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)
*------------------------------


RETURN

*-----------------------------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------------------------

    CARD.CURR.NO = R.NEW(CARD.IS.CURR.NO)
    CARD.NO = FIELD(ID.NEW,'.',2)

    SEL.CARD.HIS = "SELECT ":FN.CARD.ORDER.HIS:" WITH @ID LIKE '":ID.NEW:"...'"
    CALL EB.READLIST(SEL.CARD.HIS,SEL.LIST,'',NOF.REC,SEL.CARD.HIS.ERR)
    CARD.HIS.NOS = FIELDS(SEL.LIST,';',2)
    LAST.HIS.NO = MAXIMUM(CARD.HIS.NOS)
    Y.CARD.ORDER.HIS.ID = ID.NEW:';':LAST.HIS.NO
    R.CARD.ORDER.HIS = ''
    CARD.ORDER.HIS.ERR = ''
    CALL F.READ(FN.CARD.ORDER.HIS,Y.CARD.ORDER.HIS.ID,R.CARD.ORDER.HIS,F.CARD.ORDER.HIS,CARD.ORDER.HIS.ERR)

    ACCOUNTS = R.NEW(CARD.IS.ACCOUNT)
    Y.ACC.CUST.NO = ACCOUNTS<1,1>
    R.ACCOUNT = ''
    ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,Y.ACC.CUST.NO,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    Y.CUST.NO = R.ACCOUNT<AC.CUSTOMER>
    CR.EXPIRY.DATE = R.NEW(CARD.IS.EXPIRY.DATE)
    R.CARD.CUST = ''
    CARD.CUST.ERR = ''
    CARD.NO.POS = ''
    CALL F.READ(FN.CARD.CUSTOMER,Y.CUST.NO,R.CARD.CUST,F.CARD.CUSTOMER,CARD.CUST.ERR)

    IF R.CARD.CUST THEN
        CARD.NOS = R.CARD.CUST<APAP.DC.CARD.NO>
        CHANGE @VM TO @FM IN CARD.NOS ;*AUTO R22 CODE CONVERSION
        LOCATE ID.NEW IN CARD.NOS SETTING CARD.NO.POS THEN
            R.CARD.CUST<APAP.DC.CARD.NO,CARD.NO.POS> = ''
            R.CARD.CUST<APAP.DC.ACCOUNT.NO,CARD.NO.POS> = ''
            R.CARD.CUST<APAP.DC.ACCOUNT.TYPE,CARD.NO.POS> = ''
            R.CARD.CUST<APAP.DC.EXPIRY.DATE,CARD.NO.POS> = ''
        END
    END ELSE
        CARD.NO.POS = 1
    END
    GOSUB UPDATE.CARD.DETAILS
    CALL F.WRITE(FN.CARD.CUSTOMER,Y.CUST.NO,R.CARD.CUST)
*
    GOSUB UPDATE.CARD.REGISTER
    GOSUB UPDATE.STOCK.ENTRY

RETURN

*-----------------------------------------------------------------------------------------------
UPDATE.CARD.DETAILS:
*-----------------------------------------------------------------------------------------------
*
    NO.OF.ACCT = 1
    LOOP
        REMOVE ACCT FROM ACCOUNTS SETTING ACC.POS
    WHILE ACCT:ACC.POS

        R.CARD.CUST<APAP.DC.CARD.NO,CARD.NO.POS> = ID.NEW
        IF NO.OF.ACCT EQ 1 THEN
            R.CARD.CUST<APAP.DC.ACCOUNT.TYPE,CARD.NO.POS,NO.OF.ACCT> = "PRIMARY"
        END ELSE
            R.CARD.CUST<APAP.DC.ACCOUNT.TYPE,CARD.NO.POS,NO.OF.ACCT> ="SECONDARY"
        END
        R.CARD.CUST<APAP.DC.ACCOUNT.NO,CARD.NO.POS,NO.OF.ACCT> = ACCT
        NO.OF.ACCT += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
    R.CARD.CUST<APAP.DC.EXPIRY.DATE,CARD.NO.POS> = CR.EXPIRY.DATE
RETURN
*-----------------------------------------------------------------------------------------------
UPDATE.CARD.REGISTER:
*-----------------------------------------------------------------------------------------------
*
    YYYY = TODAY[1,4]
    Y.CARD.REG.ID = ID.NEW:'-':YYYY
    R.CARD.REGISTER = ''
    CARD.REG.ERR = ''

    CALL F.READ(FN.CARD.REGISTER,Y.CARD.REG.ID,R.CARD.REGISTER,F.CARD.REGISTER,CARD.REG.ERR)
    REG.DATES = R.CARD.REGISTER<SBP.DC.DATE>
    IF R.CARD.REGISTER THEN
        GOSUB PROCESS.FLAG
    END ELSE
        CHANGE.FLAG = 1
        CHANGE.POS = 1
        GOSUB UPD.CHANGES.CARD.REG
    END

    IF CHANGE.FLAG EQ 1 THEN ;*AUTO R22 CODE CONVERSION
        CALL F.WRITE(FN.CARD.REGISTER,Y.CARD.REG.ID,R.CARD.REGISTER)
    END

RETURN
*------------------------------------------------------------------------------
UPD.CHANGES.CARD.REG:
*------------------------------------------------------------------------------
* Update the change details in card register record array
*

    R.CARD.REGISTER<SBP.DC.DATE,CHANGE.POS> = TODAY
    R.CARD.REGISTER<SBP.DC.CUSTOMER,CHANGE.POS> =  Y.CUST.NO

    R.CARD.REGISTER<SBP.DC.CARD.NAME,CHANGE.POS> = R.NEW(CARD.IS.LOCAL.REF)<1,NAME.ON.PLASTIC.POS,1>
    R.CARD.REGISTER<SBP.DC.CARD.NUMBER,CHANGE.POS> = R.NEW(CARD.IS.LOCAL.REF)<1,CARD.NUMBER.POS,1>
    R.CARD.REGISTER<SBP.DC.TYPE.OF.CARD,CHANGE.POS> = R.NEW(CARD.IS.LOCAL.REF)<1,TYPE.OF.CARD.POS,1>

    R.CARD.REGISTER<SBP.DC.PRINCIPAL.CARD,CHANGE.POS> = R.NEW(CARD.IS.LOCAL.REF)<1,CARD.NUMBER.POS,2>
    R.CARD.REGISTER<SBP.DC.PRINCIPAL.CUST,CHANGE.POS> = Y.CUST.NO
    R.CARD.REGISTER<SBP.DC.PRINC.CUST.NAME,CHANGE.POS> = R.NEW(CARD.IS.LOCAL.REF)<1,NAME.ON.PLASTIC.POS,2>
    CARD.ACCT = R.NEW(CARD.IS.ACCOUNT)
    CHANGE @VM TO @SM IN CARD.ACCT ;*AUTO R22 CODE CONVERSION
    R.CARD.REGISTER<SBP.DC.ACCOUNTS,CHANGE.POS> = CARD.ACCT
    R.CARD.REGISTER<SBP.DC.CARD.STAGE,CHANGE.POS> = R.NEW(CARD.IS.LOCAL.REF)<1,ISSUE.STAGE.POS>
    EXP.DAT = R.NEW(CARD.IS.EXPIRY.DATE)
    R.CARD.REGISTER<SBP.DC.EXPIRY.DATE,CHANGE.POS> = EXP.DAT
    R.CARD.REGISTER<SBP.DC.TYPE.OF.ISSUE,CHANGE.POS> =  R.NEW(CARD.IS.LOCAL.REF)<1,TYPE.ISSUE.POS>
* ---------C.15 part starts-------------------------------------------------------

    CALL F.READ(FN.LATAM.CARD.ORDER,ID.NEW,R.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER,LATAM.ERR)
    R.CARD.REGISTER<SBP.DC.ISSUE.INDICATOR>=R.LATAM.CARD.ORDER<CARD.IS.ISSUE.INDICATOR>

* ---------C.15 part starts-------------------------------------------------------


RETURN
*------------------------------------------------------------------------------
UPDATE.STOCK.ENTRY:
*------------------------------------------------------------------------------
*
    IF CARD.CURR.NO EQ '1' THEN

        REG.ID = R.NEW(CARD.IS.STOCK.REG.ID)
        START.NO = R.NEW(CARD.IS.CARD.START.NO)
        CARD.SERIES.ID = R.NEW(CARD.IS.STOCK.SERIERS.ID)
        R.STOCK.REGISTER = ''
        ERR = ''
        CALL F.READ(FN.STOCK.REGISTER,REG.ID,R.STOCK.REGISTER,F.STOCK.REGISTER,ERR)
        IF R.STOCK.REGISTER THEN
            SERIES.ID = R.STOCK.REGISTER<STO.REG.SERIES.ID>
            CHANGE @VM TO @FM IN SERIES.ID ;*AUTO R22 CODE CONVERSION
            GOSUB EXTRACT.STOCK.ENTRY.ID
        END
        CALL F.READ(FN.STOCK.ENTRY,STOCK.ENTRY.ID,R.STOCK.ENTRY,F.STOCK.ENTRY,ENT.ERR)
        IF R.STOCK.ENTRY THEN

            AVAIL.DATE = R.STOCK.ENTRY<STO.ENT.LOCAL.REF,DATE.MVMT.POS>
            CHANGE @SM TO @FM IN AVAIL.DATE ;*AUTO R22 CODE CONVERSION
            GOSUB CHECK.AVAIL.DATE

        END
        CALL F.WRITE(FN.STOCK.ENTRY,STOCK.ENTRY.ID,R.STOCK.ENTRY)
    END
RETURN
*-------------------------------------------------------------------------
EXTRACT.STOCK.ENTRY.ID:
*-------------------------------------------------------------------------
*
    LOCATE CARD.SERIES.ID IN SERIES.ID SETTING SER.POS THEN
        STOCK.SERIES = R.STOCK.REGISTER<STO.REG.SERIES.NO>
        NO.OF.SERIES = DCOUNT(STOCK.SERIES,@SM) ;*AUTO R22 CODE CONVERSION
        FOR SERIES.CNTR = 1 TO NO.OF.SERIES
            TEM.STOCK.START.NO = STOCK.SERIES<1,SER.POS,SERIES.CNTR>
            STOCK.START.NO = FIELD(TEM.STOCK.START.NO,'-',1)
            IF (STOCK.START.NO-1) EQ START.NO THEN
                STOCK.ENTRY.POS = (NO.OF.SERIES - SERIES.CNTR) + 1
                STOCK.ENTRY.ID = R.STOCK.REGISTER<STO.REG.STO.ENTRY.ID,SER.POS,STOCK.ENTRY.POS>
            END
        NEXT SERIES.CNTR
    END

RETURN
*-------------------------------------------------------------------------
CHECK.AVAIL.DATE:
*-------------------------------------------------------------------------
*
    LOCATE TODAY IN AVAIL.DATE SETTING AVAIL.POS THEN
        R.STOCK.ENTRY<STO.ENT.LOCAL.REF,DATE.MVMT.POS,AVAIL.POS> = TODAY
        R.STOCK.ENTRY<STO.ENT.LOCAL.REF,COMMENTS.POS,AVAIL.POS> = 'New Card issued'
    END ELSE
        GOSUB APPEND.STOCK.ENTRY
    END

RETURN
*-------------------------------------------------------------------------
APPEND.STOCK.ENTRY:
*-------------------------------------------------------------------------
    R.STOCK.ENTRY<STO.ENT.LOCAL.REF,DATE.MVMT.POS,-1> = TODAY
    R.STOCK.ENTRY<STO.ENT.LOCAL.REF,COMMENTS.POS,-1> = 'New Card issued and sent to emboss for printing'

RETURN
*-----------------------------------------------------------------------------------------------
************
PROCESS.FLAG:
**************
    IF R.CARD.ORDER.HIS<CARD.IS.CUSTOMER.NO,1> NE R.NEW(CARD.IS.LOCAL.REF)<1,CUSTOMER.NO.POS,1> THEN
        CHANGE.FLAG = 1
    END

    IF R.CARD.ORDER.HIS<CARD.IS.NAME.ON.PLASTIC,1> NE R.NEW(CARD.IS.LOCAL.REF)<1,NAME.ON.PLASTIC.POS,1> THEN
        CHANGE.FLAG = 1
    END

    IF R.CARD.ORDER.HIS<CARD.IS.TYPE.OF.CARD,1> NE R.NEW(CARD.IS.LOCAL.REF)<1,TYPE.OF.CARD.POS,1> THEN
        CHANGE.FLAG = 1
    END

    IF R.CARD.ORDER.HIS<CARD.IS.CARD.NUMBER,1> NE R.NEW(CARD.IS.LOCAL.REF)<1,CARD.NUMBER.POS,1> THEN
        CHANGE.FLAG = 1
    END

    IF R.CARD.ORDER.HIS<CARD.IS.CARD.NUMBER,2> NE R.NEW(CARD.IS.LOCAL.REF)<1,CARD.NUMBER.POS,2> THEN
        CHANGE.FLAG = 1
    END
*    IF R.CARD.ORDER.HIS<CARD.IS.CUSTOMER.NO,2> NE R.NEW(CARD.IS.LOCAL.REF)<1,CUSTOMER.NO.POS,2> THEN
*        CHANGE.FLAG = 1
*    END
    IF R.CARD.ORDER.HIS<CARD.IS.NAME.ON.PLASTIC,2> NE R.NEW(CARD.IS.LOCAL.REF)<1,NAME.ON.PLASTIC.POS,2> THEN
        CHANGE.FLAG = 1
    END
    OLD.ACCOUNTS = DCOUNT(R.CARD.ORDER.HIS<CARD.IS.ACCOUNT>,@VM) ;*AUTO R22 CODE CONVERSION
    NEW.ACCOUNTS = DCOUNT(R.NEW(CARD.IS.ACCOUNT),@VM)
    IF OLD.ACCOUNTS GT NEW.ACCOUNTS THEN
        LOOP.CNTR =OLD.ACCOUNTS
    END ELSE
        LOOP.CNTR =NEW.ACCOUNTS
    END
    LOOP.REPEAT = 0
    LOOP
        LOOP.REPEAT += 1
    WHILE LOOP.REPEAT LE LOOP.CNTR
        IF R.CARD.ORDER.HIS<CARD.IS.ACCOUNT,LOOP.REPEAT> NE R.NEW(CARD.IS.ACCOUNT)<1,LOOP.REPEAT> THEN
            CHANGE.FLAG = 1
        END

    REPEAT
    IF R.CARD.ORDER.HIS<CARD.IS.ISSUE.STAGE> NE R.NEW(CARD.IS.LOCAL.REF)<1,ISSUE.STAGE.POS> THEN
        CHANGE.FLAG = 1
    END

    IF R.CARD.ORDER.HIS<CARD.IS.EXPIRY.DATE> NE R.NEW(CARD.IS.EXPIRY.DATE) THEN
        CHANGE.FLAG = 1
    END

    IF R.CARD.ORDER.HIS<CARD.IS.TYPE.OF.ISSUE> NE R.NEW(CARD.IS.LOCAL.REF)<1,TYPE.ISSUE.POS> THEN
        CHANGE.FLAG = 1
    END

    IF CHANGE.FLAG THEN
        NOF.CHANGES = DCOUNT(REG.DATES,@VM) ;*AUTO R22 CODE CONVERSION
        CHANGE.POS = NOF.CHANGES+1
        GOSUB UPD.CHANGES.CARD.REG
    END
RETURN
*---------------------------------------------------------------------------------------------------------
END
