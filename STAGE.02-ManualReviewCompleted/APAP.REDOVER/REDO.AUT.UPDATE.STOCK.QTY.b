* @ValidationCode : MjotMTk0Nzg4ODU2MTpDcDEyNTI6MTY4MDYxMDM4OTkwNzpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 17:43:09
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.AUT.UPDATE.STOCK.QTY
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: SWAMINATHAN
* PROGRAM NAME: REDO.AUT.UPDATE.STOCK.QTY
* ODR NO      : ODR-2010-03-0400
*----------------------------------------------------------------------
*DESCRIPTION: This routine is authorisation routine to update  REDO.STOCK.QTY.COUNT
*
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*9.03.2011  Swaminathan    ODR-2010-03-0400  INITIAL CREATION
*12 JUL 2011 KAVITHA       PACS00082671      ISSUE FIX FOR PACS00082671
*15 JUL 2011   KAVITHA        PACS00082440      ISSUE FIX FOR PACS00082440
*03-AUG-2011  JEEVA T        TDN7 - change       TDN7 Update
*----------------------------------------------------------------------
*Modification History
*DATE                     WHO                         REFERENCE                     DESCRIPITION
*04-04-2023           Conversion Tool          R22 Auto Code conversion            VM TO @VM
*04-04-2023            Samaran T               Manual R22 Code Conversion    No Changes
*-----------------------------------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.STOCK.QTY.COUNT
    $INSERT I_F.REDO.CARD.REQUEST
    $INSERT I_F.REDO.CARD.REG.STOCK
    $INSERT I_F.REDO.PREEMBOSS.STOCK
    $INSERT I_F.LATAM.CARD.ORDER



    FN.REDO.STOCK.QTY.COUNT = 'F.REDO.STOCK.QTY.COUNT'
    F.REDO.STOCK.QTY.COUNT = ''
    CALL OPF(FN.REDO.STOCK.QTY.COUNT,F.REDO.STOCK.QTY.COUNT)

    FN.LCO = 'F.LATAM.CARD.ORDER'
    F.LCO = ''
    CALL OPF(FN.LCO,F.LCO)

    FN.REDO.CARD.REQUEST = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST = ''
    CALL OPF(FN.REDO.CARD.REQUEST,F.REDO.CARD.REQUEST)

    FN.REDO.CARD.REG.STOCK = 'F.REDO.CARD.REG.STOCK'
    F.REDO.CARD.REG.STOCK = ''
    CALL OPF(FN.REDO.CARD.REG.STOCK,F.REDO.CARD.REG.STOCK)


*PACS00082440-S
    FN.REDO.PREEMBOSS.STOCK = 'F.REDO.PREEMBOSS.STOCK'
    F.REDO.PREEMBOSS.STOCK = ''
    CALL OPF(FN.REDO.PREEMBOSS.STOCK,F.REDO.PREEMBOSS.STOCK)
*PACS00082440-E

*PACS00082671-S
    R.REDO.STOCK.QTY.COUNT = ''


    Y.DATE = TODAY

    Y.SERIES = R.NEW(REDO.CARD.REQ.CARD.SERIES.ID)
    Y.TOT.SERIES = DCOUNT(Y.SERIES,@VM) ;*R22 AUTO CODE CONVERSION

    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.TOT.SERIES

        Y.NEW.SERIES = Y.SERIES<1,Y.CNT>
        Y.QTY.RECD = R.NEW(REDO.CARD.REQ.REGOFF.ACCEPTQTY)<1,Y.CNT>
*******************************TDN7 - change starts ***************************

        Y.STOCK.QTY.COUNT.ID = ID.NEW:'-':Y.NEW.SERIES

        Y.AGENCY = R.NEW(REDO.CARD.REQ.AGENCY)
        Y.STK.CARD.TYPE =        R.NEW(REDO.CARD.REQ.CARD.TYPE)<1,Y.CNT>
*******************************TDN7 - change ends ***************************

        R.REDO.STOCK.QTY.COUNT<REDO.STOCK.QTY.COUNT.AGENCY> = Y.AGENCY
        R.REDO.STOCK.QTY.COUNT<REDO.STOCK.QTY.COUNT.CARD.TYPE> = Y.STK.CARD.TYPE
        R.REDO.STOCK.QTY.COUNT<REDO.STOCK.QTY.COUNT.QTY.RECD,1> = Y.QTY.RECD
        R.REDO.STOCK.QTY.COUNT<REDO.STOCK.QTY.COUNT.DATE,1> = Y.DATE
        R.REDO.STOCK.QTY.COUNT<REDO.STOCK.QTY.COUNT.STOCK.QTY,1> = Y.QTY.RECD

        CALL F.WRITE(FN.REDO.STOCK.QTY.COUNT,Y.STOCK.QTY.COUNT.ID,R.REDO.STOCK.QTY.COUNT)


*PACS00082440-S

        IF R.NEW(REDO.CARD.REQ.PERS.CARD)<1,Y.CNT> EQ '' THEN
            PREEMB.ID = ID.COMPANY:"-":Y.NEW.SERIES
            CALL F.READU(FN.REDO.PREEMBOSS.STOCK,PREEMB.ID,R.REDO.PREEMBOSS.STOCK,F.REDO.PREEMBOSS.STOCK,PRE.ERROR,"")
            IF R.REDO.PREEMBOSS.STOCK THEN
                R.REDO.PREEMBOSS.STOCK<PREEMB.STK.SERIES.BAL> = R.REDO.PREEMBOSS.STOCK<PREEMB.STK.SERIES.BAL> + Y.QTY.RECD
            END ELSE
                R.REDO.PREEMBOSS.STOCK<PREEMB.STK.SERIES.BAL> = Y.QTY.RECD
            END
            CALL F.WRITE(FN.REDO.PREEMBOSS.STOCK,PREEMB.ID,R.REDO.PREEMBOSS.STOCK)
        END
*PACS00082440-E


        Y.CNT += 1 ;*R22 AUTO CODE CONVERSION
    REPEAT

*PACS00082671-E



    RENEWAL.FLAG = R.NEW(REDO.CARD.REQ.RENEWAL.FLAG)

    IF RENEWAL.FLAG EQ "YES" THEN

        VAULT.QUANTITY =   R.NEW(REDO.CARD.REQ.VAULT.QTY)<1,1>
        LATAM.CARD.ID = FIELD(VAULT.QUANTITY,":",2)
        LATAM.CARD.ID = TRIM(LATAM.CARD.ID)
        CALL F.READ(FN.LCO,LATAM.CARD.ID,R.LCO,F.LCO,LCO.ERR)
        R.LCO<CARD.IS.RENEW.STATUS> = "AVAILABLE"
        CALL F.WRITE(FN.LCO,LATAM.CARD.ID,R.LCO)

    END

RETURN
END
