* @ValidationCode : MjozNDgzOTcxNDQ6Q3AxMjUyOjE2ODA2OTc3ODc3MzM6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 05 Apr 2023 17:59:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.FT.NCF.ID.GEN

*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 05.04.2023       Conversion Tool       R22            Auto Conversion     - + TO = 1, VM TO @VM
* 05.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.L.NCF.STOCK

    IF OFS$BROWSER ELSE
        RETURN
    END

    GOSUB OPEN.FILES
    GOSUB GEN.NCF

RETURN
*-------------------------------------------------------
OPEN.FILES:
*-------------------------------------------------------

    FN.REDO.L.NCF.STOCK = 'F.REDO.L.NCF.STOCK'
    F.REDO.L.NCF.STOCK  = ''
    CALL OPF(FN.REDO.L.NCF.STOCK,F.REDO.L.NCF.STOCK)

    FN.REDO.AA.NCF.IDS = "F.REDO.AA.NCF.IDS"
    F.REDO.AA.NCF.IDS  = ""
    CALL OPF(FN.REDO.AA.NCF.IDS,F.REDO.AA.NCF.IDS)

RETURN
*-------------------------------------------------------
GEN.NCF:
*-------------------------------------------------------


    CALL F.READ(FN.REDO.AA.NCF.IDS,COMI,R.REDO.AA.NCF.IDS,F.REDO.AA.NCF.IDS,NCF.ERR)
    IF R.REDO.AA.NCF.IDS THEN
        RETURN          ;* Record available already.
    END

    Y.STK.ID = 'SYSTEM'
    Y.RTT = ''
    R.NCF.STOCK=''

*  READU R.NCF.STOCK FROM F.REDO.L.NCF.STOCK,Y.STK.ID ELSE ;*Tus Start
    RETRY.VAR = ""
    CALL F.READU(FN.REDO.L.NCF.STOCK,Y.STK.ID,R.NCF.STOCK,F.REDO.L.NCF.STOCK,R.NCF.STOCK.ERR,RETRY.VAR)
    IF R.NCF.STOCK.ERR THEN  ;* Tus End
        R.NCF.STOCK = ""
    END

    R.NCF.IDS  = ""
    Y.LOOP.CNT = 1
    MAX.CNT    = 2
    LOOP
    WHILE Y.LOOP.CNT LE MAX.CNT
        NCF.NUMBER = ""
        GOSUB NCF.GEN
        IF NCF.NUMBER THEN
            R.NCF.IDS<-1> = NCF.NUMBER
        END
        Y.LOOP.CNT += 1        ;** R22 Auto conversion - + TO = 1
    REPEAT



*  WRITE R.NCF.STOCK TO F.REDO.L.NCF.STOCK,'SYSTEM' ;*Tus Start
    CALL F.WRITE(FN.REDO.L.NCF.STOCK,'SYSTEM',R.NCF.STOCK);*Tus End
    IF R.NCF.IDS THEN
        LOCK.FLUSH = ""
        CALL LOG.WRITE(FN.REDO.AA.NCF.IDS,COMI,R.NCF.IDS,LOCK.FLUSH)
    END

*  RELEASE F.REDO.L.NCF.STOCK,Y.STK.ID ;*Tus Start
    CALL F.RELEASE(FN.REDO.L.NCF.STOCK,Y.STK.ID,F.REDO.L.NCF.STOCK);*Tus End

RETURN
*-----------------------------------------------------------------------
NCF.GEN:
*-----------------------------------------------------------------------

    IF R.NCF.STOCK<ST.QTY.AVAIL.ORG> GT '0' THEN
        VAR.SER        = R.NCF.STOCK<ST.SERIES>
        VAR.BUS.DIV    = R.NCF.STOCK<ST.BUSINESS.DIV>
        VAR.PECF       = R.NCF.STOCK<ST.PECF>
        VAR.AICF       = R.NCF.STOCK<ST.AICF>
        VAR.TCF        = R.NCF.STOCK<ST.TCF>
        VAR.SEQ.NO     = R.NCF.STOCK<ST.SEQUENCE.NO>
        NCF.NUMBER     = VAR.SER:VAR.BUS.DIV:VAR.PECF:VAR.AICF:VAR.TCF:VAR.SEQ.NO
        VAR.PREV.RANGE = R.NCF.STOCK<ST.PRE.ED.RG.OR>
        VAR.PREV.RANGE = VAR.PREV.RANGE<DCOUNT(VAR.PREV.RANGE,@VM)>
        IF R.NCF.STOCK<ST.SEQUENCE.NO> EQ VAR.PREV.RANGE THEN
            R.NCF.STOCK<ST.SEQUENCE.NO>=R.NCF.STOCK<ST.L.STRT.RNGE.ORG>
        END ELSE
            R.NCF.STOCK<ST.SEQUENCE.NO>=R.NCF.STOCK<ST.SEQUENCE.NO>+1
        END
        R.NCF.STOCK<ST.SEQUENCE.NO>    = FMT(R.NCF.STOCK<ST.SEQUENCE.NO>,'8"0"R')
        R.NCF.STOCK<ST.NCF.ISSUED.ORG> = R.NCF.STOCK<ST.NCF.ISSUED.ORG>+1

    END

    IF R.NCF.STOCK<ST.QTY.AVAIL.ORG>  GT 0 THEN
        R.NCF.STOCK<ST.QTY.AVAIL.ORG>  = R.NCF.STOCK<ST.QTY.AVAIL.ORG>-1
        IF  R.NCF.STOCK<ST.QTY.AVAIL.ORG> GE R.NCF.STOCK<ST.L.MIN.NCF.ORG>  THEN
            R.NCF.STOCK<ST.NCF.STATUS.ORG>='AVAILABLE'
        END ELSE
            R.NCF.STOCK<ST.NCF.STATUS.ORG>='UNAVAILABLE'
        END
    END
RETURN
END
