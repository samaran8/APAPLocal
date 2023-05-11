* @ValidationCode : Mjo3MDc2ODg4OTk6Q3AxMjUyOjE2ODMwMjAyNTIyOTE6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 02 May 2023 15:07:32
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
* Version n dd/mm/yy  GLOBUS Release No. 200508 30/06/05

SUBROUTINE REDO.TT.SORT.BY.DENOM
*
* This subroutine will sort the denomination,serial.no in ascending order
* of the denominations.
*-----------------------------------------------------------------------------
* MODIFICATION HISTORY:
*
* 20/08/07 - CI_10050936 / CI_10051016
*            New selection criteria(SORT.BY.VALUE) is introduced to display
*            the output in sorted order of DENOMINATION Value.
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*17/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION          VM TO @VM, = TO EQ, I TO I.WAR, CONVERT TO CHANGE, F.READ TO CACHE.READ
*17/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TT.STOCK.CONTROL
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ENQUIRY.SELECT
    $INSERT I_F.TELLER.DENOMINATION
*
    GOSUB INITIALISE
    IF SORT.BY.VALUE EQ 'YES' THEN ;*AUTO R22 CODE CONVERSION
        GOSUB SORT.BY.VALUE       ;* Sort by denomination value
    END ELSE
        GOSUB SORT.BY.DENOM       ;* Sort by denomination ID.
    END
    GOSUB STORE.RETURN.VALUES
*
RETURN
*-------------
INITIALISE:
*-------------
    STOCK.ID = O.DATA
* Open related files
    FN.TT.STOCK.CNT = 'F.TT.STOCK.CONTROL'
    FV.TT.STOCK.CNT = ''
    CALL OPF(FN.TT.STOCK.CNT,FV.TT.STOCK.CNT)
    FN.TT.DENOM = 'F.TELLER.DENOMINATION'
    FV.TT.DENOM = ''
    CALL OPF(FN.TT.DENOM,FV.TT.DENOM)

* SORT.BY.VALUE determines if the DENOMINATION is sorted by DENOMINATION ID or DENOMINATION Value.
    SORT.BY.VALUE = ''
    FINDSTR 'SORT.BY.VALUE' IN ENQ.SELECTION SETTING FMC,VMC,SMC THEN
        SORT.BY.VALUE = ENQ.SELECTION<4,VMC>
    END

    TT.STK.REC = ''
    CALL F.READ(FN.TT.STOCK.CNT,STOCK.ID,TT.STK.REC,FV.TT.STOCK.CNT,ERR3)
    DENOM.CNT = DCOUNT(TT.STK.REC<TT.SC.DENOMINATION>,@VM)
*
    TT.STK.TEMP.REC.DENOM = ''
    TT.STK.TEMP.REC.VALUE = ''
    TT.STK.TEMP.REC.QTY = ''
    TT.STK.TEMP.REC.SER = ''
RETURN
*-------------
SORT.BY.DENOM:
*-------------
    FOR I.VAR = 1 TO DENOM.CNT ;*AUTO R22 CODE CONVERSION
        U.TEMP = TT.STK.REC<TT.SC.DENOMINATION,I.VAR>
        LOCATE U.TEMP IN TT.STK.TEMP.REC.DENOM<1> BY 'AL' SETTING POS ELSE
            INS U.TEMP BEFORE TT.STK.TEMP.REC.DENOM<POS>
            INS TT.STK.REC<TT.SC.QUANTITY,I.VAR> BEFORE TT.STK.TEMP.REC.QTY<POS> ;*AUTO R22 CODE CONVERSION
            INS TT.STK.REC<TT.SC.SERIAL.NO,I.VAR> BEFORE TT.STK.TEMP.REC.SER<POS>
        END
    NEXT I.VAR ;*AUTO R22 CODE CONVERSION
*
RETURN
*-------------
SORT.BY.VALUE:
*-------------
    FOR I.VAR = 1 TO DENOM.CNT ;*AUTO R22 CODE CONVERSION
        R.TT.DENOM = ''
        TT.ERR     = ''
        U.TEMP = TT.STK.REC<TT.SC.DENOMINATION,I.VAR> ;*AUTO R22 CODE CONVERSION
        CALL CACHE.READ(FN.TT.DENOM, U.TEMP, R.TT.DENOM, TT.ERR) ;*AUTO R22 CODE CONVERSION
        LOCATE R.TT.DENOM<TT.DEN.VALUE> IN TT.STK.TEMP.REC.VALUE<1> BY 'DN' SETTING POS ELSE NULL
        INS U.TEMP BEFORE TT.STK.TEMP.REC.DENOM<POS>
        INS R.TT.DENOM<TT.DEN.VALUE> BEFORE TT.STK.TEMP.REC.VALUE<POS>
        INS TT.STK.REC<TT.SC.QUANTITY,I.VAR> BEFORE TT.STK.TEMP.REC.QTY<POS> ;*AUTO R22 CODE CONVERSION
        INS TT.STK.REC<TT.SC.SERIAL.NO,I.VAR> BEFORE TT.STK.TEMP.REC.SER<POS>
    NEXT I.VAR ;*AUTO R22 CODE CONVERSION
*
RETURN
*-------------------
STORE.RETURN.VALUES:
*-------------------
    CHANGE @FM TO @VM IN TT.STK.TEMP.REC.DENOM ;*AUTO R22 CODE CONVERSION
    CHANGE @FM TO @VM IN TT.STK.TEMP.REC.QTY
    CHANGE @FM TO @VM IN TT.STK.TEMP.REC.SER

    VM.COUNT = DCOUNT(TT.STK.TEMP.REC.DENOM,@VM)
    SM.COUNT = DCOUNT(TT.STK.TEMP.REC.SER,@SM)
*
    R.RECORD<110> = TT.STK.TEMP.REC.DENOM
    R.RECORD<111> = TT.STK.TEMP.REC.QTY
    R.RECORD<112> = TT.STK.TEMP.REC.SER
*
RETURN
END
