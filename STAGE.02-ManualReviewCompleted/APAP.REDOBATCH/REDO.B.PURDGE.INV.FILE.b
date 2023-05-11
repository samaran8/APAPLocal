* @ValidationCode : MjotMTQ2MDQ4ODI5OTpDcDEyNTI6MTY4MTI4Mzc4MTYxODpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:46:21
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.PURDGE.INV.FILE
********************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Jeeva T
* PROGRAM NAME: REDO.B.PURDGE.INV.FILE
*------------------------------------------------------------------------------
*DESCRIPTION:This routine is COB routine to select all FT and TT records from inventory Table
*-------------------------------------------------------------------------------
*IN PARAMETER: NONE
*OUT PARAMETER: NONE
*LINKED WITH: REDO.B.PURDGE.INV.FILE
*-----------------------
* Modification History :
*-----------------------
*DATE             WHO                    REFERENCE            DESCRIPTION
*19/03/2010      JEEVA T              ODR-2009-11-0200    INITIAL CREATION
* Date                   who                   Reference              
* 12-04-2023         CONVERSTION TOOL     R22 AUTO CONVERSTION - FM TO @FM AND ++ TO += 1 
* 12-04-2023          ANIL KUMAR B        R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_BATCH.FILES
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER


    GOSUB MAIN.PROCESS
RETURN

*------------------
MAIN.PROCESS:
*------------------

    FN.REDO.ITEM.SERIES = 'F.REDO.ITEM.SERIES'
    F.REDO.ITEM.SERIES = ''
    CALL OPF(FN.REDO.ITEM.SERIES,F.REDO.ITEM.SERIES)
    R.REDO.ITEM.SERIES = ''

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.FUNDS.TRANSFER.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FUNDS.TRANSFER.NAU = ''
    CALL OPF(FN.FUNDS.TRANSFER.NAU,F.FUNDS.TRANSFER.NAU)

    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.TELLER.NAU =  'F.TELLER$NAU'
    F.TELLER.NAU = ''

    CALL OPF(FN.TELLER.NAU,F.TELLER.NAU)

    Y.ID.LIST = 'FUNDS.TRANSFER':@FM:'TELLER'
    LOOP
        REMOVE Y.ID FROM Y.ID.LIST SETTING PS
    WHILE Y.ID:PS
        CALL F.READ(FN.REDO.ITEM.SERIES,Y.ID,R.REDO.ITEM.SERIES,F.REDO.ITEM.SERIES,Y.ER)
        R.REDO.ITEM.SERIES.BK = R.REDO.ITEM.SERIES
        BEGIN CASE
            CASE Y.ID EQ 'FUNDS.TRANSFER'
                FN.APP = FN.FUNDS.TRANSFER
                F.APP = F.FUNDS.TRANSFER
                FN.APP.NAU =FN.FUNDS.TRANSFER.NAU
                F.APP.NAU = F.FUNDS.TRANSFER.NAU
            CASE Y.ID EQ 'TELLER'
                FN.APP = FN.TELLER
                F.APP = F.TELLER
                FN.APP.NAU = FN.TELLER.NAU
                F.APP.NAU = F.TELLER.NAU
        END CASE

        GOSUB SUB.PROCESS

        CALL F.WRITE(FN.REDO.ITEM.SERIES,Y.ID,R.REDO.ITEM.SERIES.BK)
    REPEAT
RETURN
*------------------
SUB.PROCESS:
*------------------
    Y.CNT = 1
    LOOP
        REMOVE Y.TRANS.VAL FROM R.REDO.ITEM.SERIES SETTING POS
    WHILE Y.TRANS.VAL:POS
        CHANGE '-' TO '*' IN Y.TRANS.VAL
        Y.TRAN.LIST = FIELDS(Y.TRANS.VAL,"*",1)
        R.APP.NAU = '' ; R.APP = ''
        CALL F.READ(FN.APP,Y.TRAN.LIST,R.APP,F.APP,Y.ER)
        IF NOT(R.APP) THEN
            CALL F.READ(FN.APP.NAU,Y.TRAN.LIST,R.APP.NAU,F.APP.NAU,Y.ER.NAU)
            IF NOT(R.APP.NAU) THEN
                DEL R.REDO.ITEM.SERIES.BK<Y.CNT>
            END
        END
        Y.CNT += 1
    REPEAT

RETURN

END
