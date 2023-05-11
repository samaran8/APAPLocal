* @ValidationCode : MjotNTczMDMwMTA5OkNwMTI1MjoxNjgwNzgxMzA5MjkzOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Apr 2023 17:11:49
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
SUBROUTINE REDO.B.AUTO.ITEM.REQ(Y.SEL)
********************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Swaminathan.S.R
* PROGRAM NAME: REDO.B.AUTOMATIC.ORDER
*------------------------------------------------------------------------------
*DESCRIPTION:This routine is COB routine to select all INVENTORY PARAMTER and to reorder automatically based on REORDER.LEVEL. Attach to D990 stage
*-------------------------------------------------------------------------------
*IN PARAMETER: NONE
*OUT PARAMETER: NONE
*LINKED WITH: REDO.B.AUTO.ITEM.REQ
*-----------------------
* Modification History :
*-----------------------
*DATE             WHO                    REFERENCE            DESCRIPTION
*17 MAY 2010      JEEVA T              ODR-2009-11-0200    INITIAL CREATION
* Date                  who                   Reference              
* 06-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION VM TO @VM AND ++ TO += 1
* 06-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_GTS.COMMON
    $INSERT I_BATCH.FILES
    $INSERT I_F.REDO.H.ORDER.DETAILS
    $INSERT I_F.REDO.H.REORDER.LEVEL
    $INSERT I_F.REDO.ITEM.STOCK
    $INSERT I_REDO.B.AUTO.ITEM.REQ.COMMON
    $INSERT I_F.REDO.H.MAIN.COMPANY

    GOSUB PROCESS
RETURN

*---------
PROCESS:
*---------
*To reorder automatically based on REORDER.LEVEL

    Y.SEL.LIST = Y.SEL
    Y.SR.SEL.LIST.ID = FIELD(Y.SEL,"-",1)
    Y.COMPANY.ID.VAL = FIELD(Y.SR.SEL.LIST.ID,"-",1)
    Y.CODE = FIELD(Y.SEL,"-",2)
    CALL F.READ(FN.REDO.ITEM.STOCK,Y.SEL.LIST,R.REDO.ITEM.STOCK,F.REDO.ITEM.STOCK,Y.ERR)
    CALL F.READ(FN.REDO.H.REORDER.LEVEL,Y.COMPANY.ID.VAL,R.REDO.H.REORDER.LEVEL,F.REDO.H.REORDER.LEVEL,Y.INV.ERR)
    Y.ITEM.LIST.STOCK = R.REDO.ITEM.STOCK<ITEM.REG.ITEM.CODE>
    Y.ITEM.LIST = R.REDO.H.REORDER.LEVEL<RE.ORD.ITEM.VALUE>
    Y.CODE.LIST = R.REDO.H.REORDER.LEVEL<RE.ORD.CODE>
    Y.REORDER.LEVEL = R.REDO.H.REORDER.LEVEL<RE.ORD.REORDER.LEVEL>
    Y.REQ.VAL = R.REDO.H.REORDER.LEVEL<RE.ORD.QTY.TO.REQ>
    Y.CHANGE.VAL = Y.CODE.LIST
    CHANGE @VM TO '' IN Y.CHANGE.VAL
    Y.COUNT = DCOUNT(Y.ITEM.LIST.STOCK,@VM)
    Y.CNT =1
    LOOP
    WHILE Y.CNT LE Y.COUNT
        Y.BAL = R.REDO.ITEM.STOCK<ITEM.REG.BAL><1,Y.CNT>
        Y.ORD.STATUS  = R.REDO.ITEM.STOCK<ITEM.REG.ORDER.STATUS><1,Y.CNT>
        Y.ITEM = Y.ITEM.LIST.STOCK<1,Y.CNT>
        IF Y.CHANGE.VAL THEN
            GOSUB CODE.CHECK.VAL
        END ELSE
            LOCATE Y.ITEM IN Y.ITEM.LIST<1,1,1> SETTING POS THEN
                IF Y.BAL LE Y.REORDER.LEVEL<1,1,POS> AND NOT(Y.ORD.STATUS) THEN
                    DESCRIPTION = R.REDO.ITEM.STOCK<ITEM.REG.DESC><1,Y.CNT>
                    R.REC<RE.ORD.DESCRIPTION> = DESCRIPTION
                    R.REC<RE.ORD.REQUEST.QUANTITY> = Y.REQ.VAL<1,1,POS>
                    GOSUB OFS.PROCESS
                    GOSUB POST.OFS.MSG

                END
            END
        END
        Y.CNT += 1
    REPEAT
RETURN

*------------------------
CODE.CHECK.VAL:
*------------------------
    Y.COUNT.VM = DCOUNT(Y.CODE.LIST,@VM)
    Y.CNT.VM = 1
    LOOP
    WHILE Y.CNT.VM LE Y.COUNT.VM
        IF Y.CODE AND Y.CODE EQ Y.CODE.LIST<1,Y.CNT.VM> THEN
            LOCATE Y.ITEM IN Y.ITEM.LIST<1,Y.CNT.VM,1> SETTING POS THEN
                IF Y.BAL LE Y.REORDER.LEVEL<1,Y.CNT.VM,POS> AND NOT(Y.ORD.STATUS) THEN
                    DESCRIPTION = R.REDO.ITEM.STOCK<ITEM.REG.DESC><1,Y.CNT>
                    R.REC<RE.ORD.DESCRIPTION> = DESCRIPTION
                    R.REC<RE.ORD.REQUEST.QUANTITY> = Y.REQ.VAL<1,Y.CNT.VM,POS>
                    GOSUB OFS.PROCESS
                    GOSUB POST.OFS.MSG

                END
            END
        END
        Y.CNT.VM += 1
    REPEAT
RETURN
*------------
OFS.PROCESS:
*------------
*If SERIES.BAL of stock register is lesser than or equal to REORDER.LEVEL of REDO.CARD.REORDER.DEST than raise REDO.CARD.REQUEST

* Y.RECORD<REDO.CARD.REQ.AGENCY> = ID.COMPANY

    IF Y.CODE THEN
        CALL F.READ(FN.REDO.H.MAIN.COMPANY,Y.SR.SEL.LIST.ID,R.REDO.H.MAIN.COMPANY,F.REDO.H.MAIN.COMPANY,Y.ERR)
        Y.DES = R.REDO.H.MAIN.COMPANY<REDO.COM.DESCRIPTION>
        LOCATE Y.CODE IN R.REDO.H.MAIN.COMPANY<REDO.COM.CODE,1> SETTING POS.1.CODE THEN
            R.REC<RE.ORD.BRANCH.CODE> = Y.CODE
            R.REC<RE.ORD.BRANCH.DES>  = R.REDO.H.MAIN.COMPANY<REDO.COM.DESCRIPTION,POS.1.CODE>
        END
    END

RETURN
*------------------------------------------------------------------------------
POST.OFS.MSG:
*-------------
*Processing ofs for REDO.CARD.REQUEST


    APP.NAME='REDO.H.ORDER.DETAILS'
    OFSFUNCTION='I'
    PROCESS='PROCESS'
    OFS.SOURCE.ID='UPDATE.PASSBOOK'
    OFSVERSION='REDO.H.ORDER.DETAILS,ITEM.REQUEST'
    GTSMODE=''
    NO.OF.AUTH='0'
    TRANSACTION.ID= ''
    OFSRECORD = ''

    CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCTION,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,R.REC,OFSRECORD)

    OFS.REQ.MSG = OFSRECORD
    Y.MSG.KEY = ""
    Y.OPTIONS = ""

    TOTAL.COMM.CNTR1 = DCOUNT(OFS.REQ.MSG,",")

    MODIFY.PART1 = FIELD(OFS.REQ.MSG,",",3,1)
    TOT.MODIFY.CNT1 = DCOUNT(MODIFY.PART1,"/")

    MODIFY.CO.CODE1 = FIELD(MODIFY.PART1,"/",3,1)
    MODIFY.CO.CODE1 = Y.SR.SEL.LIST.ID
    MODIFY.CO.CODE1 = MODIFY.CO.CODE1:"/"

    FIRST.MOD.PART1 = FIELD(MODIFY.PART1,"/",1,2)
    FIRST.MOD.PART1 = FIRST.MOD.PART1:"/"

    REST.MOD.PART1 =  FIELD(MODIFY.PART1,"/",4,TOT.MODIFY.CNT1)
    NEW.MODIFY.PART1 = FIRST.MOD.PART1:MODIFY.CO.CODE1:REST.MOD.PART1
    NEW.MODIFY.PART1 = NEW.MODIFY.PART1:","

    FIRST.PART1 = FIELD(OFS.REQ.MSG,",",1,2)
    FIRST.PART1 = FIRST.PART1:","

    REST.PART1 = FIELD(OFS.REQ.MSG,",",4,TOTAL.COMM.CNTR1)

    Y.FINAL.MSG1 = FIRST.PART1:NEW.MODIFY.PART1:REST.PART1


    CALL OFS.POST.MESSAGE(Y.FINAL.MSG1,Y.MSG.KEY,OFS.SOURCE.ID,Y.OPTIONS)

RETURN

END
