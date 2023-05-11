* @ValidationCode : MjotNjI1NjY1Nzc3OkNwMTI1MjoxNjgxMjkwMjc3Nzk1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 14:34:37
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM and I++ to I=+1
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*---------------------------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE REDO.CARD.PRINT.LOST.UPDATE

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CARD.PRINT.LOST
    $INSERT I_F.REDO.CARD.GENERATION
    $INSERT I_F.REDO.CARD.REQUEST
    $INSERT I_F.REDO.CARD.NUMBERS
    $INSERT I_F.REDO.CARD.NO.LOCK
    $INSERT I_F.REDO.STOCK.REGISTER
    $INSERT I_F.REDO.CARD.SERIES.PARAM



    GOSUB OPEN.FILES

    GOSUB PROCES.LOST

    CALL F.RELEASE(FN.REDO.CARD.GENERATION,ID.NEW,F.REDO.CARD.GENERATION)
    CALL F.RELEASE(FN.REDO.CARD.REQUEST,ID.NEW,F.REDO.CARD.REQUEST)
    CALL F.RELEASE(FN.REDO.STOCK.REGISTER,STK.ID,F.REDO.STOCK.REGISTER)


RETURN

************
OPEN.FILES:
************
    ERR.FLAG=1

    FN.REDO.CARD.REQUEST='F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST=''
    CALL OPF(FN.REDO.CARD.REQUEST,F.REDO.CARD.REQUEST)

    FN.REDO.CARD.GENERATION='F.REDO.CARD.GENERATION'
    F.REDO.CARD.GENERATION=''
    CALL OPF(FN.REDO.CARD.GENERATION,F.REDO.CARD.GENERATION)

    FN.REDO.CARD.NUMBERS='F.REDO.CARD.NUMBERS'
    F.REDO.CARD.NUMBERS=''
    CALL OPF(FN.REDO.CARD.NUMBERS,F.REDO.CARD.NUMBERS)

    FN.REDO.CARD.NO.LOCK='F.REDO.CARD.NO.LOCK'
    F.REDO.CARD.NO.LOCK=''
    CALL OPF(FN.REDO.CARD.NO.LOCK,F.REDO.CARD.NO.LOCK)

    FN.REDO.CARD.SERIES.PARAM='F.REDO.CARD.SERIES.PARAM'

    FN.REDO.STOCK.REGISTER='F.REDO.STOCK.REGISTER'
    F.REDO.STOCK.REGISTER=''
    CALL OPF(FN.REDO.STOCK.REGISTER,F.REDO.STOCK.REGISTER)

    CALL F.READU(FN.REDO.CARD.REQUEST,ID.NEW,R.REDO.CARD.REQUEST,F.REDO.CARD.REQUEST,ERR,'P')
    AGENCY=R.REDO.CARD.REQUEST<REDO.CARD.REQ.AGENCY>
    STOCK.ENT.ID=R.REDO.CARD.REQUEST<REDO.CARD.REQ.PRINTING.SE.ID>
    Y.STATUS    =R.REDO.CARD.REQUEST<REDO.CARD.REQ.STATUS>
    IF Y.STATUS NE '4' THEN
        AF=REDO.PRN.LST.CRD.TYPE
        E='EB-INPUT.NOT.ALLOW'
        ERR.FLAG=0
    END
    CALL F.READU(FN.REDO.CARD.GENERATION,ID.NEW,R.REDO.CARD.GENERATION,F.REDO.CARD.GENERATION,ERR.GEN,'P')

    CALL CACHE.READ(FN.REDO.CARD.SERIES.PARAM,'SYSTEM',R.REDO.CARD.SERIES.PARAM,ERR)

    STK.ID='CARD':'.':ID.COMPANY:'-':R.REDO.CARD.SERIES.PARAM<REDO.CARD.SERIES.PARAM.EMBOSS.DEPT.CODE>

    CALL F.READU(FN.REDO.STOCK.REGISTER,STK.ID,R.STOCK.REGISTER,F.REDO.STOCK.REGISTER,ERR.STOCK,'P')

    Y.CARD.NUMBERS=''

RETURN

*************
PROCES.LOST:
*************

    GEN.CARD.TYPE = R.REDO.CARD.GENERATION<REDO.CARD.REQ.CARD.TYPE>

    CRD.TYPE = R.NEW(REDO.PRN.LST.CRD.TYPE)

    CNT.TYPE= DCOUNT(CRD.TYPE,@VM)


    FLAG.TYPE=0
    POS.TYPE=1
    FOR POS.TYPE=1 TO CNT.TYPE


        CRD.TYP=CRD.TYPE<1,POS.TYPE>

        GOSUB VALIDATE.TYPE

        IF FLAG.TYPE EQ 0 THEN
            CONTINUE
        END

        GOSUB VALIDATE.CARD
        IF NOT(ERR.FLAG)  THEN
            CONTINUE
        END
        GOSUB UPD.STOCK.REG       ;* Reduces count based on number of cards for the type
    NEXT
    IF NOT(ERR.FLAG)  THEN
        RETURN
    END
    CALL F.WRITE(FN.REDO.STOCK.REGISTER,STK.ID,R.STOCK.REGISTER)
    CALL F.WRITE(FN.REDO.CARD.REQUEST,ID.NEW,R.REDO.CARD.REQUEST)

RETURN

***************
VALIDATE.TYPE:
***************

    LOCATE CRD.TYP IN GEN.CARD.TYPE<1,1> SETTING POS.GEN THEN

        FLAG.TYPE=1

    END

RETURN

***************
VALIDATE.CARD:
***************

    CRD.NUMBERS.LOST=R.NEW(REDO.PRN.LST.CRD.NUMBER.LOST)<1,POS.TYPE>

    GEN.NUMBERS=R.REDO.CARD.GENERATION<REDO.CARD.GEN.CARD.NUMBERS,POS.GEN>

    LOST.CNT=DCOUNT(CRD.NUMBERS.LOST,@SM)

    POS.CNT=1

    REDO.NUMBERS.ID=CRD.TYP:'.':AGENCY

    CALL F.READU(FN.REDO.CARD.NUMBERS,REDO.NUMBERS.ID,R.REDO.CARD.NUMBERS,F.REDO.CARD.NUMBERS,ERR.NUMBRS,'P')

    CALL F.READU(FN.REDO.CARD.NO.LOCK,REDO.NUMBERS.ID,R.REDO.CARD.NO.LOCK,F.REDO.CARD.NO.LOCK,ERR.LOCK,'P')

    CRD.NUMBERS=R.REDO.CARD.NUMBERS<REDO.CARD.NUM.CARD.NUMBER>

    CRD.STATUS=R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS>


    LOOP

        REMOVE CRD.NUM FROM CRD.NUMBERS.LOST SETTING POS.CRD

    WHILE CRD.NUM NE ''


        LOCATE CRD.NUM IN GEN.NUMBERS<1,1,1> SETTING POS.GEN.NUM THEN

            LOCATE CRD.NUM IN CRD.NUMBERS<1,1> SETTING CRD.NUM.POS THEN

                BEGIN CASE

                    CASE CRD.STATUS<1,CRD.NUM.POS> EQ 'AVAILABLE' AND R.REDO.CARD.NUMBERS<REDO.CARD.NUM.EMBOSS.TYPE,CRD.NUM.POS> EQ 'PREEMBOZADA'

                        CRD.STATUS<1,CRD.NUM.POS>='LOST'
                        R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS>=CRD.STATUS
                        GOSUB UPD.CARD.LOCK

                    CASE CRD.STATUS<1,CRD.NUM.POS> EQ 'INUSE' AND R.REDO.CARD.NUMBERS<REDO.CARD.NUM.EMBOSS.TYPE,CRD.NUM.POS> EQ 'PERSONALIZADA'

                        CRD.STATUS<1,CRD.NUM.POS>='LOST'
                        R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS>=CRD.STATUS
                        GOSUB UPD.CARD.LOCK

                END CASE

            END ELSE
                CONTINUE
            END

        END
        POS.CNT += 1
        CRD.NUM=''
    REPEAT

    IF NOT(ERR.FLAG)  THEN
        RETURN
    END

    R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS>=CRD.STATUS

    CALL F.WRITE(FN.REDO.CARD.NUMBERS,REDO.NUMBERS.ID,R.REDO.CARD.NUMBERS)
    CALL F.WRITE(FN.REDO.CARD.NO.LOCK,REDO.NUMBERS.ID,R.REDO.CARD.NO.LOCK)

RETURN

***************
UPD.CARD.LOCK:
***************


    CRD.AVAIL=R.REDO.CARD.NO.LOCK<REDO.CARD.LOCK.CARD.NUMBER,1>



    IF CRD.AVAIL EQ '' THEN
        LOCATE "AVAILABLE" IN R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS,1> SETTING AVL.POS THEN
*           INS R.REDO.CARD.NUMBERS<REDO.CARD.NUM.CARD.NUMBER,AVL.POS> IN R.REDO.CARD.NO.LOCK<REDO.CARD.LOCK.CARD.NUMBER,1>

            R.REDO.CARD.NO.LOCK<REDO.CARD.LOCK.CARD.NUMBER,1>=R.REDO.CARD.NUMBERS<REDO.CARD.NUM.CARD.NUMBER,AVL.POS>
        END


        RETURN
    END

    LOCATE CRD.AVAIL IN R.REDO.CARD.NUMBERS<REDO.CARD.NUM.CARD.NUMBER,1> SETTING POS.CARD THEN

        STATUS.CRD=R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS,POS.CARD>
        IF STATUS.CRD NE 'AVAILABLE' THEN
            LOCATE "AVAILABLE" IN R.REDO.CARD.NUMBERS<REDO.CARD.NUM.STATUS,1> SETTING AVL.POS THEN
                INS R.REDO.CARD.NUMBERS<REDO.CARD.NUM.CARD.NUMBER,AVL.POS> BEFORE R.REDO.CARD.NO.LOCK<REDO.CARD.LOCK.CARD.NUMBER,1>
            END
        END

    END



RETURN

**************
UPD.STOCK.REG:
**************

*REDO.CARD.SERIES.PARAM.CARD.TYPE
*REDO.CARD.SERIES.PARAM.CARD.DESC
*REDO.CARD.SERIES.PARAM.CARD.SERIES

    CRD.TYPES=R.REDO.CARD.SERIES.PARAM<REDO.CARD.SERIES.PARAM.CARD.TYPE>
    CRD.SERIES=R.REDO.CARD.SERIES.PARAM<REDO.CARD.SERIES.PARAM.CARD.SERIES>

    LOCATE CRD.TYP IN CRD.TYPES<1,1> SETTING POS.CARD.TYPE THEN
        CARD.SERIES=CRD.SERIES<1,POS.CARD.TYPE>
    END ELSE

    END


    Y.SERIES.ID = CARD.SERIES

*    LOST.CNT

*STOCK.ENT.ID

    STOCK.SERIES.ID = R.STOCK.REGISTER<STK.REG.SERIES.ID>

    CHANGE @VM TO @FM IN STOCK.SERIES.ID

    LOCATE Y.SERIES.ID IN STOCK.SERIES.ID SETTING Y.SERIES.ID.POS THEN

        Y.NEW.SERIES.BAL = R.STOCK.REGISTER<STK.REG.SERIES.BAL,Y.SERIES.ID.POS> - LOST.CNT

        R.STOCK.REGISTER<STK.REG.SERIES.BAL,Y.SERIES.ID.POS> = Y.NEW.SERIES.BAL
        R.STOCK.REGISTER<STK.REG.STO.REG.BAL> =  R.STOCK.REGISTER<STK.REG.STO.REG.BAL> -LOST.CNT
        STK.QTY=0
        STK.QTY=R.REDO.CARD.REQUEST<REDO.CARD.REQ.REGOFF.ACCEPTQTY,POS.GEN>
        R.REDO.CARD.REQUEST<REDO.CARD.REQ.REGOFF.ACCEPTQTY,POS.GEN>=STK.QTY-LOST.CNT


    END

RETURN

END
