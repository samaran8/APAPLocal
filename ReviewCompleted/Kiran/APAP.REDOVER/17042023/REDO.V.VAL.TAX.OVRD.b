* @ValidationCode : MjoxMjEyNDU3MTg4OkNwMTI1MjoxNjgxNzM0MzI5MzA1OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 17:55:29
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
SUBROUTINE REDO.V.VAL.TAX.OVRD
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: SHANKAR RAJU
* PROGRAM NAME: REDO.V.VAL.TAX.OVRD
* ODR NO      : ODR-2009-12-0285
*----------------------------------------------------------------------
*DESCRIPTION: This routine is an input routine attached to below versions,
* TELLER,CHQ.OTHERS
* TELLER,CHQ.TAX
* TELLER,CHQ.NO.TAX
* TELLER,MGR.CHQ.TAX
* TELLER,MGR.CHQ.NOTAX



*IN PARAMETER: NA
*OUT PARAMETER: NA
*LINKED WITH: TELLER
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION

*05.07.2010  SHANKAR RAJU  ODR-2009-12-0285  ADDED NEW CHECKS
*03-02-2011  KAVITHA       ODR-2009-12-0285  TO FIX HD1104937 . Logic Changed as confirmed by BC. Transaction amt will be entered in AMOUNT.LOCAL.1 core field
*                                              local field will hold total of  total of TAX, AMOUNT, CHARGE
*----------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                   VM TO @VM,START.LOOP+1 TO +=1
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*---------------------------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
    $INSERT I_F.TELLER.TRANSACTION
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.FT.CHARGE.TYPE
    $INSERT I_F.TAX
    $INSERT I_GTS.COMMON



*    IF (OFS.VAL.ONLY EQ 1) AND (MESSAGE EQ '') THEN
    GOSUB LOCAL.REF
    GOSUB PROCESS
*    END
RETURN
*----------------------------------------------------------------------
LOCAL.REF:
*----------------------------------------------------------------------

    LOC.REF.APPLICATION="TELLER"
    LOC.REF.FIELDS='WAIVE.TAX':@VM:'L.TT.WAI.CHARGE':@VM:'L.TT.BASE.AMT'
    LOC.REF.POS=''

    FN.TELLER.TRANSACTION = 'F.TELLER.TRANSACTION'
    F.TELLER.TRANSACTION = ''
    R.TELLER.TRANSACTION = ''
    CALL OPF(FN.TELLER.TRANSACTION,F.TELLER.TRANSACTION)

    FN.FT.COMMISSION.TYPE = 'F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE = ''
    R.FT.COMMISSION.TYPE = ''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)

    FN.FT.CHARGE.TYPE = 'F.FT.CHARGE.TYPE'
    F.FT.CHARGE.TYPE = ''
    R.FT.CHARGE.TYPE = ''
    CALL OPF(FN.FT.CHARGE.TYPE,F.FT.CHARGE.TYPE)

    FN.TAX = 'F.TAX'
    F.TAX = ''
    R.TAX = ''
    CALL OPF(FN.TAX,F.TAX)

    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LREF.POS)
    POS.WAIVE.TAX = LREF.POS<1,1>
    POS.WAIVE.CHARGE = LREF.POS<1,2>
    POS.BASE.AMT = LREF.POS<1,3>

    FN.REDO.ADMIN.CHQ.PARAM = 'F.REDO.ADMIN.CHQ.PARAM'
    F.REDO.ADMIN.CHQ.PARAM = ''
    R.REDO.ADMIN.CHQ.PARAM = ''
*CALL OPF(FN.REDO.ADMIN.CHQ.PARAM,F.REDO.ADMIN.CHQ.PARAM) ;*Tus S/E
    CALL CACHE.READ(FN.REDO.ADMIN.CHQ.PARAM,'SYSTEM',R.REDO.ADMIN.CHQ.PARAM,ERR.PAR)

RETURN

*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------

*TO CALCULATE THE CHARGE AMOUNT FOR THE BASE AMOUNT

    GOSUB CALCULATE.AMOUNT

    Y.WAIVE.CHARGE = R.NEW(TT.TE.LOCAL.REF)<1,POS.WAIVE.CHARGE>
*    Y.WAIVE.CHARGE = COMI
    Y.WAIVE.TAX = COMI
    Y.BASE.AMT = R.NEW(TT.TE.LOCAL.REF)<1,POS.BASE.AMT>

    Y.ACCOUNT.2 = R.NEW(TT.TE.ACCOUNT.2)

    Y.ALL.AC.NOS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT>
    Y.ALL.TAX.KEYS = R.REDO.ADMIN.CHQ.PARAM<ADMIN.CHQ.PARAM.TAX.KEY>
    LOCATE Y.ACCOUNT.2 IN Y.ALL.AC.NOS<1,1> SETTING POS1 THEN

        Y.TAX.KEY = Y.ALL.TAX.KEYS<1,POS1>

    END

    Y.ALL.CHARGE.CODES = R.NEW(TT.TE.CHARGE.CODE)
    Y.ALL.LOC.CHGS = R.NEW(TT.TE.CHRG.AMT.LOCAL)
    Y.ALL.FOR.CHGS = R.NEW(TT.TE.CHRG.AMT.FCCY)
*HD1104937 -S
    TEMP.AMT = LCCY:' ':'0.00'
*  IF (OFS.VAL.ONLY EQ 1) AND (MESSAGE EQ '') THEN
    IF Y.WAIVE.CHARGE EQ 'YES' AND Y.WAIVE.TAX NE 'YES' THEN

        GOSUB PROCESS.WAIVE.CHARGE

    END

    IF Y.WAIVE.CHARGE NE 'YES' AND Y.WAIVE.TAX EQ 'YES' THEN

        GOSUB PROCESS.WAIVE.TAX

    END

    IF Y.WAIVE.CHARGE EQ 'YES' AND Y.WAIVE.TAX EQ 'YES' THEN

        GOSUB PROCESS.WAIVE.ALL

    END
*HD1104937 -E
* END

* IF Y.WAIVE.CHARGE NE 'YES' AND Y.WAIVE.TAX NE 'YES' THEN
*       R.NEW(TT.TE.WAIVE.CHARGES) = 'NO'
*      R.NEW(TT.TE.CHRG.AMT.LOCAL) = Y.AMOUNTS
*  END

RETURN
*----------------------------------------------------------------------------------------------------
CALCULATE.AMOUNT:
*~~~~~~~~~~~~~~~~

    IF R.NEW(TT.TE.CURRENCY.1) EQ LCCY THEN
        Y.AMOUNTS = R.NEW(TT.TE.CHRG.AMT.LOCAL)
    END ELSE
        Y.AMOUNTS = R.NEW(TT.TE.CHRG.AMT.FCCY)
    END

    Y.BASE.AMOUNT = R.NEW(TT.TE.LOCAL.REF)<1,POS.BASE.AMT>

*  Y.TELLER.TRANSACTION = R.NEW(TT.TE.TRANSACTION.CODE)

*   CALL F.READ(FN.TELLER.TRANSACTION,Y.TELLER.TRANSACTION,R.TELLER.TRANSACTION,F.TELLER.TRANSACTION,ERR.TT)
*  Y.ALL.CHARGE.CODES = R.TELLER.TRANSACTION<TT.TR.CHARGE.CODE>

*    Y.NO.CHARGE.CODES = DCOUNT(Y.ALL.CHARGE.CODES,VM)

*   START.COUNT = 1
*  LOOP
* WHILE START.COUNT LE Y.NO.CHARGE.CODES

*    Y.CHARGE.CODE = Y.ALL.CHARGE.CODES<1,START.COUNT>

*   CALL F.READ(FN.FT.COMMISSION.TYPE,Y.CHARGE.CODE,R.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE,ERR.FTCT)

*  IF R.FT.COMMISSION.TYPE THEN
*     IF R.FT.COMMISSION.TYPE<FT4.FLAT.AMT> NE '' THEN
*       Y.AMOUNTS<-1> = R.FT.COMMISSION.TYPE<FT4.FLAT.AMT>
* END ELSE
*     Y.AMOUNTS<-1> = (R.FT.COMMISSION.TYPE<FT4.PERCENTAGE>*Y.BASE.AMOUNT)/100
*            END

*END ELSE
*   CALL F.READ(FN.FT.CHARGE.TYPE,Y.CHARGE.CODE,R.FT.CHARGE.TYPE,F.FT.CHARGE.TYPE,ERR.FTCT)
*  IF R.FT.CHARGE.TYPE THEN
*     Y.AMOUNTS<-1> = R.FT.CHARGE.TYPE<FT5.FLAT.AMT>
* END ELSE
*    Y.AMOUNTS<-1> = R.FT.CHARGE.TYPE<FT5.CHARGE.AMT>
* END
*END

*START.COUNT = START.COUNT + 1
*REPEAT
*Y.AMOUNTS = LOWER(Y.AMOUNTS)

RETURN
*----------------------------------------------------------------------------------------------------
PROCESS.WAIVE.ALL:
*~~~~~~~~~~~~~~~~~

*    CURR.NO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM) + 1
*    TEXT = 'WAIVE.CHARGE.TAX'
*    CALL STORE.OVERRIDE(CURR.NO)

    LOCATE Y.TAX.KEY IN Y.ALL.CHARGE.CODES<1,1> SETTING POS2 THEN

        IF Y.ALL.LOC.CHGS NE '' AND R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2> NE TEMP.AMT THEN

*            R.NEW(TT.TE.NARRATIVE.2) = R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2>
*            R.NEW(TT.TE.NARRATIVE.2) = Y.AMOUNTS<1,POS2>
            R.NEW(TT.TE.NARRATIVE.2) = R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2>
        END ELSE

            IF Y.ALL.FOR.CHGS NE '' AND R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2> NE TEMP.AMT THEN

*                R.NEW(TT.TE.NARRATIVE.2) = R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2>
*                R.NEW(TT.TE.NARRATIVE.2) = Y.AMOUNTS<1,POS2>
                R.NEW(TT.TE.NARRATIVE.2) = R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2>

            END

        END
    END

*    Y.BASE.AMT = Y.BASE.AMT - SUM(Y.AMOUNTS)
    Y.BASE.AMT = Y.BASE.AMT - R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2>

    R.NEW(TT.TE.LOCAL.REF)<1,POS.BASE.AMT> = DROUND(Y.BASE.AMT,2)
*    R.NEW(TT.TE.AMOUNT.LOCAL.2) = DROUND(Y.BASE.AMT,2)

    R.NEW(TT.TE.WAIVE.CHARGES) = 'YES'
    R.NEW(TT.TE.CHRG.AMT.LOCAL) = ''
    R.NEW(TT.TE.CHRG.AMT.FCCY) = ''
    R.NEW(TT.TE.CHARGE.CODE) = ''
    R.NEW(TT.TE.CHARGE.CUSTOMER) = ''
    R.NEW(TT.TE.CHARGE.ACCOUNT) = ''
    R.NEW(TT.TE.CHARGE.CATEGORY) = ''

RETURN
*----------------------------------------------------------------------------------------------------
PROCESS.WAIVE.CHARGE:
*~~~~~~~~~~~~~~~~~~~~

    R.NEW(TT.TE.WAIVE.CHARGES) = 'NO'
    LOCATE Y.TAX.KEY IN Y.ALL.CHARGE.CODES<1,1> SETTING POS3 THEN

        TOT.CHARGES = DCOUNT(Y.ALL.CHARGE.CODES,@VM)

        START.LOOP = 1

        GOSUB LOOP.WAIVE.CHARGE
    END

    R.NEW(TT.TE.LOCAL.REF)<1,POS.BASE.AMT> = DROUND(Y.BASE.AMT,2)
*    R.NEW(TT.TE.AMOUNT.LOCAL.2) = DROUND(Y.BASE.AMT,2)

*    CALL REFRESH.FIELD(TT.TE.AMOUNT.LOCAL.1,"")

RETURN
*----------------------------------------------------------------------------------------------------
LOOP.WAIVE.CHARGE:
*-----------------
    LOOP
    WHILE START.LOOP LE TOT.CHARGES

        IF START.LOOP NE POS3 THEN

            IF Y.ALL.LOC.CHGS NE '' AND R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2> NE TEMP.AMT THEN

                Y.BASE.AMT = Y.BASE.AMT - Y.AMOUNTS<1,START.LOOP>
                R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,START.LOOP> = LCCY:' ':'0.00'

            END ELSE

                IF Y.ALL.FOR.CHGS NE '' AND R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2> NE TEMP.AMT THEN
                    Y.BASE.AMT = Y.BASE.AMT - Y.AMOUNTS<1,START.LOOP>
                    R.NEW(TT.TE.CHRG.AMT.FCCY)<1,START.LOOP> = LCCY:' ':'0.00'
                END

            END

        END ELSE
            IF Y.ALL.LOC.CHGS NE '' THEN
                R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS3> = Y.AMOUNTS<1,POS3>
            END ELSE
                IF Y.ALL.FOR.CHGS NE '' THEN
                    R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS3> = Y.AMOUNTS<1,POS3>
                END
            END

        END

        START.LOOP += 1

    REPEAT
RETURN
*----------------------------------------------------------------------------------------------------
PROCESS.WAIVE.TAX:
*~~~~~~~~~~~~~~~~~

    R.NEW(TT.TE.WAIVE.CHARGES) = 'NO'

    LOCATE Y.TAX.KEY IN Y.ALL.CHARGE.CODES<1,1> SETTING POS2 THEN

        TOT.CHARGES = DCOUNT(Y.ALL.CHARGE.CODES,@VM)

        START.LOOP = 1

        GOSUB LOOP.WAIVE.TAX

    END

    R.NEW(TT.TE.LOCAL.REF)<1,POS.BASE.AMT> = DROUND(Y.BASE.AMT,2)
*    R.NEW(TT.TE.AMOUNT.LOCAL.2) = DROUND(Y.BASE.AMT,2)

RETURN
*---------------------------------------------------------------------------------------------------
LOOP.WAIVE.TAX:
*--------------
    LOOP
    WHILE START.LOOP LE TOT.CHARGES

        IF START.LOOP EQ POS2 THEN
            IF Y.ALL.LOC.CHGS NE '' AND R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2> NE TEMP.AMT THEN

*            R.NEW(TT.TE.NARRATIVE.2) = R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2>
                Y.BASE.AMT = Y.BASE.AMT - Y.AMOUNTS<1,START.LOOP>
                R.NEW(TT.TE.NARRATIVE.2) = Y.AMOUNTS<1,POS2>
                R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,POS2> = LCCY:' ':'0.00'

            END ELSE

                IF Y.ALL.FOR.CHGS NE '' AND R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2> NE TEMP.AMT THEN

*                R.NEW(TT.TE.NARRATIVE.2) = R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2>
                    Y.BASE.AMT = Y.BASE.AMT - Y.AMOUNTS<1,START.LOOP>
                    R.NEW(TT.TE.NARRATIVE.2) = Y.AMOUNTS<1,POS2>
                    R.NEW(TT.TE.CHRG.AMT.FCCY)<1,POS2> = LCCY:' ':'0.00'

                END

            END
        END ELSE

            IF Y.ALL.LOC.CHGS NE '' THEN

                R.NEW(TT.TE.CHRG.AMT.LOCAL)<1,START.LOOP> = Y.AMOUNTS<1,START.LOOP>

            END ELSE

                IF Y.ALL.FOR.CHGS NE '' THEN

                    R.NEW(TT.TE.CHRG.AMT.FCCY)<1,START.LOOP> = Y.AMOUNTS<1,START.LOOP>

                END

            END
        END

        START.LOOP += 1

    REPEAT
RETURN
*---------------------------------------------------------------------------------------------------
END
