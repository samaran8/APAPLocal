* @ValidationCode : MjotNTgzOTY4MTU3OkNwMTI1MjoxNjgxMTEyNjIyNzIwOjMzM3N1Oi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 13:13:42
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
*-----------------------------------------------------------------------------------
* Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*10/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION           VM TO @VM, SM TO @SM,++ TO +=
*10/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*-----------------------------------------------------------------------------------
SUBROUTINE REDO.GET.MARGIN.ID(Y.CAMP.TYPE,Y.AFF.CAMP,R.REDO.RATE.CHANGE,Y.MARGIN.ID)
*-----------------------------------------------------------------------------
* Description: This routine is to get the respective Margin ID for the Loan based on
*              Campaign type & Affiliated Company.
*-----------------------------------------------------------------------------
* Input Arg: Y.CAMP.TYPE - Campaign Type.
*            Y.AFF.CAMP  - Affiliated Company.
*            R.REDO.RATE.CHANGE - Param Table Details
* Output Arg: Y.MARGIN.ID - Margin Table ID.
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE

*-----------------------------------------------------------------------------
* Here the main process begins.
*-----------------------------------------------------------------------------

    Y.MARGIN.ID       = ''
    Y.TABLE.CAMP.TYPE = R.REDO.RATE.CHANGE<1>
    Y.TABLE.AFF.COMP  = R.REDO.RATE.CHANGE<2>
    Y.TABLE.MARGIN.ID = R.REDO.RATE.CHANGE<3>
    Y.TABLE.DEFAULT   = R.REDO.RATE.CHANGE<4>

    BEGIN CASE
        CASE Y.CAMP.TYPE EQ '' AND Y.AFF.CAMP EQ ''
            GOSUB CHECK.NULL
        CASE Y.CAMP.TYPE NE '' AND Y.AFF.CAMP EQ ''
            GOSUB CHECK.CAMP.TYPE     ;* Only Campaign type is for the Loan
            IF Y.MARGIN.ID ELSE
                GOSUB CHECK.NULL
            END
        CASE Y.CAMP.TYPE EQ '' AND Y.AFF.CAMP NE ''
            GOSUB CHECK.AFF.CAMP      ;* Only Affiliated Company is for the Loan
            IF Y.MARGIN.ID ELSE
                GOSUB CHECK.NULL
            END
        CASE Y.CAMP.TYPE NE '' AND Y.AFF.CAMP NE ''
            GOSUB CHECK.BOTH          ;* If Both Campaign type and Affiliated Company exist for the Loan
            IF Y.MARGIN.ID ELSE
                GOSUB CHECK.NULL
            END
    END CASE

    IF Y.MARGIN.ID ELSE
        Y.MARGIN.ID = Y.TABLE.DEFAULT
    END

RETURN
*----------------------------------------------------------------------
CHECK.NULL:
*----------------------------------------------------------------------
* If Both Campaign type & Affiliated Company not exist for the Loan

    IF Y.TABLE.CAMP.TYPE EQ '' AND Y.TABLE.AFF.COMP EQ '' THEN          ;* Table Doesnt have both aff comp & Camp type.
        IF Y.TABLE.MARGIN.ID THEN
            Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,1,1>
            RETURN
        END
    END

    IF Y.TABLE.CAMP.TYPE EQ '' AND Y.TABLE.AFF.COMP NE '' THEN
        LOCATE '' IN Y.TABLE.AFF.COMP<1,1,1> SETTING POS1 THEN
            Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,1,POS1>
            RETURN
        END
    END

    Y.LOOP.CNT = DCOUNT(Y.TABLE.CAMP.TYPE,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.LOOP.CNT

        IF Y.TABLE.CAMP.TYPE<1,Y.VAR1> EQ '' THEN
            IF Y.TABLE.AFF.COMP<1,Y.VAR1> EQ '' THEN
                Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,Y.VAR1,1>
                RETURN
            END
            Y.VAR2 = 1
            Y.AFF.COMP.CNT = DCOUNT(Y.TABLE.AFF.COMP<1,Y.VAR1>,@SM)
            LOOP
            WHILE Y.VAR2 LE Y.AFF.COMP.CNT
                IF Y.TABLE.AFF.COMP<1,Y.VAR1,Y.VAR2> EQ '' THEN
                    Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,Y.VAR1,Y.VAR2>
                    RETURN
                END
                Y.VAR2 += 1 ;*AUTO R22 CODE CONVERSION
            REPEAT
        END
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

RETURN
*----------------------------------------------------------------------
CHECK.BOTH:
*----------------------------------------------------------------------
* If Both Campaign type and Affiliated Company exist for the Loan

    Y.LOOP.CNT = DCOUNT(Y.TABLE.CAMP.TYPE,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.LOOP.CNT
        IF Y.CAMP.TYPE EQ Y.TABLE.CAMP.TYPE<1,Y.VAR1> THEN
            LOCATE Y.AFF.CAMP IN Y.TABLE.AFF.COMP<1,Y.VAR1,1> SETTING POS1 THEN
                Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,Y.VAR1,POS1>
            END
        END
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

    IF Y.MARGIN.ID ELSE
        GOSUB CHECK.CAMP.TYPE
    END

    IF Y.MARGIN.ID ELSE
        GOSUB CHECK.AFF.CAMP
    END

RETURN
*----------------------------------------------------------------------
CHECK.CAMP.TYPE:
*----------------------------------------------------------------------
* Only Campaign type is for the Loan.

    Y.LOOP.CNT = DCOUNT(Y.TABLE.CAMP.TYPE,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.LOOP.CNT
        IF Y.CAMP.TYPE EQ Y.TABLE.CAMP.TYPE<1,Y.VAR1> THEN

            IF Y.TABLE.AFF.COMP<1,Y.VAR1> EQ '' THEN
                Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,Y.VAR1,1>
                RETURN
            END

            Y.VAR2 = 1
            Y.AFF.COMP.CNT = DCOUNT(Y.TABLE.AFF.COMP<1,Y.VAR1>,@SM)
            LOOP
            WHILE Y.VAR2 LE Y.AFF.COMP.CNT

                IF Y.TABLE.AFF.COMP<1,Y.VAR1,Y.VAR2> EQ '' THEN
                    Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,Y.VAR1,Y.VAR2>
                    RETURN
                END
                Y.VAR2 += 1 ;*AUTO R22 CODE CONVERSION
            REPEAT
        END
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

RETURN
*----------------------------------------------------------------------
CHECK.AFF.CAMP:
*----------------------------------------------------------------------

    IF Y.TABLE.CAMP.TYPE EQ '' THEN
        LOCATE Y.AFF.CAMP IN Y.TABLE.AFF.COMP<1,1,1> SETTING POS1 THEN
            Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,1,POS1>
            RETURN
        END

    END

    Y.LOOP.CNT = DCOUNT(Y.TABLE.CAMP.TYPE,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.LOOP.CNT
        IF Y.TABLE.CAMP.TYPE<1,Y.VAR1> EQ '' THEN

            LOCATE Y.AFF.CAMP IN Y.TABLE.AFF.COMP<1,Y.VAR1,1> SETTING POS1 THEN
                Y.MARGIN.ID = Y.TABLE.MARGIN.ID<1,Y.VAR1,POS1>
                RETURN
            END
        END
        Y.VAR1 += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT

RETURN
END
