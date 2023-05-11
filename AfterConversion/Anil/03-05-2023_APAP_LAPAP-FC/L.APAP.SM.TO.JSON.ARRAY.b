* @ValidationCode : MjotNzMxMjAyMTgwOkNwMTI1MjoxNjgzMDk0NTc1NTIzOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 03 May 2023 11:46:15
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
$PACKAGE APAP.LAPAP
SUBROUTINE  L.APAP.SM.TO.JSON.ARRAY(SM.IN.FIELDS, JSON.ARRAY.OUT)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE
* 21-APRIL-2023      Conversion Tool       R22 Auto Conversion - = to EQ , > to GT , Include to Insert and T24.BP is removed from Insert
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE

*Subroutine Convert a @VM with @SM to Json Array of Array
*----------------------------------------------------------------------------------------------------------------------------------------------------
*DEBUG
    Y.VM.FIELD = DCOUNT(SM.IN.FIELDS,@VM)
    Y.SM.FIELD = ''
    Y.I = 0
    Y.J = 0
    RTN.SM = ''
    JSON.ARRAY.OUT  = ''

    IF Y.VM.FIELD GT 0 THEN
        FOR Y.I = 1 TO Y.VM.FIELD
            IF Y.I EQ 1 THEN
                GOSUB CONVERTSM
                JSON.ARRAY.OUT =  RTN.SM
            END
            ELSE
                GOSUB CONVERTSM
                JSON.ARRAY.OUT := ',' : RTN.SM
            END
        NEXT Y.I
    END
    JSON.ARRAY.OUT  =  '[' : JSON.ARRAY.OUT : ']'

RETURN

CONVERTSM:
*DEBUG
    Y.SM.FIELD = DCOUNT(SM.IN.FIELDS<1,Y.I>,@SM)
    Y.J = 0
    RTN.SM = ''
    IF Y.SM.FIELD GT 0 THEN
        FOR Y.J = 1 TO Y.SM.FIELD
            IF Y.J EQ 1 THEN
                RTN.SM  = QUOTE(SM.IN.FIELDS<1,Y.I,Y.J>)
            END
            ELSE
                RTN.SM := ',' :  QUOTE(SM.IN.FIELDS<1,Y.I,Y.J>)
            END
        NEXT Y.J
    END
    RTN.SM  = '[' : RTN.SM : ']'

RETURN

RETURN
END
