* @ValidationCode : MjoxMjM3MDg5MTAzOkNwMTI1MjoxNjgxMjk1MjE1NzUyOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 15:56:55
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
SUBROUTINE REDO.H.TAX.DATA.CHECKS.VALIDATE
*---------------------------------------------------------------------------------------------
*
* Description           : Validation routine to validate the date entered in the record.

* Developed By          : Thilak Kumar Kumaresan
*
* Development Reference : RegN11
*
* Attached To           : N/A
*
* Attached As           : N/A
*---------------------------------------------------------------------------------------------
* Input Parameter:
*----------------*
* Argument#1 : N/A
*
*-----------------*
* Output Parameter:
*-----------------*
* Argument#4 : N/A
*
*---------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*---------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*
*
* Date             Who                   Reference      Description
* 12.04.2023       Conversion Tool       R22            Auto Conversion     - No changes
* 12.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*---------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.H.TAX.DATA.CHECKS
*
    GOSUB INITIALISATION
    GOSUB PROCESS
*
RETURN
*---------------------------------------------------------------------------------------------
*
INITIALISATION:
*--------------
    FN.REDO.H.TAX.DATA.CHECKS = 'F.REDO.H.TAX.DATA.CHECKS'
    F.REDO.H.TAX.DATA.CHECKS  = ''
*
    CALL OPF(FN.REDO.H.TAX.DATA.CHECKS,F.REDO.H.TAX.DATA.CHECKS)
*
RETURN
*---------------------------------------------------------------------------------------------
*
PROCESS:
*-------
    Y.DATE = TODAY
    Y.ID   = ID.NEW
    CALL CACHE.READ(FN.REDO.H.TAX.DATA.CHECKS,Y.ID,R.REDO.H.TAX.DATA.CHECKS,Y.ERR)
    Y.DATE.FROM = R.NEW(REDO.TAX.DATE.FROM)
    Y.DATE.TO   = R.NEW(REDO.TAX.DATE.TO)
*
    IF Y.DATE.FROM THEN
        GOSUB CHECK.FROM.DATE
    END ELSE
        ETEXT='EB-FR.DATE'
        CALL STORE.END.ERROR
    END
*
    IF Y.DATE.TO THEN
        GOSUB CHECK.TO.DATE
    END ELSE
        ETEXT='EB-TO.DATE'
        CALL STORE.END.ERROR
    END
*
RETURN
*---------------------------------------------------------------------------------------------
*
CHECK.FROM.DATE:
*---------------
    IF Y.DATE.FROM GT Y.DATE THEN
        AF=REDO.TAX.DATE.FROM
        ETEXT='EB-FR.LS.TO.TODAY'
        CALL STORE.END.ERROR
    END
*
    IF Y.DATE.TO AND Y.DATE.FROM GT Y.DATE.TO THEN
        AF=REDO.TAX.DATE.FROM
        ETEXT= 'EB-FR.LS.TO.DATE'
        CALL STORE.END.ERROR
    END ELSE
        IF Y.DATE.TO EQ '' THEN
            AF = REDO.TAX.DATE.TO
            ETEXT = 'EB-TO.DATE'
            CALL STORE.END.ERROR
        END
    END
*
RETURN
*--------------------------------------------------------------------------------------------------
CHECK.TO.DATE:
*-------------
    IF Y.DATE.TO GT Y.DATE THEN
        AF=REDO.TAX.DATE.TO
        ETEXT='EB-FR.LS.TO.TODAY'
        CALL STORE.END.ERROR
    END
*
    IF Y.DATE.TO LT Y.DATE.FROM THEN
        AF=REDO.TAX.DATE.TO
        ETEXT = 'EB-FR.LS.TO.DATE'
        CALL STORE.END.ERROR
    END
*
RETURN
*---------------------------------------------------------------------------------------------
END
