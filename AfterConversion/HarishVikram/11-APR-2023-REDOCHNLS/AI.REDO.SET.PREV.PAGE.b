* @ValidationCode : MjoxNDc5MjU5ODc4OkNwMTI1MjoxNjgxMTk2OTU1ODUyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 12:39:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOCHNLS
SUBROUTINE AI.REDO.SET.PREV.PAGE
*-----------------------------------------------------------------------------
*Company   Name    : APAP
*Developed By      : Martin Macias
*Program   Name    : AI.REDO.SET.PREV.PAGE
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
* DATE              WHO                REFERENCE                 DESCRIPTION
*11-APR-2023     Conversion tool    R22 Auto conversion        IF Condition added
* 11-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System

    FN.REDO.EB.USER.PRINT.VAR='F.REDO.EB.USER.PRINT.VAR'
    F.REDO.EB.USER.PRINT.VAR=''
    CALL OPF(FN.REDO.EB.USER.PRINT.VAR,F.REDO.EB.USER.PRINT.VAR)

    HTML.PREV.PAGE = O.DATA
    Y.USR.VAR = System.getVariable("EXT.EXTERNAL.USER")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - START
        Y.USR.VAR = ""
    END					;*R22 Auto conversion - END
    Y.USR.VAR = Y.USR.VAR:"-":"CURRENT.PREV.PAGE"
*  WRITE HTML.PREV.PAGE TO F.REDO.EB.USER.PRINT.VAR,Y.USR.VAR ;*Tus Start
    CALL F.WRITE(FN.REDO.EB.USER.PRINT.VAR,Y.USR.VAR,HTML.PREV.PAGE)

RETURN
END
