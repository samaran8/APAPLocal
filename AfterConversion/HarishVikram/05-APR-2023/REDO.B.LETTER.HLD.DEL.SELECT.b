* @ValidationCode : MjoxNzM0ODI4NTU1OkNwMTI1MjoxNjgwNjgwMDEyNTI0OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 13:03:32
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
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.LETTER.HLD.DEL.SELECT
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.B.LETTER.HLD.DEL.SELECT
* ODR NO      : ODR-2009-10-0838
*----------------------------------------------------------------------
*DESCRIPTION: This is the Select Routine  to delete the record that is in
*status HOLD



*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH: REDO.LETTER.ISSUE
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE          	      WHO           REFERENCE         DESCRIPTION
*18.03.2010    	    H GANESH       ODR-2009-10-0838   INITIAL CREATION
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.LETTER.ISSUE
    $INSERT I_REDO.B.LETTER.HLD.DEL.COMMON


    GOSUB OPENFILES
    GOSUB PROCESS
RETURN



*----------------------------------------------------------------------
OPENFILES:
*----------------------------------------------------------------------

    CALL OPF(FN.REDO.LETTER.ISSUE,F.REDO.LETTER.ISSUE)
RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------
    SEL.CMD='SELECT ':FN.REDO.LETTER.ISSUE:' WITH RECORD.STATUS EQ IHLD'
    LIST.PARAMETER ="F.SEL.INT.LIST"
    LIST.PARAMETER<1> = ''
    LIST.PARAMETER<3> = SEL.CMD
    CALL BATCH.BUILD.LIST(LIST.PARAMETER,'')
RETURN
END
