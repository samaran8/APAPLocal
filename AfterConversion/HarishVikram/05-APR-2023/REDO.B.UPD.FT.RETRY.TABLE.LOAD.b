* @ValidationCode : MjotNDE5MzQ5OTA3OkNwMTI1MjoxNjgwNjgyMzMyMjI1OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 13:42:12
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
SUBROUTINE REDO.B.UPD.FT.RETRY.TABLE.LOAD
*-------------------------------------------------------------------------------------------------
*DESCRIPTION:
* This routine is the load routine of the batch job REDO.B.UPD.FT.RETRY.TABLE
*  which updates the local table REDO.STO.PENDING.RESUBMISSION and REDO.RESUBMIT.FT.DET
* This routine Opens the necessary files
* ------------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who              Reference                   Description
* 03-JUN-2010   N.Satheesh Kumar    TAM-ODR-2009-10-0331          Initial Creation
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.UPD.FT.RETRY.TABLE.COMMON

    GOSUB OPEN.FILES
RETURN

*----------
OPEN.FILES:
*----------
*---------------------------------------
* This section opens the necessary files
*---------------------------------------

    FN.REDO.RESUBMIT.FT.DET = 'F.REDO.RESUBMIT.FT.DET'
    F.REDO.RESUBMIT.FT.DET = ''
    CALL OPF(FN.REDO.RESUBMIT.FT.DET,F.REDO.RESUBMIT.FT.DET)

    FN.REDO.STO.PENDING.RESUBMISSION = 'F.REDO.STO.PENDING.RESUBMISSION'
    F.REDO.STO.PENDING.RESUBMISSION = ''
    CALL OPF(FN.REDO.STO.PENDING.RESUBMISSION,F.REDO.STO.PENDING.RESUBMISSION)

    FN.OFS.RESPONSE.QUEUE = 'F.OFS.RESPONSE.QUEUE'
    F.OFS.RESPONSE.QUEUE = ''
    CALL OPF(FN.OFS.RESPONSE.QUEUE,F.OFS.RESPONSE.QUEUE)

RETURN
END
