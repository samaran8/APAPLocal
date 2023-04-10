* @ValidationCode : Mjo4MjU1NDI4NDA6Q3AxMjUyOjE2ODA2ODY4NzY0Nzk6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 05 Apr 2023 14:57:56
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
SUBROUTINE REDO.BRR.INWRETURN.PROCESS.LOAD
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : NATCHIMUTHU
* Program Name  : REDO.BRR.INWRETURN.PROCESS.LOAD
* ODR           : ODR-2010-09-0148.
*-------------------------------------------------------------------------
* Description: This routine is a load routine used to load the variables
*
*---------------------------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*-------------------------------------------------------------------------
*   DATE              ODR              WHO                   DESCRIPTION
* 30-09-10          ODR-2010-09-0148   NATCHIMUTHU           Initial Creation
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_F.REDO.MAPPING.TABLE
    $INSERT I_BATCH.FILES
    $INSERT I_F.REDO.CLEARING.PROCESS
    $INSERT I_REDO.BRR.INWRETURN.PROCESS.COMMON

    GOSUB INIT
RETURN

*******
INIT:
*******


    FN.REDO.CLEARING.PROCESS = 'F.REDO.CLEARING.PROCESS'
    F.REDO.CLEARING.PROCESS  = ''
    CALL OPF(FN.REDO.CLEARING.PROCESS,F.REDO.CLEARING.PROCESS)

    FN.REDO.MAPPING.TABLE = 'F.REDO.MAPPING.TABLE'
    F.REDO.MAPPING.TABLE = ''
    CALL OPF(FN.REDO.MAPPING.TABLE,F.REDO.MAPPING.TABLE)

    FN.REDO.APAP.CLEARING.INWARD = 'F.REDO.APAP.CLEARING.INWARD'
    F.REDO.APAP.CLEARING.INWARD = ''
    CALL OPF(FN.REDO.APAP.CLEARING.INWARD,F.REDO.APAP.CLEARING.INWARD)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.REDO.REJECT.REASON =  'F.REDO.REJECT.REASON'
    F.REDO.REJECT.REASON  =  ''
    CALL OPF(FN.REDO.REJECT.REASON,F.REDO.REJECT.REASON)


    F.PATH = ''

    Y.FINAL.ARRAY = ''

    Y.FILE.PATH = ''

    Y.FILE.NAME = ''

RETURN
*-------------------------------------------------------------------------------------
END
*---------------------------------------------------------------------------------------
