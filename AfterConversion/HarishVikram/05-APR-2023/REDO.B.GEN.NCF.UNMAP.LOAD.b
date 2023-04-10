* @ValidationCode : Mjo0NTgzMzU4NjQ6Q3AxMjUyOjE2ODA2Nzk2MTk0NTg6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 05 Apr 2023 12:56:59
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
SUBROUTINE REDO.B.GEN.NCF.UNMAP.LOAD
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
*This Routine will load and perform CALL OPF for all the application used in record
*routine
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who           Reference            Description
* 25-MAR-2010        Prabhu.N       ODR-2009-10-0321     Initial Creation
* 04-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*--------------------------------------------------------------------------------
    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.STMT.ENTRY
    $INSERT I_REDO.B.GEN.NCF.UNMAP.COMMON

    FN.REDO.NCF.ISSUED = "F.REDO.NCF.ISSUED"
    F.REDO.NCF.ISSUED  = ""
    CALL OPF(FN.REDO.NCF.ISSUED,F.REDO.NCF.ISSUED)

    FN.STMT.ENTRY='F.STMT.ENTRY'
    F.STMT.ENTRY=''
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)

    FN.CATEG.ENTRY='F.CATEG.ENTRY'
    F.CATEG.ENTRY=''
    CALL OPF(FN.CATEG.ENTRY,F.CATEG.ENTRY)

    FN.NCF.UNMAPPED='F.REDO.L.NCF.UNMAPPED'
    F.NCF.UNMAPPED=''
    CALL OPF(FN.NCF.UNMAPPED,F.NCF.UNMAPPED)

    FN.NCF.STATUS='F.REDO.L.NCF.STATUS'
    F.NCF.STATUS=''
    CALL OPF(FN.NCF.STATUS,F.NCF.STATUS)

    FN.TRANSACTION='F.TRANSACTION'
    F.TRANSACTION=''
    CALL OPF(FN.TRANSACTION,F.TRANSACTION)
    FN.TRANSACTION.CHARGE='F.TRANSACTION.CHARGE'
    F.TRANSACTION.CHARGE=''
    CALL OPF(FN.TRANSACTION.CHARGE,F.TRANSACTION.CHARGE)

    FN.TAX='F.TAX'
    F.TAX=''
    CALL OPF(FN.TAX,F.TAX)
RETURN
END
