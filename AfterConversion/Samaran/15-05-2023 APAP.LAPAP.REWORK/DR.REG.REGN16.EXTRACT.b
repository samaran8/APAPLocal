* @ValidationCode : MjotODIyMjI4MTM4OkNwMTI1MjoxNjg0MTQ4MzYzMDk5OnNhbWFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 15 May 2023 16:29:23
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
$PACKAGE APAP.LAPAP
SUBROUTINE DR.REG.REGN16.EXTRACT(REC.ID)
*----------------------------------------------------------------------------
* Company Name   : APAP
* Company Name   : APAP
* Developed By   : gangadhar@temenos.com
* Program Name   : DR.REG.REGN16.EXTRACT
* Date           : 3-May-2013
*----------------------------------------------------------------------------
* Description:
*------------
* This multi-thread job is meant for to extact the REDO.ISSUE.CLAIMS Details for each Customer.
*----------------------------------------------------------------------------
*
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 31-07-2014        Ashokkumar                PACS00366332- Initial revision
* 28-11-2017        Ashokkumar                CN006499 - Changed the parameter file to add more data.
* Date                  who                   Reference
* 21-04-2023         CONVERSTION TOOL      R22 AUTO CONVERSTION -$INSERT TAM.BP TO $INSERT AND $INSERT LAPAP.BP TO $INSERT
* 21-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES

*-----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_TSA.COMMON
    $INSERT I_F.CUSTOMER
*
    $INSERT I_F.REDO.ISSUE.CLAIMS
    $INSERT I_DR.REG.REGN16.EXTRACT.COMMON
*
    GOSUB PROCESS
*
RETURN
*----------------------------------------------------------------------------
PROCESS:
*------*
*
    CALL F.READ(FN.REDO.ISSUE.CLAIMS,REC.ID,R.REDO.ISSUE.CLAIMS,F.REDO.ISSUE.CLAIMS,REDO.ISSUE.CLAIMS.ERR)
    IF R.REDO.ISSUE.CLAIMS THEN
        YACC.ID = R.REDO.ISSUE.CLAIMS<ISS.CL.ACCOUNT.ID>
        R.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,YACC.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
        RCL$INT.TAX(2) = R.ACCOUNT

        RCL.MAP.FMT    = "MAP"
        RCL.ID         = "DR.REG.REGN16.EXT"
        RCL.BASE.APP   = FN.REDO.ISSUE.CLAIMS
        RCL.BASE.ID    = REC.ID
        RCL.BASE.R.REC = R.REDO.ISSUE.CLAIMS
        RETURN.MSG     = ""
        ERROR.MSG      = ""
        CALL RAD.CONDUIT.LINEAR.TRANSLATION(RCL.MAP.FMT, RCL.ID, RCL.BASE.APP, RCL.BASE.ID, RCL.BASE.R.REC, RETURN.MSG, ERROR.MSG)

        IF RETURN.MSG THEN
            CALL F.WRITE(FN.DR.REG.REGN16.WORKFILE, REC.ID, RETURN.MSG)
        END
    END
*
RETURN
*----------------------------------------------------------------------------
END
