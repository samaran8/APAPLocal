* @ValidationCode : MjotMTcxOTIzNzUwNDpDcDEyNTI6MTY4MTM3Mjk0MDQyOTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 13:32:20
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
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.CONV.CRM.CUS.VERSION
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------
* DESCRIPTION : A Conversion routine choose Customer Version based on the local field L.CU.TIPO.CL
*
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : Pradeep S
* PROGRAM NAME : REDO.E.CONV.CRM.CUS.VERSION
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE              WHO                REFERENCE         DESCRIPTION
* 11.May.2011       Pradeep S          PACS00060849      INITIAL CREATION
* 13-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 13-APR-2023      Harishvikram C   Manual R22 conversion      No changes
* ----------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.CUS.TYPE = O.DATA


    BEGIN CASE

        CASE Y.CUS.TYPE EQ 'PERSONA FISICA'
            O.DATA = 'CUSTOMER,REDO.CLIENTE.PF.MOD'
        CASE Y.CUS.TYPE EQ 'CLIENTE MENOR'
            O.DATA = 'CUSTOMER,REDO.CLIENTE.MENOR.MOD'
        CASE Y.CUS.TYPE EQ 'PERSONA JURIDICA'
            O.DATA = 'CUSTOMER,REDO.CLIENTE.PJ.MOD'
    END CASE

RETURN
END
