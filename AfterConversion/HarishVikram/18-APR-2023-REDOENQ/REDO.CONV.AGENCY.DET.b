* @ValidationCode : Mjo0NjYwNDE3MTU6Q3AxMjUyOjE2ODE4MDE2NDUxMjU6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Apr 2023 12:37:25
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
SUBROUTINE REDO.CONV.AGENCY.DET
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.CONV.AGENCY.DET
*--------------------------------------------------------------------------------------------------------
*Description       : REDO.APAP.CLASSIFICATION is a conversion routine attached to the ENQUIRY>
*                    REDO.APAP.NOF.LINK.RPT, the routine fetches the value from O.DATA delimited
*                    with stars and formats them according to the selection criteria and returns the value
*                     back to O.DATA
*Linked With       :
*In  Parameter     : N/A
*Out Parameter     : N/A
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*--------------------------------------------------------------------------------------------------------
*=====================
*      Date                 Who                  Reference                 Description
*     ------               -----               -------------              -------------
* 20 OCT 2010              Dhamu S             ODR-2010-03-0098            Initial Creation
*
* 18-APR-2023    	 Conversion tool       R22 Auto conversion       No changes
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*-------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
    GOSUB PROCESS.PARA

RETURN
*------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************

    IF NOT(O.DATA) THEN
        O.DATA = "ALL"
    END

RETURN
END
*------------------------------------------------------------------------------------------------------
