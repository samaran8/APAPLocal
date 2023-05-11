* @ValidationCode : MjotMzEwMzU1NDI6Q3AxMjUyOjE2ODI0MTIzNTE5ODU6SGFyaXNodmlrcmFtQzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.PRINT.CHQS
*---------------------------------------------------------------------------------
* This is an routine to call the deal slip for printing
*----------------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHOROS Y PRESTAMOS
* Developed By  : SHANKAR RAJU
* ODR NUMBER    : ODR-2010-03-0447
*----------------------------------------------------------------------
*MODIFICATION DETAILS:
*---------------------
*   DATE           RESOURCE           REFERENCE             DESCRIPTION

* 09-03-2011     SHANKAR RAJU      ODR-2010-03-0447     Printing of Cheques
*Modification history
*Date                Who               Reference                  Description
*17-04-2023      conversion tool     R22 Auto code conversion     No changes
*17-04-2023      Mohanraj R          R22 Manual code conversion   No changes
*----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.USER
    $INSERT I_RC.COMMON
    $INSERT I_GTS.COMMON

    GOSUB INITIALISE

RETURN
*----------------------------------------------------------------------------------
INITIALISE:
*----------

    DEAL.SLIP.ID = 'CHQ.PRINT.ADMIN'
    OFS$DEAL.SLIP.PRINTING = 1
    CALL PRODUCE.DEAL.SLIP(DEAL.SLIP.ID)

RETURN
*----------------------------------------------------------------------------------
END
