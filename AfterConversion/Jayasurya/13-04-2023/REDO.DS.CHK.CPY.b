* @ValidationCode : MjoxNjAwNDExOTMzOkNwMTI1MjoxNjgxMzc4MzI3NzA0OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 15:02:07
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.DS.CHK.CPY(Y.TRANS.ID)
**********************************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.APAP.H.DEAL.SLIP.QUEUE
*---------------------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : DHAMU S
* Program Name : REDO.DS.CHK.CPY
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------------------------------
* Description : This is a conversion routine which gets the value of Transaction number
**********************************************************************************************
*Linked With :
*In parameter :
*Out parameter :
********************************************************************************************


    GOSUB INIT
    GOSUB PROCESS

RETURN

****
INIT:
*****

    FN.REDO.APAP.H.DEAL.SLIP.QUEUE = 'F.REDO.APAP.H.DEAL.SLIP.QUEUE'
    F.REDO.APAP.H.DEAL.SLIP.QUEUE  = ''
    CALL OPF(FN.REDO.APAP.H.DEAL.SLIP.QUEUE,F.REDO.APAP.H.DEAL.SLIP.QUEUE)

RETURN


********
PROCESS:
*********

    CALL F.READ(FN.REDO.APAP.H.DEAL.SLIP.QUEUE,Y.TRANS.ID,R.REDO.APAP.H.DEAL.SLIP.QUEUE,F.REDO.APAP.H.DEAL.SLIP.QUEUE,QUEUE.ERR)

    Y.PRINT = R.REDO.APAP.H.DEAL.SLIP.QUEUE<REDO.DS.QUEUE.INIT.PRINT>


    IF Y.PRINT EQ 'YES' THEN
        Y.TRANS.ID = 'COPIA'
    END ELSE
        Y.TRANS.ID = ''
    END

RETURN
****************************************************************************************************
END
