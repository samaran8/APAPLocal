* @ValidationCode : Mjo5NjIyNDg3NDg6Q3AxMjUyOjE2ODEyMTgxNDczNTA6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Apr 2023 18:32:27
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
SUBROUTINE REDO.CARD.GENERATION.PROCESS


*DESCRIPTIONS:
*-------------
* This is  input routine of REDO.CARD.GENERATION to update the REDO.CARD.REQUEST with CARD GENERATION REQUEST RAISED
* It contains the table definitions
*-----------------------------------------------------------------------------
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*

* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who                    Reference             Description
*  21 Apr 2011   Balagurunathan            PACS00052986          TO update REDO.CARD.REQUEST with value Yes in SENT.CRD.GEN.REQ
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-----------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CARD.REQUEST

    FN.REDO.CARD.REQUEST='F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST=''
    CALL OPF(FN.REDO.CARD.REQUEST,F.REDO.CARD.REQUEST)

    CALL F.READ(FN.REDO.CARD.REQUEST,ID.NEW,R.REDO.CARD.REQUEST,FN.REDO.CARD.REQUEST,ERR)

    R.REDO.CARD.REQUEST<REDO.CARD.REQ.SENT.CRD.GEN.REQ>='YES'

    CALL F.WRITE(FN.REDO.CARD.REQUEST,ID.NEW,R.REDO.CARD.REQUEST)

RETURN

END
