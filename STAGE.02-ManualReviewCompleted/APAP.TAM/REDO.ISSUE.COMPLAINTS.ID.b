* @ValidationCode : Mjo5NDE4MzMxODY6Q3AxMjUyOjE2ODEyOTUyMTY1NTQ6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 15:56:56
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.ISSUE.COMPLAINTS.ID
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : This routine is used to check the ID value for the table REDO.ISSUE.REQUESTS
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY :PRABHU N
* PROGRAM NAME : REDO.ISSUE.COMPLAINTS.ID
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                REFERENCE         DESCRIPTION
*01.03.2010      PRABHU             CRM-HD1100464     INITIAL CREATION
*10.03.2010      PRABHU             CRM-HD1100441     Common varible added
*16.05.2011      PRADEEP S          PACS00060849      PGM.VERSION set to current variable to access from
*                                                     drop down enquiry
* 12.04.2023    Conversion Tool       R22             Auto Conversion     - No changes
* 12.04.2023    Shanmugapriya M       R22             Manual Conversion   - No changes
*
* ----------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_REDO.SET.REQ.COMP.ID
    $INSERT I_F.REDO.ISSUE.COMPLAINTS
    $INSERT I_F.LOCKING
    $INSERT I_System

    GOSUB PROCESS
RETURN
*--------------------------------------------------------------------------------------------------------
PROCESS:
*--------------------------------------------------------------------------------------------------------

    FN.FRONT.COMPLAINTS='F.REDO.FRONT.COMPLAINTS'
    F.FRONT.COMPLAINTS=''
    CALL OPF(FN.FRONT.COMPLAINTS,F.FRONT.COMPLAINTS)
    Y.ID.NEW=CRM.CUR.TXN.ID

    IF Y.ID.NEW THEN
        ID.NEW=Y.ID.NEW
        Y.FRONT.ID=FIELD(Y.ID.NEW,'.',3)
        CALL F.DELETE(FN.FRONT.COMPLAINTS,Y.FRONT.ID)
        CRM.CUR.TXN.ID=''
    END

    Y.PGM.VERSION = PGM.VERSION ; Y.APPLICATION = APPLICATION
    CALL System.setVariable('CURRENT.PGM.VER',Y.PGM.VERSION)
    CALL System.setVariable('CURRENT.APPLICATION',Y.APPLICATION)

RETURN
*------------------------------------------------------------------------------------------------------------
END
