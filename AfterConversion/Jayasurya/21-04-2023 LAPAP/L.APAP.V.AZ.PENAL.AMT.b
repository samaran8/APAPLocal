* @ValidationCode : Mjo5OTc1Nzg4MDM6Q3AxMjUyOjE2ODIwNjk0NDY4MDU6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:00:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*---------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.V.AZ.PENAL.AMT
*-----------------------------------------------------------------------------
*
* Description : The routine is to get the AZ local field L.AZ.PENAL.AMT value before user modification.
* Developed By: Ashokkumar
*

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_F.AZ.ACCOUNT

    IF V$FUNCTION NE 'S' THEN
        GOSUB INIT
        GOSUB PROCESS
    END
RETURN

INIT:
*****
    L.AZ.PENAL.AMT.POSN = ''
    CALL GET.LOC.REF('AZ.ACCOUNT','L.AZ.PENAL.AMT',L.AZ.PENAL.AMT.POSN)
RETURN

PROCESS:
********

    Y.PENAL.AMT = R.NEW(AZ.LOCAL.REF)<1,L.AZ.PENAL.AMT.POSN>
    CALL System.setVariable('CURRENT.PENAL.AMOUNT',Y.PENAL.AMT)
RETURN
END
