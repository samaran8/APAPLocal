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
