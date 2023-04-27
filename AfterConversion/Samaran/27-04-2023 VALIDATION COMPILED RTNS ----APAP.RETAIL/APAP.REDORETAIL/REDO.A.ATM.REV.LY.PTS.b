* @ValidationCode : MjotMjQ0MTM0ODY1OkNwMTI1MjoxNjgxMjc2NTUzNTgzOklUU1M6LTE6LTE6MTkzOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 10:45:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 193
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
**************************************************************************************************
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*10-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           NO CHANGES
*10-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*****************************************************************************************************************************************************
SUBROUTINE REDO.A.ATM.REV.LY.PTS

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AT.ISO.COMMON
    $INSERT I_F.ATM.REVERSAL
    $INSERT I_F.FUNDS.TRANSFER


    GOSUB OPEN.FILES


    Y.OFS.UTIL.NAME= 'REDO.LY.POINTS.US,REVERSE.ONLINE.DC'
    Y.OFS.FUNCTION='I'
    Y.OFS.OPERATION='PROCESS'
    Y.REV.GEN.ID=R.ATM.REVERSAL<AT.REV.LY.PTS.US.REF>
    IF Y.REV.GEN.ID THEN
        Y.OFS.REV= Y.OFS.UTIL.NAME:"/":Y.OFS.FUNCTION:"/":Y.OFS.OPERATION:",":",":Y.REV.GEN.ID
        Y.OFS.SOURCE='DEBIT.CARD'
        Y.OFS.ID=''
        CALL OFS.POST.MESSAGE(Y.OFS.REV,Y.OFS.ID,Y.OFS.SOURCE,'')

    END


RETURN
************
OPEN.FILES:
************

    FN.ATM.REVERSAL='F.ATM.REVERSAL'
    F.ATM.REVERSAL=''
    CALL OPF(FN.ATM.REVERSAL,F.ATM.REVERSAL)


    Y.APP='FUNDS.TRANSFER'

    Y.FLDS="AT.UNIQUE.ID"

    Y.FLD.POS=''


    CALL GET.LOC.REF(Y.APP,Y.FLDS,Y.FLD.POS)
    Y.FLD.POS.VAL=R.NEW(FT.LOCAL.REF)<1,Y.FLD.POS>
    CALL F.READ(FN.ATM.REVERSAL,Y.FLD.POS.VAL,R.ATM.REVERSAL,F.ATM.REVERSAL,ERR)

    Y.REV.GEN.ID=''

RETURN



END
