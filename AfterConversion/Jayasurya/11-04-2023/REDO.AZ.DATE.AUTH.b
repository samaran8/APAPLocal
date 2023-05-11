* @ValidationCode : Mjo1MzE1MjcwNTY6Q3AxMjUyOjE2ODExOTc2MTcxMTQ6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Apr 2023 12:50:17
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
SUBROUTINE REDO.AZ.DATE.AUTH
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: MANJU.G
* PROGRAM NAME: REDO.AZ.DATE.CALC
* ODR NO      : ODR-2009-12-0285
*----------------------------------------------------------------------
*DESCRIPTION:Difference between VALUE.DATE date and opening date of the contract MATURITY.DATE the module AZ

*IN PARAMETER: NA
*OUT PARAMETER: NA
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*25.05.2010  Manju.G     ODR-2011-05-0118      INITIAL CREATION
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*10-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM
*10-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT

    GOSUB PROCESS
RETURN
PROCESS:
********
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    FN.ACCOUNT.ACT = 'F.ACCOUNT.ACT'
    F.ACCOUNT.ACT = ''
    CALL OPF(FN.ACCOUNT.ACT,F.ACCOUNT.ACT)

    Y.SELECT.ID = ID.NEW
    LREF.APPLN = 'ACCOUNT'
    LREF.FLDS = 'L.AC.AZ.STA.DAT':@VM:'L.AC.AZ.MAT.DAT'
    LREF.POS = ''
    CALL MULTI.GET.LOC.REF(LREF.APPLN,LREF.FLDS,LREF.POS)
    Y.START.DT = LREF.POS<1,1>
    Y.END.DT = LREF.POS<1,2>
    Y.VALUE.DATE = R.NEW(AZ.VALUE.DATE)
    Y.MATURE.DATE = R.NEW(AZ.MATURITY.DATE)
    Y.OLD.VALUE.DATE = R.OLD(AZ.VALUE.DATE)
    Y.OLD.MATURE.DATE = R.OLD(AZ.MATURITY.DATE)
    IF Y.VALUE.DATE NE Y.OLD.VALUE.DATE OR Y.MATURE.DATE NE Y.OLD.MATURE.DATE THEN
        CALL F.READ(FN.ACCOUNT,Y.SELECT.ID,R.ACCOUNT,F.ACCOUNT,ER.ACCOUNT)
        R.ACCOUNT<AC.LOCAL.REF,Y.START.DT> = Y.VALUE.DATE
        R.ACCOUNT<AC.LOCAL.REF,Y.END.DT> = Y.MATURE.DATE
        CALL F.LIVE.WRITE(FN.ACCOUNT,Y.SELECT.ID,R.ACCOUNT)
        Y.CURR.NO = R.ACCOUNT<AC.CURR.NO>
        Y.ACT.ID = Y.SELECT.ID:';':Y.CURR.NO
        R.ACCOUNT.ACT = TODAY
        WRITE R.ACCOUNT.ACT ON F.ACCOUNT.ACT,Y.ACT.ID
* CALL F.WRITE(FN.ACCOUNT.ACT,Y.ACT.ID,R.ACCOUNT.ACT)
    END
RETURN
END
