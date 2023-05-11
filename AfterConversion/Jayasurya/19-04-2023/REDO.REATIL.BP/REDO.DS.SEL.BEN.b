* @ValidationCode : MjotMTQyMzUwNDExMDpDcDEyNTI6MTY4MTg3OTQ5NTY1MTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:14:55
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
SUBROUTINE REDO.DS.SEL.BEN(BEN.DST3)
*--------------------------------------------------------------------------------
*Company Name :Asociacion Popular de Ahorros y Prestamos
*Developed By :BTORRESALBORNOZ
*Program Name :REDO.DS.SEL.BEN
*---------------------------------------------------------------------------------
*DESCRIPTION :This program is used to get the SELL DESTINATION value from EB.LOOKUP TABLE
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.USER
    GOSUB PROCESS
RETURN
*********
PROCESS:
*********
    LOC.REF.FIELD = 'L.TT.BENEFICIAR'
    LOC.REF.APP = 'TELLER'
    LOC.POS = ''
    CALL GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.POS)
    BEN.DST = R.NEW(TT.TE.LOCAL.REF)<1,LOC.POS>
    BEN.DST1=BEN.DST<1,1,1>
    BEN.DST2=BEN.DST1[1,29]
    BEN.DST3=FMT(BEN.DST2,'R,#29')
RETURN
END
