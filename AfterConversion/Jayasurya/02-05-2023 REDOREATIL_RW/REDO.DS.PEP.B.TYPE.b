* @ValidationCode : MjotNjA5MDI0OTU0OkNwMTI1MjoxNjgzMDE5NjQwNjQ1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 02 May 2023 14:57:20
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
*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.DS.PEP.B.TYPE(Y.TYPE)
*-----------------------------------------------------------------------------
* PACS00371128
**
*
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            CALL RTN METHOD ADDED, VM TO @VM , FM TO @FM
*-------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.T24.FUND.SERVICES
    $USING APAP.REDOEB

    GOSUB INIT
    GOSUB PROCESS
RETURN
*
PROCESS:
*=======
*
    IF Y.PEP.BEN EQ "SI" THEN
        Y.LOOOKUP.VAL = Y.PEP.TBEN
        GOSUB GET.EBL.DESC
    END
*
    Y.TYPE = Y.DESC.VAL
RETURN
*
GET.EBL.DESC:
*============
*
    Y.DESC.VAL  = ''
*CALL APAP.REDOEB.REDO.EB.LOOKUP.LIST(Y.LOOKUP.ID,Y.LOOOKUP.VAL,Y.DESC.VAL,RES1,RES2) ;* MANUAL R22 CODE CONVERSION
    CALL APAP.REDOEB.redoEbLookupList(Y.LOOKUP.ID,Y.LOOOKUP.VAL,Y.DESC.VAL,RES1,RES2) ;* MANUAL R22 CODE CONVERSION
RETURN
*
INIT:
*====
*
    LOC.REF.FIELD = 'L.PEP.BEN' :@VM: 'L.TYPE.PEP.BEN' :@FM: 'L.PEP.BEN' :@VM: 'L.TYPE.PEP.BEN':@FM: 'L.PEP.BEN' :@VM: 'L.TYPE.PEP.BEN'
    LOC.REF.APP = 'TELLER':@FM:'FUNDS.TRANSFER':@FM:'T24.FUND.SERVICES'
    LOC.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FIELD,LOC.POS)
    POS.PEP.BEN      = LOC.POS<1,1>
    POS.PEP.TYPE.BEN = LOC.POS<1,2>
    POS.FT.BEN       = LOC.POS<2,1>
    POS.FT.TYPE.BEN  = LOC.POS<2,2>
    POS.TFS.BEN      = LOC.POS<3,1>
    POS.TFS.TYPE.BEN = LOC.POS<3,2>
*
    Y.TXN.PREF = ID.NEW[1,2]
    IF Y.TXN.PREF EQ "TT" THEN
        Y.PEP.BEN   = R.NEW(TT.TE.LOCAL.REF)<1,POS.PEP.BEN>
        Y.PEP.TBEN  = R.NEW(TT.TE.LOCAL.REF)<1,POS.PEP.TYPE.BEN>
    END
*
    IF Y.TXN.PREF EQ "FT" THEN
        Y.PEP.BEN   = R.NEW(FT.LOCAL.REF)<1,POS.FT.BEN>
        Y.PEP.TBEN  = R.NEW(FT.LOCAL.REF)<1,POS.FT.TYPE.BEN>
    END
*
    IF Y.TXN.PREF EQ "T2" THEN
        Y.PEP.BEN= R.NEW(TFS.LOCAL.REF)<1,POS.TFS.BEN>
        Y.PEP.TBEN  = R.NEW(TFS.LOCAL.REF)<1,POS.TFS.TYPE.BEN>
    END
    Y.DESC.VAL  = ''
    Y.LOOKUP.ID = "L.TYPE.PEP.INT"
RETURN

END
