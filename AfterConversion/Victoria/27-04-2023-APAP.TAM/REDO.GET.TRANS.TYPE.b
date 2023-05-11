* @ValidationCode : Mjo2ODIwMjE3NDM6Q3AxMjUyOjE2ODA3NjAzMzYwOTI6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Apr 2023 11:22:16
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
SUBROUTINE REDO.GET.TRANS.TYPE
*-----------------------------------------------------------------------------

* This is conversion routine attached with the enquiry REDO.DISB.E.AUTH.ARR to
* display the description of the transaction type.

* Reference : PACS00240923
* Developed  By : Marimuthus@temenos.com
* Date : 01-02-2013
*------------------------------------------------------------------------------

* Revision History:
*------------------
* Date          who                  Reference       Description
* 11/03/2013    Vignesh Kumaar M R   PACS00251027    Description field information updation
* 26/03/2013    Sivakumar K          PACS00255148    PAYOFF,TRESURY,CAPITAL PAYMENT partial versions added
* 06.04.2023    Conversion Tool       R22            Auto Conversion     - No changes
* 06.04.2023    Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.FC.FORM.DISB
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_ENQUIRY.COMMON

    FN.FT.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FT.NAU = ''
    CALL OPF(FN.FT.NAU,F.FT.NAU)

    FN.REDO.FC.FORM.DISB = 'F.REDO.FC.FORM.DISB'
    F.REDO.FC.FORM.DISB = ''
    CALL OPF(FN.REDO.FC.FORM.DISB,F.REDO.FC.FORM.DISB)

    Y.APPL = 'FUNDS.TRANSFER'
    Y.FIELDS = 'L.ACTUAL.VERSIO'
    Y.POS = ''
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELDS,Y.POS)


    Y.ID = O.DATA
    CALL F.READ(FN.FT.NAU,Y.ID,R.FT,F.FT.NAU,FT.ERR)
    Y.ACT.VER = R.FT<FT.LOCAL.REF,Y.POS>

    BEGIN CASE

        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.OTI' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.OTI'
            Y.FC = 'APERTURA.DEP'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.CHEQUE' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.CHEQUE'
            Y.FC = 'CHEQUE'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.ACDP' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.ACDP'
            Y.FC = 'CR.CTA'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.CASH' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.CASH'
            Y.FC = 'EFECTIVO'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.MULTI.AA.ACCRAP.DISB' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.MULTI.AA.ACCRAP.PDISB'
            Y.FC = 'PAGO.PR'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.LTCC' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.LTCC'
            Y.FC = 'PAGO.TARJ'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.IB.ACH' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.IB.ACH'
            Y.FC = 'TRANSFERENCIA'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.MULTI.AA.ACRP.DISB' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.MULTI.AA.PART.ACRP.DISB'
            Y.FC = 'PAGO.CAP.PREST'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.INTERBRANCH.ACH' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.AA.PART.INTERBRANCH.ACH'
            Y.FC = 'TRANSF.TESORERIA'
        CASE Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.MULTI.AA.ACPOAP.DISB' OR Y.ACT.VER EQ 'FUNDS.TRANSFER,REDO.MULTI.AA.PART.ACPOAP.DISB'
            Y.FC = 'CANC.PRESTAMO'

    END CASE

    IF Y.FC THEN
        CALL F.READ(FN.REDO.FC.FORM.DISB,Y.FC,R.REDO.FORM.DISB,F.REDO.FC.FORM.DISB,FC.ERR)
        O.DATA = R.REDO.FORM.DISB<FC.PR.DESCRIPCION>
    END ELSE
        O.DATA = ''
    END

RETURN

END
