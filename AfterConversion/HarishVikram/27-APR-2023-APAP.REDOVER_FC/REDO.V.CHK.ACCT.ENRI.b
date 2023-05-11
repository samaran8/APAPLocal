* @ValidationCode : MjoxNTIxNDQxMTU1OkNwMTI1MjoxNjgyNDEyMzQ0MzQ1OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:44
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.CHK.ACCT.ENRI

*----------------------------------------------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : VIGNESH KUMAAR M R
* Program Name : REDO.V.INP.VERIFY.STAFF
*----------------------------------------------------------------------------------------------------------------------
* This routine is used to modify the enrichment for the liquidation account per the requirement
*----------------------------------------------------------------------------------------------------------------------
* Linked with : VERSION.CONTROL of APAP.H.GARNISH.DETAILS TABLE
* In parameter : None
* out parameter : None
*----------------------------------------------------------------------------------------------------------------------
* MODIFICATION HISTORY
*----------------------------------------------------------------------------------------------------------------------
* DATE NAME Refernce DESCRIPTION
* 28-03-2013 Vignesh Kumaar R PACS00255146 Modify the AZ liq acct enrichment with the AZ Deposit no
*----------------------------------------------------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*10-04-2023       Conversion Tool        R22 Auto Code conversion          VM TO @VM, ++ TO +=1
*10-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
*-----------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_GTS.COMMON
    $INSERT I_F.APAP.H.GARNISH.DETAILS

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    Y.APP = 'ACCOUNT'
    LREF.FLDS = 'L.AC.AZ.ACC.REF'
    LREF.POS = ''
    CALL GET.LOC.REF(Y.APP,LREF.FLDS,LREF.POS)

    IF V$FUNCTION NE 'I' AND V$FUNCTION NE 'A' THEN

        FN.GARNISH.DETAILS = 'F.APAP.H.GARNISH.DETAILS'
        F.GARNISH.DETAILS = ''
        CALL OPF(FN.GARNISH.DETAILS,F.GARNISH.DETAILS)

        CALL F.READ(FN.GARNISH.DETAILS,COMI,R.GARNISH.DETAILS,F.GARNISH.DETAILS,GARNISH.DETAILS.ERR)
        IF NOT(R.GARNISH.DETAILS) THEN
            FN.GARNISH.DETAILS.NAU = 'F.APAP.H.GARNISH.DETAILS$NAU'
            F.GARNISH.DETAILS.NAU = ''
            CALL OPF(FN.GARNISH.DETAILS.NAU,F.GARNISH.DETAILS.NAU)
            CALL F.READ(FN.GARNISH.DETAILS.NAU,COMI,R.GARNISH.DETAILS,F.GARNISH.DETAILS.NAU,GARNISH.DETAILS.ERR)
        END

        Y.ACCT.LIST = R.GARNISH.DETAILS<APAP.GAR.ACCOUNT.NO>
    END ELSE
        Y.ACCT.LIST = R.NEW(APAP.GAR.ACCOUNT.NO)
    END
    Y.ACCT.CNT = 1
    Y.ACCT.SIZE = DCOUNT(Y.ACCT.LIST,@VM)
    LOOP
        Y.ACCT.NO = Y.ACCT.LIST<1,Y.ACCT.CNT>

        CALL F.READ(FN.ACCOUNT,Y.ACCT.NO,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)
        Y.GET.AZ.DEP.ACCT = R.ACCOUNT<AC.LOCAL.REF><1,LREF.POS>

        IF Y.GET.AZ.DEP.ACCT THEN
            OFS$ENRI<APAP.GAR.ACCOUNT.NO,Y.ACCT.CNT> = "CTA. INTS. REINVERTIDOS DEPOSITO ":Y.GET.AZ.DEP.ACCT
        END
    WHILE Y.ACCT.CNT LE Y.ACCT.SIZE
        Y.ACCT.CNT += 1
    REPEAT

RETURN
