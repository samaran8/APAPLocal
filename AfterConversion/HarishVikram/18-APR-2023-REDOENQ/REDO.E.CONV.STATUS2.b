* @ValidationCode : MjoyNTgwOTM3MjE6Q3AxMjUyOjE2ODE4MDM2NzA1Nzk6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 18 Apr 2023 13:11:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.CONV.STATUS2
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 18-APR-2023     Conversion tool    R22 Auto conversion      FM TO @FM
* 18-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.LOOKUP
    $INSERT I_ENQUIRY.COMMON

    Y.ID = O.DATA

    FN.EB.LOOKUP = 'F.EB.LOOKUP'
    F.EB.LOOKUP = ''
    CALL OPF(FN.EB.LOOKUP, F.EB.LOOKUP)

    IF Y.ID THEN

        Y.ID = CHANGE(Y.ID,';',@FM)

        LOOP
            REMOVE Y.GET.LOOKUP.ID FROM Y.ID SETTING Y.LOOKUP.POS
        WHILE Y.GET.LOOKUP.ID:Y.LOOKUP.POS

            Y.LOOKUP.ID = 'L.AC.STATUS2*':Y.GET.LOOKUP.ID
            CALL F.READ(FN.EB.LOOKUP, Y.LOOKUP.ID, R.LOOKUP, F.EB.LOOKUP, Y.READ.ERR)

            Y.LOOKUP.TRANS = R.LOOKUP<EB.LU.DESCRIPTION,LNGG>

            IF NOT(Y.LOOKUP.TRANS) THEN
                Y.LOOKUP.TRANS = R.LOOKUP<EB.LU.DESCRIPTION,1>
            END
            Y.RETURN.VAL<-1> = Y.LOOKUP.TRANS
        REPEAT

        Y.RETURN.VAL = CHANGE(Y.RETURN.VAL,@FM,';')
        O.DATA = Y.RETURN.VAL
    END

RETURN
END
