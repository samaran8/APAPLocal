* @ValidationCode : MjotMTQxMzAyNTc2MTpDcDEyNTI6MTY4MjA3MTEwNDkyNjpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:28:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE LAPAP.V.INP.FT.CHECK.ACC.RT
*-----------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*21-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM,INSERT FILE MODIFIED
*21-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*------------------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON    ;*R22 AUTO CODE CONVERSION.START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.USER
    $INSERT I_F.FUNDS.TRANSFER     ;*R22 AUTO CODE CONVERSION.END

    GOSUB INIT
    GOSUB FILEOPEN
    GOSUB PROCESS
RETURN
*----
INIT:
*----
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    LREF.APP = 'ACCOUNT'
    LREF.FIELD = 'L.AC.STATUS1'
    LREF.FIELD2 = 'L.AC.STATUS2'

RETURN
*--------
FILEOPEN:
*--------
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL GET.LOC.REF(LREF.APP,LREF.FIELD,LREF.POS)
    CALL GET.LOC.REF(LREF.APP,LREF.FIELD2,LREF.POS2)

RETURN
*--------
PROCESS:
*--------
    Y.CR.DEB.ID=R.NEW(FT.CREDIT.ACCT.NO)
    GOSUB OVERRIDE.GEN

    Y.CR.DEB.ID=R.NEW(FT.DEBIT.ACCT.NO)
    GOSUB OVERRIDE.GEN

RETURN

*------------
OVERRIDE.GEN:
*------------
    CALL F.READ(FN.ACCOUNT,Y.CR.DEB.ID,R.ACCOUNT,F.ACCOUNT,CRE.ERR)
    Y.STATUS=R.ACCOUNT<AC.LOCAL.REF,LREF.POS>
    Y.STATUS2=R.ACCOUNT<AC.LOCAL.REF,LREF.POS2>


    IF Y.STATUS2 EQ 'DECEASED' AND R.ACCOUNT<AC.CUSTOMER> NE '' THEN
        ETEXT = "CUENTA PERTENECE A FALLECIDO"
        CALL STORE.END.ERROR

        RETURN
    END

    IF Y.STATUS NE 'ACTIVE' AND Y.STATUS NE '3YINACTIVE' AND Y.STATUS NE '6MINACTIVE' AND Y.STATUS NE '' AND R.ACCOUNT<AC.CUSTOMER> NE '' THEN
        VIRTUAL.TAB.ID='L.AC.STATUS1'
        CALL EB.LOOKUP.LIST(VIRTUAL.TAB.ID)
        Y.LOOKUP.LIST=VIRTUAL.TAB.ID<2>
        Y.LOOKUP.DESC=VIRTUAL.TAB.ID<11>
        CHANGE '_' TO @FM IN Y.LOOKUP.LIST
        CHANGE '_' TO @FM IN Y.LOOKUP.DESC
        LOCATE Y.STATUS IN Y.LOOKUP.LIST SETTING POS1 THEN
            IF R.USER<EB.USE.LANGUAGE> EQ 1 THEN
                Y.MESSAGE=Y.LOOKUP.DESC<POS1,1>
            END
            IF R.USER<EB.USE.LANGUAGE> EQ 2 AND Y.LOOKUP.DESC<POS1,2> NE '' THEN
                Y.MESSAGE=Y.LOOKUP.DESC<POS1,2>
            END ELSE
                Y.MESSAGE=Y.LOOKUP.DESC<POS1,1>
            END
        END
        TEXT="REDO.AC.CHECK.ACTIVE":@FM:Y.CR.DEB.ID:@VM:Y.MESSAGE
        OVERRIDE.FIELD.VALUE = R.NEW(AC.OVERRIDE)
        CURR.NO = DCOUNT(OVERRIDE.FIELD.VALUE,'VM') + 1
        CALL STORE.OVERRIDE(CURR.NO)
    END
RETURN

END
