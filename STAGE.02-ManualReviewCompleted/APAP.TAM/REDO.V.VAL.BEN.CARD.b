* @ValidationCode : MjoxNzMwNjM2NDI4OkNwMTI1MjoxNjgyNTE4ODgzNjQ0OklUU1M6LTE6LTE6MTgzOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 19:51:23
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 183
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.V.VAL.BEN.CARD
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.VAL.ACCOUNT.SEARCH
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is attached as authorization routine in all the version used
*                  in the development N.83.It will fetch the value from sunnel interface
*                  and assigns it it R.NEW
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 16-APR-2010        Prabhu.N       ODR-2009-10-0536   Initial Creation
* 03-DEC-2010        Prabhu.N       ODR-2010-11-0211    Modified based on Sunnel
*25-APR-2023    CONVERSION TOOL     R22 AUTO CONVERSION     IF BLOCK ADDED
*25-APR-2023    VICTORIA S          R22 MANUAL CONVERSION   CALL method format changed
*-------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_System
    IF VAL.TEXT NE '' THEN
        RETURN
    END
    GOSUB INIT
RETURN

*---------
INIT:
*---------
    FN.CUS.BEN.LIST='F.CUS.BEN.LIST'
    F.CUS.BEN.LIST=''
    CALL OPF(FN.CUS.BEN.LIST,F.CUS.BEN.LIST)
    LREF.APP=APPLICATION
    LREF.POS=''
    LREF.FIELDS='L.FT.CR.CARD.NO'
    CUSTOMER.ID=System.getVariable('EXT.SMS.CUSTOMERS')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 AUTO CONVERSION START
        CUSTOMER.ID = ""
    END ;*R22 AUTO CONVERSION END

    CUS.BEN.LIST.ID = CUSTOMER.ID:'-OWN'
    CALL F.READ(FN.CUS.BEN.LIST,CUS.BEN.LIST.ID,R.CUS.BEN.LIST,F.CUS.BEN.LIST,CUS.BEN.LIST.ER)
    LOCATE COMI IN R.CUS.BEN.LIST<1> SETTING Y.BEN.POS ELSE
        ETEXT = 'EB-INVALID.CARD'
        CALL STORE.END.ERROR
    END
    GOSUB PROCESS
    VAR.CARD.NO=R.NEW(FT.LOCAL.REF)<1,LREF.POS>
    VAR.CARD.NO=VAR.CARD.NO[1,6]:'******':VAR.CARD.NO[13,4]
    R.NEW(FT.LOCAL.REF)<1,LREF.POS>=VAR.CARD.NO

RETURN
*-------
PROCESS:
*-------
    CALL MULT.GET.LOC.REF(LREF.APP,LREF.FIELDS,LREF.POS)
    Y.ARRAY='BUSCAR_TARJETA_CUENTA.2'
    CALL APAP.REDOVER.REDO.V.WRAP.SUNNEL(Y.ARRAY);* R22 Manual conversion - CALL method format changed
RETURN
END
