$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.CONV.DESC.PROD.TYPE
****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : SHANKAR RAJU
* Program Name  : REDO.CONV.CLASS.SELECTION
*-------------------------------------------------------------------------
* Description: This routine is a conversion routine to format Classification
*----------------------------------------------------------
* Linked with: All enquiries with Customer no as selection field
* In parameter : N/A
* out parameter : N/A
*------------------------------------------------------------------------
* MODIFICATION HISTORY
*--------------------------------------------
*   DATE        WHO             ODR                 DESCRIPTION
* 01-AUG-11   SHANKAR RAJU    PACS00094166      Routine to format
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion  - F.READ to CACHE.READ
* 06-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.PRODUCT

    GOSUB INIT
    GOSUB PROCESS

RETURN

*----
INIT:
*----
    FN.AA.PRODUCT = "F.AA.PRODUCT"
    F.AA.PRODUCT  = ""
    R.AA.PRODUCT  = ""
    ERR.AA.PROD   = ""

    CALL OPF(FN.AA.PRODUCT,F.AA.PRODUCT)

RETURN
*-------
PROCESS:
*-------
    Y.ID = O.DATA
    CALL CACHE.READ(FN.AA.PRODUCT, Y.ID, R.AA.PRODUCT, ERR.AA.PROD)    ;*R22 Auto Conversion  - F.READ to CACHE.READ

    Y.PRD.DSC = R.AA.PRODUCT<AA.PDT.DESCRIPTION,2>

    IF Y.PRD.DSC EQ "" THEN
        Y.PRD.DSC = R.AA.PRODUCT<AA.PDT.DESCRIPTION,1>
    END

    O.DATA = Y.PRD.DSC

RETURN
END
