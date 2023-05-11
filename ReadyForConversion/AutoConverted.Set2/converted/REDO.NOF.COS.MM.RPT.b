SUBROUTINE REDO.NOF.COS.MM.RPT(Y.DATE)
*-----------------------------------------------------------------------------
*  Company   Name    : Asociacion Popular de Ahorros y Prestamos
*  Developed By      : MANJU.G
*  ODR Number        : ODR-2010-08-0431
*  Program   Name    : REDO.NOF.COS.MM.RPT
*-----------------------------------------------------------------------------
* Incoming/Outgoing Parameters
*-------------------------------
* In  : --N/A--
* Out : Y.OUT.ARRAY
*-----------------------------------------------------------------------------
* DESCRIPTION       : This is a NOFILE enquiry routine to get a report that shows
*                     all insurance daily entries report
*------------------------------------------------------------------------------
* Modification History :
*-----------------------
*  DATE            WHO             REFERENCE            DESCRIPTION
*  -----           ----            ----------           -----------
*  11-Nov-2010     MANJU.G       ODR-2010-03-0140     INITIAL CREATION
*-------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    GOSUB INIT
RETURN

INIT:
*----------
    Y.DATE = ''
    Y.CURRENCY = ''

    LOCATE "DATE" IN D.FIELDS<1> SETTING Y.POS THEN
        Y.SEL.DATE = D.RANGE.AND.VALUE<Y.POS>
    END

    LOCATE "CURRENCY" IN D.FIELDS<1> SETTING Y.POS THEN
        Y.CURRENCY  = D.RANGE.AND.VALUE<Y.POS>
    END

    Y.DATE = Y.SEL.DATE:'*':Y.CURRENCY:'*':"View"
RETURN
END
