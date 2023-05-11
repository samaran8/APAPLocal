* @ValidationCode : Mjo1NjU3ODAxMzpDcDEyNTI6MTY4MTE5OTcwNzA4OTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 11 Apr 2023 13:25:07
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
$PACKAGE APAP.REDOCHNLS
SUBROUTINE REDO.IVR.RESP.TWS(Y.RESPONSE)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This is a routine to simplify the TWS response for loan payments through IVR.
*
* Input/Output:
*--------------
* IN : -NA-
* OUT : -NA-
*---------------
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who                   Reference               Description
*  10-JUL-2014   RMONDRAGON             ODR-2011-02-0099        INITIAL VERSION
*
* 11-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 11-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
* </region>
*-----------------------------------------------------------------------------

    IF LNGG EQ 1 THEN
        Y.MSG = 'Operation cannot be processed'
    END ELSE
        Y.MSG = 'Operacion no puede ser procesada'
    END

    FINDSTR '//-1' IN Y.RESPONSE SETTING Y.POS THEN
        Y.RESPONSE = '<requests><request>//-1/NO,':Y.MSG:'</request></requests>'
    END ELSE
        FINDSTR '//1' IN Y.RESPONSE SETTING Y.POS THEN
            Y.FT.ID = FIELD(Y.RESPONSE,'/',1)
            Y.FT.ID = FIELD(Y.FT.ID,'<request>',2)
            Y.RESPONSE = '<requests><request>':Y.FT.ID:'//1/</request></requests>'
        END ELSE
            Y.RESPONSE = '<requests><request>//-1/NO,':Y.MSG:'</request></requests>'
        END
    END

RETURN

END
