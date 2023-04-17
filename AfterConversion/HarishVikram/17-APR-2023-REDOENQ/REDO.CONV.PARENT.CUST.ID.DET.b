* @ValidationCode : MjoxODE4ODYxNTQyOkNwMTI1MjoxNjgxNzE1NjAwNTE2OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Apr 2023 12:43:20
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
SUBROUTINE REDO.CONV.PARENT.CUST.ID.DET
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is attached as a conversion routine to the enquiry
* display the field description of EB.LOOKUP instead of the ID.
*-------------------------------------------------------------------------
* HISTORY:
*---------
*   Date               who           Reference            Description

* 07-03-2012         RIYAS      ODR-2012-03-0162     Initial Creation
*
* 13-APR-2023      Conversion tool    R22 Auto conversion       No changes
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.EB.SECURE.MESSAGE
    $INSERT I_F.TELLER
    $INSERT I_F.REDO.AUT.INP.VERSION.NAME
    $INSERT I_ENQUIRY.COMMON

    FN.EB.SECURE.MESSAGE = 'F.EB.SECURE.MESSAGE'
    F.EB.SECURE.MESSAGE  = ''
    CALL OPF(FN.EB.SECURE.MESSAGE,F.EB.SECURE.MESSAGE)

    FN.CUSTOMER  = 'F.CUSTOMER'
    F.CUSTOMER   = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    Y.TXN.DETAILS   = ''


    CALL F.READ(FN.EB.SECURE.MESSAGE,O.DATA,R.EB.SECURE.MESSAGE,F.EB.SECURE.MESSAGE,EB.SECURE.MESSAGE.ERR)
    Y.PARENT.MSG.ID = R.EB.SECURE.MESSAGE<EB.SM.PARENT.MESSAGE.ID>
    O.DATA = R.EB.SECURE.MESSAGE<EB.SM.FROM.CUSTOMER>
    IF Y.PARENT.MSG.ID THEN
        O.DATA = R.EB.SECURE.MESSAGE<EB.SM.TO.CUSTOMER>
        IF NOT(O.DATA) THEN
            O.DATA = R.EB.SECURE.MESSAGE<EB.SM.FROM.CUSTOMER>
        END
    END

RETURN
